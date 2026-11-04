library(shiny)
library(tidyverse)

# ---------- GAME DATA (edit these to tweak the game) ----------

heroes <- data.frame(
  name    = c("Knight", "Mage", "Rogue", "Cleric"),
  hp      = c(120, 80, 90, 110),
  attack  = c(22, 30, 25, 20),
  defense = c(10, 4, 6, 8),
  speed   = c(5, 8, 15, 7),               # speed = % chance to dodge
  special = c("Shield Bash", "Fireball", "Backstab", "Holy Smite"),
  blurb   = c("Tough and steady.", "Hits hard, fragile.",
              "Fast and slippery.", "Balanced and sturdy."),
  stringsAsFactors = FALSE
)

enemies <- data.frame(
  name    = c("Slime", "Goblin", "Skeleton", "Ember Dragon"),
  hp      = c(40, 60, 80, 150),
  attack  = c(10, 14, 18, 24),
  defense = c(2, 4, 6, 8),
  stringsAsFactors = FALSE
)

reward_text <- "You claim the Dragon's Hoard: 500 gold and the Ember Crown!"

# ---------- HELPER FUNCTIONS (plain R, no Shiny needed) ----------

# Damage = attack (times a multiplier) minus half the defense, with a little randomness
calc_damage <- function(attack, defense, mult = 1) {
  base <- attack * mult - defense * 0.5
  max(1, round(base * runif(1, 0.85, 1.15)))
}

hp_bar <- function(current, max_hp, colour) {
  pct <- max(0, round(100 * current / max_hp))
  div(class = "bar-outer",
      div(class = "bar-inner",
          style = paste0("width:", pct, "%; background:", colour, ";")),
      span(class = "bar-label", paste0(max(0, current), " / ", max_hp)))
}

# ---------- UI ----------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#1b1b2f; color:#eee; font-family: Georgia, serif; }
    .card { background:#2b2b4a; border-radius:10px; padding:15px; margin:8px;
            text-align:center; }
    .bar-outer { position:relative; background:#444; border-radius:6px;
                 height:22px; margin:4px 0 10px 0; }
    .bar-inner { height:100%; border-radius:6px; }
    .bar-label { position:absolute; top:0; left:0; right:0; text-align:center;
                 font-size:13px; line-height:22px; }
    .log { background:#111; border-radius:8px; padding:10px; height:200px;
           overflow-y:auto; font-family: monospace; font-size:13px; }
    .btn-move { margin:4px; }
    h1, h2, h3 { text-align:center; }
  "))),
  titlePanel("Hero Quest"),
  uiOutput("screen")
)

# ---------- SERVER ----------

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    screen = "select",   # "select", "battle", "victory", "defeat"
    hero = NULL, hp = 0, guard = FALSE, charges = 3,
    enemy_idx = 1, enemy_hp = 0,
    log = character()
  )
  
  add_log <- function(...) {
    rv$log <- c(paste0(...), rv$log)   # newest message goes on top
  }
  
  start_game <- function(i) {
    rv$hero <- heroes[i, ]
    rv$hp <- heroes$hp[i]
    rv$guard <- FALSE
    rv$charges <- 3
    rv$enemy_idx <- 1
    rv$enemy_hp <- enemies$hp[1]
    rv$log <- paste0("The quest begins! A wild ", enemies$name[1], " appears!")
    rv$screen <- "battle"
  }
  
  # --- Hero selection buttons (one observer per hero) ---
  lapply(seq_len(nrow(heroes)), function(i) {
    observeEvent(input[[paste0("pick_", i)]], start_game(i))
  })
  
  # --- One full round: player acts, then the enemy responds ---
  play_round <- function(action) {
    hero  <- rv$hero
    enemy <- enemies[rv$enemy_idx, ]
    rv$guard <- FALSE
    
    # Player's move
    if (action == "attack") {
      d <- calc_damage(hero$attack, enemy$defense)
      rv$enemy_hp <- rv$enemy_hp - d
      add_log(hero$name, " strikes ", enemy$name, " for ", d, " damage.")
    } else if (action == "special") {
      rv$charges <- rv$charges - 1
      d <- calc_damage(hero$attack, enemy$defense, mult = 2)
      rv$enemy_hp <- rv$enemy_hp - d
      add_log(hero$name, " uses ", hero$special, "! ", d, " damage!")
    } else if (action == "guard") {
      rv$guard <- TRUE
      add_log(hero$name, " braces for the next hit.")
    }
    
    # Did the enemy die?
    if (rv$enemy_hp <= 0) {
      add_log(enemy$name, " is defeated!")
      if (rv$enemy_idx == nrow(enemies)) {
        rv$screen <- "victory"
      } else {
        # Move to the next enemy, with a small recovery between fights
        heal <- round(0.4 * hero$hp)
        rv$hp <- min(hero$hp, rv$hp + heal)
        rv$charges <- min(3, rv$charges + 1)
        rv$enemy_idx <- rv$enemy_idx + 1
        rv$enemy_hp <- enemies$hp[rv$enemy_idx]
        add_log("You rest and recover ", heal, " HP. Next: ",
                enemies$name[rv$enemy_idx], "!")
      }
      return(invisible())
    }
    
    # Enemy's turn
    if (runif(1) < hero$speed / 100) {
      add_log(hero$name, " dodges ", enemy$name, "'s attack!")
    } else {
      d <- calc_damage(enemy$attack, hero$defense)
      if (rv$guard) d <- max(1, round(d / 2))
      rv$hp <- rv$hp - d
      add_log(enemy$name, " hits ", hero$name, " for ", d, " damage.")
    }
    
    if (rv$hp <= 0) rv$screen <- "defeat"
  }
  
  observeEvent(input$attack,  play_round("attack"))
  observeEvent(input$special, play_round("special"))
  observeEvent(input$guard,   play_round("guard"))
  observeEvent(input$restart, rv$screen <- "select")
  
  # --- Screens ---
  output$screen <- renderUI({
    
    if (rv$screen == "select") {
      cards <- lapply(seq_len(nrow(heroes)), function(i) {
        h <- heroes[i, ]
        column(3, div(class = "card",
                      h3(h$name), p(em(h$blurb)),
                      p("HP: ", h$hp), p("Attack: ", h$attack),
                      p("Defense: ", h$defense), p("Speed: ", h$speed),
                      p("Special: ", strong(h$special)),
                      actionButton(paste0("pick_", i), paste("Choose", h$name),
                                   class = "btn-primary")
        ))
      })
      return(tagList(h2("Choose your hero"), fluidRow(cards)))
    }
    
    if (rv$screen == "victory") {
      return(div(class = "card",
                 h2("Victory!"),
                 p("The ", strong(enemies$name[nrow(enemies)]), " has fallen."),
                 h3(reward_text),
                 p("The quest is complete. Thanks for playing!"),
                 actionButton("restart", "Play again", class = "btn-success")
      ))
    }
    
    if (rv$screen == "defeat") {
      return(div(class = "card",
                 h2("Defeated..."),
                 p("Your hero has fallen on the quest. Game over."),
                 actionButton("restart", "Try again", class = "btn-danger")
      ))
    }
    
    # Battle screen
    hero  <- rv$hero
    enemy <- enemies[rv$enemy_idx, ]
    fluidRow(
      column(6, div(class = "card",
                    h3(hero$name), hp_bar(rv$hp, hero$hp, "#4caf50"),
                    p("Special charges: ", rv$charges)
      )),
      column(6, div(class = "card",
                    h3(enemy$name),
                    hp_bar(rv$enemy_hp, enemy$hp, "#e53935"),
                    p("Enemy ", rv$enemy_idx, " of ", nrow(enemies))
      )),
      column(12, div(style = "text-align:center;",
                     actionButton("attack", "Attack", class = "btn-move btn-primary"),
                     actionButton("special", paste0(hero$special, " (x2)"),
                                  class = "btn-move btn-warning",
                                  disabled = if (rv$charges <= 0) "disabled" else NULL),
                     actionButton("guard", "Guard", class = "btn-move btn-info")
      )),
      column(12, h4("Battle log"),
             div(class = "log", lapply(rv$log, function(m) div(m))))
    )
  })
}

shinyApp(ui, server)