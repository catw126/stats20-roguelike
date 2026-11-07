library(shiny)
library(tidyverse)

# ---------- GAME DATA (edit these to tweak the game) ----------
hero_template <- list(
  knight = list(name = "Knight", hp = 120, attack = 22, defense = 10,
                speed = 5, special = "Shield Bash", level = 1, blurb = "Tough and steady."),
  mage   = list(name = "Mage", hp = 80, attack = 30, defense = 4,
                speed = 8, special = "Fireball", level = 1, blurb = "Smart and focused."),
  assassin = list(name = "Assassin", hp = 90, attack = 25, defense = 6,
                  speed = 15, special = "Backstab", level = 1, blurb = "Fast and slippery"),
  archer = list(name = "Archer", hp = 110, attack = 20, defense = 8,
                speed = 7, special = "Holy Smite", level = 1, blurb = "Balanced and sturdy")
)
 
enemies <- data.frame( #for all of the regular waves, could do like a random sampling of these
  name    = c("Slime", "Goblin", "Skeleton", "Ember Dragon"), #add a few more, can just generate
  hp      = c(40, 60, 80, 150),
  attack  = c(10, 14, 18, 24),
  defense = c(2, 4, 6, 8),
  speed = c(3, 4, 5, 12),
  stringsAsFactors = FALSE
) #TODO: implement a way to randomize the grunt waves per run, but keeping corresponding stats for the same enemy intact
#maybe create a function that creates a vector of enemy names by randomly sampling the set of names (excluding boss)

reward_text <- "You claim the Dragon's Hoard: 500 gold and the Ember Crown!"

# ---------- HELPER FUNCTIONS (plain R, no Shiny needed) ----------

# Damage = attack (times a multiplier) minus half the defense, with a little randomness
calc_damage <- function(attack, defense, mult = 1) {
  stopifnot(is.numeric(attack), is.numeric(defense), is.numeric(mult))
  a <- attack * mult #1 for normal, 1.5 for crit if we do that, 2 for special if we do that
  raw <- a * (100/(100 + defense)) * runif(1, 0.8, 1.2)
  return (max(1, round(raw)))
}

get_effective_atk <- function(character, is_hero=T){ 
  req(rv$hero)
  base <- character$attack
  if (is_hero){
    total <- base + atk_mod
  }else{
    total <- base + enemy_atk_mod
  }
  return(max(1, total))
}

get_effective_def <- function(character, is_hero=T){ #placeholder
  req(rv$hero)
  base <- character$defense
  if (is_hero){
    total <- base + def_mod
  }else{
    total <- base + enemy_def_mod
  }
  return(max(1, total))
}

get_effective_spd <- function(character, is_hero=T){ #placeholder
  req(rv$hero)
  base <- character$speed
  if (is_hero){
    total <- base + spd_mod
  }else{
    total <- base + enemy_spd_mod
  }
  return(max(1, total))
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
    log = character(),
    
    # --- DYNAMIC MODIFIERS & BUFFS (Temporary additions/multipliers) ---
    atk_mod = 0,
    def_mod = 0,
    spd_mod = 0,
    
    enemy_atk_mod = 0,
    enemy_def_mod = 0,
    enemy_spd_mod = 0,
    
    # --- STATUS FLAGS ---
    poisoned = FALSE,
    paralyzed = FALSE,
    sleep = FALSE, #can add more status effects if we feel like it but this is base list
    
    enemy_poisoned = FALSE,
    enemy_paralyzed = FALSE,
    enemy_sleep = FALSE
  )
  
  add_log <- function(...) {
    rv$log <- c(paste0(...), rv$log)   # newest message goes on top
  }
  
  load_hero <- function(key) {
    rv$hero <- hero_template[[key]]
    rv$hp   <- hero_template[[key]]$hp
  }
  
  start_game <- function(key) {
    load_hero(key)
    rv$guard <- FALSE
    rv$charges <- 3
    rv$enemy_idx <- 1
    rv$enemy_hp <- enemies$hp[1]
    rv$atk_mod <- 0; rv$def_mod <- 0; rv$spd_mod <- 0
    rv$enemy_atk_mod <- 0; rv$enemy_def_mod <- 0; rv$enemy_spd_mod <- 0
    rv$log <- paste0("The quest begins! A wild ", enemies$name[1], " appears!")
    rv$screen <- "battle"
  }
  
  player_turn <- function(action) {
    hero  <- rv$hero
    enemy <- enemies[rv$enemy_idx, ]
    hero_atk <- get_effective_atk(hero, T)
    hero_def <- get_effective_def(hero, T)
    hero_spd <- get_effective_spd(hero, T)
    
    enemy_atk <- get_effective_atk(enemy, F)
    enemy_def <- get_effective_def(enemy, F)
    enemy_spd <- get_effective_spd(enemy, F)
    
    hero_mult <- if (runif(1, 0, 1) >= 0.80) 1.5 else 1 #Crit chance, do we want to put that in log text?
    enemy_mult <- if (runif(1, 0, 1) >= 0.80) 1.5 else 1
    
    if (action == "attack") { #TODO: implement stat boost/lowering moves once those are finalized
      d <- calc_damage(hero_atk, enemy_def, hero_mult)
      rv$enemy_hp <- max(0, rv$enemy_hp - d)
      add_log(hero$name, " strikes ", enemy$name, " for ", d, " damage.")
    } else if (action == "special") { #Decide if we wanna keep this
      rv$charges <- rv$charges - 1
      d <- calc_damage(hero_atk, enemy_def, mult = 2)
      rv$enemy_hp <- max(0, rv$enemy_hp - d)
      add_log(hero$name, " uses ", hero$special, "! ", d, " damage!")
    } else if (action == "guard") {
      rv$guard <- TRUE
      add_log(hero$name, " braces for the next hit.")
    }
  }
  
  enemy_turn <- function() {
    hero  <- rv$hero
    enemy <- enemies[rv$enemy_idx, ]
    
    if (runif(1) < hero$speed / 100) {
      add_log(hero$name, " dodges ", enemy$name, "'s attack!")
    } else {
      d <- calc_damage(enemy_atk, hero_def, enemy_mult)
      if (rv$guard) d <- max(1, round(d / 2))
      rv$hp <- max(0, rv$hp - d)
      add_log(enemy$name, " hits ", hero$name, " for ", d, " damage.")
    }
    
    if (rv$hp <= 0) rv$screen <- "defeat"
  }
  
  end_wave <- function() {
    hero  <- rv$hero
    enemy <- enemies[rv$enemy_idx, ]
    add_log(enemy$name, " is defeated!")
    
    if (rv$enemy_idx == nrow(enemies)) {
      rv$screen <- "victory"
    } else {
      heal <- round(0.4 * hero$hp)
      rv$hp <- min(hero$hp, rv$hp + heal)
      rv$charges <- min(3, rv$charges + 1)
      rv$enemy_idx <- rv$enemy_idx + 1
      rv$enemy_hp <- enemies$hp[rv$enemy_idx]
      
      rv$atk_mod <- 0; rv$def_mod <- 0; rv$spd_mod <- 0
      rv$enemy_atk_mod <- 0; rv$enemy_def_mod <- 0; rv$enemy_spd_mod <- 0
      
      add_log("You rest and recover ", heal, " HP. Next: ",
              enemies$name[rv$enemy_idx], "!")
    }
  }
  
  play_round <- function(action) {
    rv$guard <- FALSE
    enemy_spd <- get_effective_spd(enemy, F) #not in scope, will have to fix leaving off 10/6
    hero_spd  <- get_effective_spd(hero, T)
    enemy_first <- enemy_spd > hero_spd   # ties go to the hero
    
    if (enemy_first) enemy_turn()
    if (rv$screen == "defeat") return(invisible())
    
    player_turn(action)
    if (rv$enemy_hp <= 0) {
      end_wave()
      return(invisible())
    }
    
    if (!enemy_first) enemy_turn()
  }
  
  observeEvent(input$start, start_game(input$hero_pick))
  observeEvent(input$attack,  play_round("attack"))
  observeEvent(input$special, play_round("special"))
  observeEvent(input$guard,   play_round("guard"))
  observeEvent(input$restart, rv$screen <- "select")
  
  # --- Screens ---
  output$hero_preview <- renderUI({
    h <- hero_template[[input$hero_pick]]
    req(h)
    div(class = "card",
        h3(h$name), p(em(h$blurb)),
        p("HP: ", h$hp), p("Attack: ", h$attack),
        p("Defense: ", h$defense), p("Speed: ", h$speed),
        p("Special: ", strong(h$special)))
  })
  
  output$screen <- renderUI({
    
    if (rv$screen == "select") {
      return(tagList(
        h2("Choose your hero"),
        div(style = "text-align:center;",
            selectInput("hero_pick", "Choose your hero:",
                        choices = names(hero_template)),
            uiOutput("hero_preview"),
            actionButton("start", "Start quest", class = "btn-primary")
        )
      ))
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