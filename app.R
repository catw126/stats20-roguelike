library(shiny)
library(tidyverse)


# ---------- GAME DATA (edit these to tweak the game) ----------
hero_template <- list(
  knight = list(name = "Knight", hp = 110, attack = 6, defense = 9, intellect = 4,
                speed = 5, level = 1, blurb = "Tough and steady."),
  mage   = list(name = "Mage", hp = 85, attack = 2, defense = 4, intellect = 10,
                speed = 6, level = 1, blurb = "Smart and focused."),
  assassin = list(name = "Assassin", hp = 80, attack = 7, defense = 6, intellect = 4,
                  speed = 9, level = 1, blurb = "Fast and slippery"),
  berserker = list(name = "Berserker", hp = 100, attack = 10, defense = 5, intellect = 2,
                speed = 7, level = 1, blurb = "Balanced and sturdy")
)
 
enemies_template <- data.frame( 
  name    = c("Slime", "Goblin", "Skeleton", "Cultist", "Stone Golem", "Wraith"), #add a few more, can just generate
  hp      = c(40, 60, 60, 50, 70, 60),
  attack  = c(4, 6, 5, 6, 7, 6), #modify these to match the <= 10 base stats from above
  phys_defense = c(2, 2, 4, 1, 7, 15), #simulating wraith invis with just insanely high physical defense
  mag_defense = c(1, 2, 2, 5, 3, 7), 
  speed = c(3, 4, 5, 5, 2, 6),
  stringsAsFactors = FALSE
) 

boss <- data.frame(
  name = "Ember Dragon",
  hp = 120,
  attack = 9,
  phys_defense = 7,
  mag_defense = 7,
  speed = 7,
  stringsAsFactors = FALSE
)

#TODO: implement a way to randomize the grunt waves per run, but keeping corresponding stats for the same enemy intact
#maybe create a function that creates a vector of enemy names by randomly sampling the set of names (excluding boss)

moves <- tribble( #move names are placeholders, replace once we know actual moves
  ~owner,         ~id,              ~label,            ~type,                    ~power,
  "knight",       "slash",          "Slash",           "physical",               1.0,
  "knight",       "shield_bash",    "Shield Bash",     "physical+condition_st",  0.7,
  "knight",       "spear_throw",    "Spear Throw",     "physical",               1.0,
  "mage",         "fireball",       "Fireball",        "magic",                  1.0,
  "mage",         "ice_shard",      "Ice Shard",       "magic",                  1.0,    
  "mage",         "power_boost",    "Power Boost",     "stat",                   0.0,
  "mage",         "sleep_fog",      "Sleep Fog",       "condition_sl",           0.0,
  "assassin",     "backstab",       "Backstab",        "physical",               1.0,
  "assassin",     "shadow_bolt",    "Shadow Bolt",     "magic",                  1.0,
  "assassin",     "agility",        "Agility",         "stat",                   0.0,
  "berserker",    "frenzy_chop",    "Frenzy Chop",     "physical",               1.0,
  "berserker",    "bloodlust_roar", "Bloodlust Roar",  "stat",                   0.0,
  "berserker",    "body_slam",      "Body Slam",       "physical",               1.2,
  "Slime",        "tackle",         "Tackle",          "physical",               1.0,
  "Slime",        "acid_spit",      "Acid Spit",       "physical+stat",          0.5,
  "Goblin",       "cowards_ret",    "Coward's Retreat","stat+heal",              0.0,
  "Goblin",       "rusty_shiv",     "Rusty Shiv",      "physical",               1.0,
  "Skeleton",     "bone_bash",      "Bone Bash",       "physical",               1.2,
  "Cultist",      "staff_strike",   "Staff Strike",    "physical",               0.5, #im simulating difference between physical and magical with this, easier than making 2 stats
  "Cultist",      "dark_siphon",    "Dark Siphon",     "physical+heal",          1.0,
  "Stone Golem",  "boulder_smash",  "Boulder Smash",   "physical",               1.5,
  "Stone Golem",  "harden",         "Harden",          "stat",                   0.0,     
  "Wraith",       "chilling_touch", "Chilling Touch",  "physical",               1.8, #higher number for ignore defense ig
  "Ember Dragon", "rending_claw",   "Rending Claw",    "physical+condition_p",   1.0,         
  "Ember Dragon", "scorching_br",   "Scorching Breath","physical",               1.3,
  "Ember Dragon", "tail_sweep",     "Tail Sweep",      "physical+condition_st",  1.0,
  "Ember Dragon", "ash_cloud",      "Ash Cloud",       "stat",                   0
)


reward_text <- "You claim the Dragon's Hoard: 500 gold and the Ember Crown!"

# ---------- HELPER FUNCTIONS (plain R, no Shiny needed) ----------

calc_damage <- function(attack, defense, mult = 1, power) {
  stopifnot(is.numeric(attack), is.numeric(defense), is.numeric(mult))
  a <- attack * mult * power #1 for normal, 1.5 for crit if we do that
  raw <- a * (10/(10 + defense)) * runif(1, 0.8, 1.2)
  return (max(1, round(raw)))
}

hp_bar <- function(current, max_hp, colour) {
  pct <- max(0, round(100 * current / max_hp))
  div(class = "bar-outer",
      div(class = "bar-inner",
          style = paste0("width:", pct, "%; background:", colour, ";")),
      span(class = "bar-label", paste0(max(0, current), " / ", max_hp)))
}

get_moves <- function(owner) {
  return(moves[moves$owner == owner, ])
}

get_enemy_stat <- function(en_name, en_stat) {
  if (!(en_stat %in% enemies_template))
    return(invisible())
  return(enemies_template$en_stat[enemies_template$name == en_name])
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
    hero = NULL, hero_key = "", hp = 0, guard = FALSE, wave = NULL,
    enemy_idx = 1, enemy_hp = 0,
    log = character(),
    
    # --- DYNAMIC MODIFIERS & BUFFS (Temporary additions/multipliers) ---
    atk_mod = 0, #gonna treat these like stat stages, with each additional stage changing the multiplier 
    int_mod = 0, #probably should max out at +/- 4
    def_mod = 0,
    spd_mod = 0,
    
    enemy_atk_mod = 0,
    enemy_pdef_mod = 0,
    enemy_mdef_mod = 0,
    enemy_spd_mod = 0,
    
    # --- STATUS FLAGS ---
    poisoned = FALSE,
    stunned = FALSE,
    sleep = FALSE, #can add more status effects if we feel like it but this is base list
    
    enemy_poisoned = FALSE,
    enemy_stunned = FALSE,
    enemy_sleep = FALSE
  )
  
  calc_fraction <- function(modifier){
    if (modifier == 0){
      return(1)
    }
    else if (modifier < 0){
      return (2/(2 + abs(modifier)))
    }
    else{
      return ((2 + modifier)/2)
    }
  }
  
  apply_stat_move <- function(id, user_name, enemy_name) {
    spec <- switch(id,
                   power_boost    = list(stat = "int_mod", delta = 1, target = "hero", label = "intellect"),
                   agility        = list(stat = "spd_mod", delta = 1, target = "hero", label = "speed"),
                   bloodlust_roar = list(stat = "enemy_atk_mod", delta = -1, target = "enemy", label = "attack"),
                   acid_spit      = list(stat = "def_mod", delta = -1, target = "hero", label = "defense"),
                   cowards_ret    = list(stat = "enemy_spd_mod", delta = 1, target = "enemy", label = "speed"),
                   ash_cloud      = list(stat = "enemy_spd_mod", delta = 4, target = "enemy", label = "speed"),
                   harden         = list(stat = "enemy_pdef_mod", delta = 2, target = "enemy", label = "physical defense"),
                   NULL)
    if (is.null(spec)) return(invisible())
    
    who <- if (spec$target == "enemy") enemy_name else user_name
    rv[[spec$stat]] <- max(-4, min(4, rv[[spec$stat]] + spec$delta))
    if (spec$delta > 0) {
      add_log(who, "'s ", spec$label, " rose!")
    } else {
      add_log(who, "'s ", spec$label, " fell!")
    }
  }
  
  apply_heal <- function(id, user_name, max_hp, damage_dealt){
    spec <- switch(id,
                   cowards_ret    = list(num = 10, target = "enemy_hp", percentage = FALSE),
                   dark_siphon    = list(num = damage_dealt, target = "enemy_hp", percentage = FALSE),
                   NULL)
    if (is.null(spec)) return(invisible())
    if (spec$percentage){
      healed <- rv[[spec$target]]*spec$num
      rv[[spec$target]] <- min(max_hp, rv[[spec$target]]*(1 + spec$num))
      add_log(user_name, " recovered ", healed, " HP!")
    }
    else{
      rv[[spec$target]] <- min(max_hp, rv[[spec$target]] + spec$num)
      add_log(user_name, " recovered ", spec$num, " HP!")
    }
  }
  
  apply_condition <- function(type, target, target_name) {
    base <- switch(type,
                   condition_st = "stunned",
                   condition_sl = "sleep",
                   condition_p  = "poisoned",
                   NULL)
    if (is.null(base)) return(invisible())
    flag <- if (target == "enemy") paste0("enemy_", base) else base
    rv[[flag]] <- TRUE
    msg <- switch(base,
                  stunned  = " is stunned!",
                  sleep    = " falls asleep!",
                  poisoned = " is poisoned!")
    add_log(target_name, msg)
  }
  
  clear_statuses <- function(keep_hpoison = FALSE) {
    rv$stunned <- FALSE
    rv$sleep <- FALSE
    if (!keep_hpoison) rv$poisoned <- FALSE
    rv$enemy_stunned <- FALSE
    rv$enemy_sleep <- FALSE
    rv$enemy_poisoned <- FALSE
  }
  
  add_log <- function(...) {
    rv$log <- c(paste0(...), rv$log)   # newest message goes on top
  }
  
  load_hero <- function(key) {
    rv$hero_key <- key
    rv$hero <- hero_template[[key]]
    rv$hp   <- hero_template[[key]]$hp
  }
  
  start_game <- function(key) {
    load_hero(key)
    rv$guard <- FALSE
    clear_statuses()
    grunts <- enemies_template[sample(nrow(enemies_template), 5, replace = TRUE), ]
    rv$wave <- rbind(grunts, boss)
    rownames(rv$wave) <- NULL
    rv$enemy_idx <- 1
    rv$enemy_hp <- rv$wave$hp[1]
    rv$atk_mod <- 0; rv$def_mod <- 0; rv$spd_mod <- 0; rv$int_mod <- 0;
    rv$enemy_atk_mod <- 0; rv$enemy_mdef_mod <- 0; rv$enemy_spd_mod <- 0; rv$enemy_pdef_mod <- 0;
    rv$log <- paste0("The quest begins! A wild ", rv$wave$name[1], " appears!")
    rv$screen <- "battle"
  }
  
  player_turn <- function(action) {
    hero  <- rv$hero
    enemy <- rv$wave[rv$enemy_idx, ]
    move <- moves[moves$id == action, ]
    hero_atk <- hero$attack * calc_fraction(rv$atk_mod)
    hero_int <- hero$intellect * calc_fraction(rv$int_mod)
    enemy_pdef <- enemy$phys_defense * calc_fraction(rv$enemy_pdef_mod)
    enemy_mdef <- enemy$mag_defense * calc_fraction(rv$enemy_mdef_mod)
    enemy_spd <- enemy$speed*calc_fraction(rv$enemy_spd_mod)
    move_types <- strsplit(move$type, "+", fixed = TRUE)[[1]]
    d <- 0
    
    hero_mult <- if (runif(1, 0, 1) >= 0.80) 1.5 else 1 #Crit chance, do we want to put that in log text?
    crit <- if (hero_mult == 1.5) TRUE else FALSE
    
    if (rv$sleep) {
      if (runif(1) < 0.3) {
        rv$sleep <- FALSE
        add_log(hero$name, " wakes up!")
      } else {
        add_log(hero$name, " is fast asleep.")
        return(invisible())
      }
    }
    
    if (rv$stunned && runif(1, 0, 1) >= 0.70){
        add_log(hero$name, " is stunned and cannot move.")
      return(invisible())
    }
    if ("physical" %in% move_types) { #TODO: implement stat boost/lowering moves once those are finalized
      if (runif(1) < enemy_spd / 100) {
        add_log(enemy$name, " dodges ", hero$name, "'s ", "attack!")
        if (rv$poisoned){
          rv$hp <- rv$hp - floor(rv$hp * (1/8))
          add_log(hero$name, " takes poison damage.")
        }
        if (rv$enemy_poisoned){
          rv$enemy_hp <- rv$enemy_hp - floor(rv$enemy_hp * (1/8))
          add_log(enemy$name, " takes poison damage.")
        }
        return(invisible())
      }
      d <- calc_damage(hero_atk, enemy_pdef, hero_mult, move$power)
      rv$enemy_hp <- max(0, rv$enemy_hp - d)     
      if (crit) add_log("Critical hit!")
      add_log(hero$name, " uses ", move$label, " on ", enemy$name, " for ", d, " damage.")
    } 
    else if ("magic" %in% move_types) { 
      if (runif(1) < enemy_spd / 100) {
        add_log(enemy$name, " dodges ", hero$name, "'s ", "attack!")
        if (rv$poisoned){
          rv$hp <- rv$hp - floor(rv$hp * (1/8))
          add_log(hero$name, " takes poison damage.")
        }
        if (rv$enemy_poisoned){
          rv$enemy_hp <- rv$enemy_hp - floor(rv$enemy_hp * (1/8))
          add_log(enemy$name, " takes poison damage.")
        }
        return(invisible())
      }
      d <- calc_damage(hero_int, enemy_mdef, hero_mult, move$power)
      rv$enemy_hp <- max(0, rv$enemy_hp - d)
      if (crit) add_log("Critical hit!")
      add_log(hero$name, " uses ", move$label, " on ", enemy$name, " for ", d, " damage.")
    } 
    if ("stat" %in% move_types) {
      apply_stat_move(action, hero$name, enemy$name)
    }
    if (any(startsWith(move_types, "condition"))) {
      apply_condition(move_types[startsWith(move_types, "condition")], "enemy", enemy$name)
    }
    if ("heal" %in% move_types){
      apply_heal(action, hero$name, hero$hp, d)
    }
    
    if (rv$poisoned){
      rv$hp <- rv$hp - floor(rv$hp * (1/8))
      add_log(hero$name, " takes poison damage.")
    }
    if (rv$enemy_poisoned){
      rv$enemy_hp <- rv$enemy_hp - floor(rv$enemy_hp * (1/8))
      add_log(enemy$name, " takes poison damage.")
    }
    
    if (rv$hp <= 0) rv$screen <- "defeat"
  }
  
  enemy_turn <- function() {
    hero  <- rv$hero
    enemy <- rv$wave[rv$enemy_idx, ]
    enemy_moves <- get_moves(enemy$name)
    enemy_move <- enemy_moves[sample(nrow(enemy_moves), 1), ]
    enemy_atk <- enemy$attack*calc_fraction(rv$enemy_atk_mod)
    hero_def <- hero$defense*calc_fraction(rv$def_mod)
    hero_spd <- hero$speed*calc_fraction(rv$spd_mod)
    move_types <- strsplit(enemy_move$type, "+", fixed = TRUE)[[1]]
    d <- 0
    
    enemy_mult <- if (runif(1, 0, 1) >= 0.80) 1.5 else 1
    crit <- if (enemy_mult == 1.5) TRUE else FALSE
    
    if (enemy$name == "Ember Dragon" && (rv$enemy_hp < 100)){
      enemy_atk <- 1.5*enemy_atk
      add_log("The Ember Dragon has become enraged!")
    }
    
    if (rv$enemy_sleep) {
      if (runif(1) < 0.3) {
        rv$enemy_sleep <- FALSE
        add_log(enemy$name, " wakes up!")
      } else {
        add_log(enemy$name, " is fast asleep.")
        return(invisible())
      }
    }
    if (rv$enemy_stunned && runif(1, 0, 1) >= 0.70){
        add_log(enemy$name, " is stunned and cannot move.")
      return(invisible())
    }

    if ("physical" %in% move_types) {
      if (runif(1) < hero_spd / 100) {
        add_log(hero$name, " dodges ", enemy$name, "'s ", "attack!")
        if (rv$poisoned){
          rv$hp <- rv$hp - floor(rv$hp * (1/8))
          add_log(hero$name, " takes poison damage.")
        }
        if (rv$enemy_poisoned){
          rv$enemy_hp <- rv$enemy_hp - floor(rv$enemy_hp * (1/8))
          add_log(enemy$name, " takes poison damage.")
        }
        return(invisible())
      }
      d <- calc_damage(enemy_atk, hero_def, enemy_mult, enemy_move$power)
      if (rv$guard){
        d <- max(1, round(d / 2))
      }
      rv$hp <- max(0, rv$hp - d)
      if (crit) add_log("Critical hit!")
      add_log(enemy$name, " uses ", enemy_move$label, " on ", hero$name, " for ", d, " damage.")
    } 
    if ("stat" %in% move_types) {
      apply_stat_move(enemy_move$id, hero$name, enemy$name)
    }
    if (any(startsWith(move_types, "condition"))) {
      apply_condition(move_types[startsWith(move_types, "condition")], "hero", hero$name)
    }
    if ("heal" %in% move_types){
      apply_heal(enemy_move$id, enemy$name, enemy$hp, d)
    }
    
    if (rv$poisoned){
      if (runif(1) < 0.2) {
        rv$poisoned <- FALSE
        add_log(hero$name, " recovers from poison!")
      }
      else{
        rv$hp <- rv$hp - floor(rv$hp * (1/8))
        add_log(hero$name, " takes poison damage.")
      }
    }
    if (rv$enemy_poisoned){
      if (runif(1) < 0.2) {
        rv$enemy_poisoned <- FALSE
        add_log(enemy$name, " recovers from poison!")
      }
      else{
        rv$enemy_hp <- rv$enemy_hp - floor(rv$enemy_hp * (1/8))
        add_log(enemy$name, " takes poison damage.")
      }
    }
    
    if (rv$hp <= 0) rv$screen <- "defeat"
  }
  
  end_wave <- function() {
    hero  <- rv$hero
    enemy <- rv$wave[rv$enemy_idx, ]
    add_log(enemy$name, " is defeated!")
    
    if (rv$enemy_idx == nrow(rv$wave)) {
      rv$screen <- "victory"
    } else {
      clear_statuses(keep_hpoison = TRUE)
      heal <- round(0.4 * hero$hp)
      rv$hp <- min(hero$hp, rv$hp + heal)
      rv$enemy_idx <- rv$enemy_idx + 1
      rv$enemy_hp <- rv$wave$hp[rv$enemy_idx]
      
      rv$atk_mod <- 0; rv$def_mod <- 0; rv$spd_mod <- 0
      rv$enemy_atk_mod <- 0; rv$enemy_mdef_mod <- 0; rv$enemy_spd_mod <- 0; rv$enemy_pdef_mod <- 0
      
      add_log("You rest and recover ", heal, " HP. Next: ",
              rv$wave$name[rv$enemy_idx], "!")
    }
  }
  
  play_round <- function(action) {
    hero <- rv$hero
    enemy <- rv$wave[rv$enemy_idx, ]
    rv$guard <- FALSE
    enemy_spd <- enemy$speed*calc_fraction(rv$enemy_spd_mod)
    hero_spd  <- hero$speed*calc_fraction(rv$spd_mod)
    enemy_first <- enemy_spd > hero_spd   # ties go to the hero
    
    if (action == "guard") {
      rv$guard <- TRUE
      add_log(hero$name, " braces for the next hit.")
    }
    
    if (enemy_first) enemy_turn()
    if (rv$screen == "defeat") return(invisible())
    if (rv$enemy_hp <= 0) {
      end_wave()
      return(invisible())
    }
    
    if (action != "guard") player_turn(action)
    if (rv$screen == "defeat") return(invisible())
    if (rv$enemy_hp <= 0) {
      end_wave()
      return(invisible())
    }
    
    if (!enemy_first) enemy_turn()
    if (rv$screen == "defeat") return(invisible())
    if (rv$enemy_hp <= 0) {
      end_wave()
      return(invisible())
    }
  }
  
  observeEvent(input$start, start_game(input$hero_pick))
  lapply(moves$id, function(mid) {
    observeEvent(input[[paste0("move_", mid)]], play_round(mid), ignoreInit = TRUE)
  })
  observeEvent(input$guard,   play_round("guard"))
  observeEvent(input$restart, rv$screen <- "select")
  
  # --- Screens ---
  output$hero_preview <- renderUI({
    h <- hero_template[[input$hero_pick]]
    req(h)
    div(class = "card",
        h3(h$name), p(em(h$blurb)),
        p("HP: ", h$hp), p("Attack: ", h$attack),
        p("Intellect: ", h$intellect),
        p("Defense: ", h$defense), p("Speed: ", h$speed))
  })
  
  output$move_buttons <- renderUI({
    m <- get_moves(rv$hero_key)
    lapply(seq_len(nrow(m)), function(i) {
      actionButton(paste0("move_", m$id[i]), m$label[i], class = "btn-move btn-primary")
    })
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
                 p("The ", strong(rv$wave$name[nrow(rv$wave)]), " has fallen."),
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
    enemy <- rv$wave[rv$enemy_idx, ]
    fluidRow(
      column(6, div(class = "card",
                    h3(hero$name), hp_bar(rv$hp, hero$hp, "#4caf50")
      )),
      column(6, div(class = "card",
                    h3(enemy$name),
                    hp_bar(rv$enemy_hp, enemy$hp, "#e53935"),
                    p("Enemy ", rv$enemy_idx, " of ", nrow(rv$wave))
      )),
      column(12, div(style = "text-align:center;",
                     uiOutput("move_buttons"),
                     actionButton("guard", "Guard", class = "btn-move btn-info")
      )),
      column(12, h4("Battle log"),
             div(class = "log", lapply(rv$log, function(m) div(m))))
    )
  })
}

shinyApp(ui, server)