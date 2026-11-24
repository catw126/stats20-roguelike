apply_stat_move <- function(id, user_name, enemy_name) {
  spec <- switch(id,
                 power_boost    = list(stat = "int_mod", delta = 1, target = "self", label = "intellect"),
                 agility        = list(stat = "spd_mod", delta = 1, target = "self", label = "speed"),
                 bloodlust_roar = list(stat = "enemy_atk_mod", delta = -1, target = "enemy", label = "attack"),
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
  rv$wave <- enemies_template[sample(nrow(enemies_template), 5, replace = TRUE), ]
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
  
  hero_mult <- if (runif(1, 0, 1) >= 0.80) 1.5 else 1 #Crit chance, do we want to put that in log text?
  
  if (rv$sleep) {
    if (runif(1) < 0.5) {
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
  if (move$type == "physical") { #TODO: implement stat boost/lowering moves once those are finalized
    d <- calc_damage(hero_atk, enemy_pdef, hero_mult)
    rv$enemy_hp <- max(0, rv$enemy_hp - d)        
    add_log(hero$name, " uses ", move$label, " on ", enemy$name, " for ", d, " damage.")
  } 
  else if (move$type == "magic") { 
    d <- calc_damage(hero_int, enemy_mdef, hero_mult)
    rv$enemy_hp <- max(0, rv$enemy_hp - d)
    add_log(hero$name, " uses ", move$label, " on ", enemy$name, " for ", d, " damage.")
  } 
  else if (startsWith(move$type, "stat")) {
    apply_stat_move(action, hero$name, enemy$name)
  }
  else {
    apply_condition(move$type, "enemy", enemy$name)
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
  
  enemy_mult <- if (runif(1, 0, 1) >= 0.80) 1.5 else 1
  
  if (rv$enemy_sleep) {
    if (runif(1) < 0.5) {
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
  
  if (enemy_move$type == "physical") {
    if (runif(1) < hero_spd / 100) {
      add_log(hero$name, " dodges ", enemy$name, "'s ", enemy_move$label, "!")
      return(invisible())
    }
    d <- calc_damage(enemy_atk, hero_def, enemy_mult)
    if (rv$guard){
      d <- max(1, round(d / 2))
    }
    rv$hp <- max(0, rv$hp - d)
    add_log(enemy$name, " uses ", enemy_move$label, " on ", hero$name, " for ", d, " damage.")
  } else {
    apply_condition(enemy_move$type, "hero", hero$name)
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
