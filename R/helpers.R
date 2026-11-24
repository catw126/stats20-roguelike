# ---------- HELPER FUNCTIONS ----------
initial_state <- function() {
  list(
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
}

calc_damage <- function(attack, defense, mult = 1) {
  stopifnot(is.numeric(attack), is.numeric(defense), is.numeric(mult))
  a <- attack * mult #1 for normal, 1.5 for crit if we do that
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
  if (!(en_stat %in% names(enemies_template)))
    return(invisible())
  return(enemies_template[[en_stat]][enemies_template$name == en_name])
}

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

