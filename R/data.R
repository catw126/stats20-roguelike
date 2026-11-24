# ---------- GAME DATA  ----------
hero_template <- list(
  knight = list(name = "Knight", hp = 110, attack = 6, defense = 9, intellect = 4,
                speed = 3, level = 1, blurb = "Tough and steady."),
  mage   = list(name = "Mage", hp = 65, attack = 2, defense = 4, intellect = 10,
                speed = 6, level = 1, blurb = "Smart and focused."),
  assassin = list(name = "Assassin", hp = 75, attack = 7, defense = 2, intellect = 4,
                  speed = 9, level = 1, blurb = "Fast and slippery"),
  berserker = list(name = "Berserker", hp = 90, attack = 10, defense = 3, intellect = 2,
                   speed = 7, level = 1, blurb = "Balanced and sturdy")
)

enemies_template <- data.frame( 
  name    = c("Slime", "Goblin", "Skeleton"), 
  hp      = c(40, 60, 80),
  attack  = c(3, 4, 5), 
  phys_defense = c(1, 2, 4),
  mag_defense = c(3, 1, 4),
  speed = c(3, 4, 5),
  stringsAsFactors = FALSE
) 

boss <- data.frame(
  name = "Ember Dragon",
  hp = 150,
  attack = 9,
  phys_defense = 8,
  mag_defense = 8,
  speed = 8,
  stringsAsFactors = FALSE
)

#TODO: implement a way to randomize the grunt waves per run, but keeping corresponding stats for the same enemy intact
#maybe create a function that creates a vector of enemy names by randomly sampling the set of names (excluding boss)

moves <- tribble( #move names are placeholders, replace once we know actual moves
  ~owner,         ~id,              ~label,            ~type,
  "knight",       "slash",          "Slash",           "physical",
  "knight",       "shield_bash",    "Shield Bash",     "condition_st",
  "knight",       "spear_throw",    "Spear Throw",     "physical",
  "mage",         "fireball",       "Fireball",        "magic",
  "mage",         "ice_shard",      "Ice Shard",       "magic",
  "mage",         "power_boost",    "Power Boost",     "stat_self",
  "mage",         "sleep_fog",      "Sleep Fog",       "condition_sl",
  "assassin",     "backstab",       "Backstab",        "physical",
  "assassin",     "shadow_bolt",    "Shadow Bolt",     "magic",
  "assassin",     "agility",        "Agility",         "stat_self",
  "berserker",    "frenzy_chop",    "Frenzy Chop",     "physical",
  "berserker",    "bloodlust_roar", "Bloodlust Roar",  "stat_en",
  "berserker",    "body_slam",      "Body Slam",       "physical",
  "Slime",        "bounce",         "Bounce",          "physical",
  "Slime",        "poison",         "Poison",          "condition_p",
  "Goblin",       "club_smash",     "Club Smash",      "physical",
  "Skeleton",     "bone_throw",     "Bone Throw",      "physical",
  "Ember Dragon", "tail_swipe",     "Tail Swipe",      "physical",
  "Ember Dragon", "fire_breath",    "Fire Breath",     "physical"
)


reward_text <- "You claim the Dragon's Hoard: 500 gold and the Ember Crown!"
