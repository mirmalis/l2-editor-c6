library(dplyr)
library(data.table)
library(stringr)
name <- data.table(read.table("skillname.txt", header = T, sep = '\t', quote = ""))
setkey(name, id, level)

# dance buffs
name[name == 'Dance of the Warrior',description := description %>% str_replace("Temporarily boosts party members' P. Atk. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                               "Temporarily increases party members' P. Atk. Continuous dancing consumes additional MP. Usable while one is equipped with a dual-sword type of weapon.") %>% 
                                                                   str_replace("Temporarily increases party members' P. Atk. Continuous dancing consumes additional MP. Usable while one is equipped with a dual-sword type of weapon.", 
                                                                               "Increases P. Atk. by 12%")]
name[name == 'Dance of Inspiration',description := description %>% str_replace("Temporarily increases party members' accuracy. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                               "Temporarily increases party members' Accuracy. Continuous dancing consumes additional MP. Usable while one is equipped with a dual-sword type weapon.") %>% 
                                                                   str_replace("Temporarily increases party members' Accuracy. Continuous dancing consumes additional MP. Usable while one is equipped with a dual-sword type weapon.", 
                                                                               "Increases Accuracy by 4.")]
name[name == 'Dance of the Mystic',description := description %>% str_replace("Temporarily increases party members' M. Atk. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                              "Temporarily increases party members' M. Atk. Continuous dancing consumes additional MP. Usable while one is equipped with dual-sword type weapon.") %>% 
                                                                  str_replace("Temporarily increases party members' M. Atk. Continuous dancing consumes additional MP. Usable while one is equipped with dual-sword type weapon.", 
                                                                              "Increases M. Atk. by 20%.")]
name[name == 'Dance of Fire',description := description %>% str_replace("Temporarily increases party members' critical damage. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                        "Temporarily enhances party members' critical attack capability. Continuous dancing consumes additional MP. Usable while one is equipped with dual-sword type weapon.") %>% 
                                                            str_replace("Temporarily enhances party members' critical attack capability. Continuous dancing consumes additional MP. Usable while one is equipped with dual-sword type weapon.", 
                                                                        "Increases Critical Damage by 35%.")]
name[name == 'Dance of Fury',description := description %>% str_replace("Temporarily increases party members' attack speed. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                        "Temporarily enhances party members' Atk. Spd. Continuous dancing consumes additional MP. Usable while one is equipped with dual-sword type weapon.") %>% 
                                                            str_replace("Temporarily enhances party members' Atk. Spd. Continuous dancing consumes additional MP. Usable while one is equipped with dual-sword type weapon.", 
                                                                        "Increases Atk. Spd. by 15%.")]
name[name == 'Dance of Concentration',description := description %>% str_replace("Temporarily decreases party members' magic cancel rate to damage and increases Casting Spd. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                                 "Temporarily increases party members' Casting Spd., and decreases magic cancel rate. Continuous dancing consumes additional MP. An equipped two-handed sword weapon is required to use this skill.") %>% 
                                                                     str_replace("Temporarily increases party members' Casting Spd., and decreases magic cancel rate. Continuous dancing consumes additional MP. An equipped two-handed sword weapon is required to use this skill.", 
                                                                                 "Decreases magic cancel damage by 40 and increases Casting Spd. by 30%.")] 
name[name == 'Dance of Light',description := description %>% str_replace("Temporarily bestows sacred power to party members' physical attack. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                         "Temporarily bestows sacred power to party members' physical attack. Continuous dancing consumes additional MP. An equipped two-handed sword weapon is required to use this skill.") %>% 
                                                             str_replace("Temporarily bestows sacred power to party members' physical attack. Continuous dancing consumes additional MP. An equipped two-handed sword weapon is required to use this skill.", 
                                                                         "Makes all physical attacks Holy attribute.")]
name[name == 'Dance of Aqua Guard',description := description %>% str_replace("Temporarily increases party members' resistance to attacks by water. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                              "Temporarily increases party members' resistance to attacks by water. Continuous dancing consumes additional MP. This skill can be used while one is equipped with  two-handed swords.") %>% 
                                                                  str_replace("Temporarily increases party members' resistance to attacks by water. Continuous dancing consumes additional MP. This skill can be used while one is equipped with  two-handed swords.", 
                                                                              "Increases Resistance to Water attacks by 30%.")]
name[name == 'Dance of Earth Guard',description := description %>% str_replace("Temporarily increases party members' resistance to attacks by earth. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                               "Temporarily increases party members' resistance to earth attacks. Continuous dancing consumes additional MP. This skill can be used while one is equipped with  two-handed swords.") %>% 
                                                                   str_replace("Temporarily increases party members' resistance to earth attacks. Continuous dancing consumes additional MP. This skill can be used while one is equipped with  two-handed swords.", 
                                                                               "Increases Resistance to Earth attacks by 30%.")]
name[name == 'Dance of the Vampire',description := description %>% str_replace("Partially restores party members' HP by using damage inflicted upon the enemy. Damage inflicted by skill or remote attack is excluded. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", "Partially restores party members' HP by using damage inflicted upon the enemy. Damage inflicted by skill or remote attack is excluded. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.") %>% 
                                                                   str_replace("Partially restores party members' HP by using damage inflicted upon the enemy. Damage inflicted by skill or remote attack is excluded. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                               "Gives the ability to recover 8% of any standard melee damage inflicted on the enemy as HP.")]
name[name == 'Dance of Protection',description := description %>% str_replace("Temporarily increases party members' resistance to terrain damage. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                              "Temporarily increases party members' resistance to terrain damage. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.") %>% 
                                                                  str_replace("Temporarily increases party members' resistance to terrain damage. Continuous dancing consumes additional MP. An equipped Dual-Sword Weapon is required to use this skill.", 
                                                                              "Gives the ability to decrease any environment-related damage by 30.")]
name[name == "Siren's Dance",description := description %>% str_replace("Temporarily increases a party member's success rate of inflicting critical damage through attack magic. Continuous dancing consumes additional MP. Usable when equipped with a two-blade weapon.", 
                                                                        "Increases Magic Critical Rate by 200%")]
name[name == 'Dance of Shadows',description := description %>% str_replace("Temporarily decreases speed and gives party members the ability to be unaffected by a monster's preemptive attack. Continuous dancing consumes additional MP. Usable when equipped with a two-blade weapon.", 
                                                                           "Decreases Speed by 50% and prevents target from being pre-emptively attacked by monsters.")]


# songs
name[name == 'Song of Earth',description := description %>% str_replace("Temporarily increases P. Def. of party members. Continuous singing consumes additional MP.", 
                                                                        "Temporarily increases P. Def. of party members. Continuous singing consumes additional MP while singing/dancing effects remain.") %>% 
                                                            str_replace("Temporarily increases P. Def. of party members. Continuous singing consumes additional MP while singing/dancing effects remain.", 
                                                                        "Increases P. Def. by 25%.")]
name[name == 'Song of Life',description := description %>% str_replace("Temporarily increases party members' HP regeneration. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases party members' HP regeneration. Continuous singing consumes additional MP.") %>% 
                                                           str_replace("Temporarily increases party members' HP regeneration. Continuous singing consumes additional MP.", 
                                                                       "Increases HP Regeneration by 20%.")]
name[name == 'Song of Water',description := description %>% str_replace("Temporarily increases party's Evasion. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases Evasion of party members. Continuous singing consumes additional MP while singing/dancing effects remain.") %>% 
                                                           str_replace("Temporarily increases Evasion of party members. Continuous singing consumes additional MP while singing/dancing effects remain.", 
                                                                       "Increases Evasion by 3.")]
name[name == 'Song of Warding',description := description %>% str_replace("Temporarily increases party members' M. Def. When singing continuously, additional MP is consumed.", 
                                                                       "Temporarily increases M. Def. of party members. Continuous singing consumes additional MP while singing/dancing effects remain.") %>% 
                                                           str_replace("Temporarily increases M. Def. of party members. Continuous singing consumes additional MP while singing/dancing effects remain.", 
                                                                       "Increases M. Def. by 30%.")]
name[name == 'Song of Wind',description := description %>% str_replace("Temporarily increases party's movement. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases Speed of party members. Continuous singing consumes additional MP while singing/dancing effects remain.") %>% 
                                                           str_replace("Temporarily increases Speed of party members. Continuous singing consumes additional MP while singing/dancing effects remain.", 
                                                                       "Increases Speed by 20.")]
name[name == 'Song of Hunter',description := description %>% str_replace("Temporarily increases party members' critical rate. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases critical attack rate of party members. Continuous singing consumes additional MP while singing/dancing effects remain.") %>% 
                                                           str_replace("Temporarily increases critical attack rate of party members. Continuous singing consumes additional MP while singing/dancing effects remain.", 
                                                                       "Increases Critical Rate by 100%.")]
name[name == 'Song of Invocation',description := description %>% str_replace("Temporarily increases party members' resistance to dark magic attacks. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases party members' resistance to dark attack. Continuous singing consumes additional MP while singing/dancing effects remain.") %>% 
                                                           str_replace("Temporarily increases party members' resistance to dark attack. Continuous singing consumes additional MP while singing/dancing effects remain.", 
                                                                       "Increases Resistance to Dark attacks by 20%.")]
name[name == 'Song of Vitality',description := description %>% str_replace("Instantly increases party members' maximum HP. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases party members' maximum HP. Continuous singing while singing/dancing effect lasts consumes additional MP.") %>% 
                                                           str_replace("Temporarily increases party members' maximum HP. Continuous singing while singing/dancing effect lasts consumes additional MP.", 
                                                                       "Increases Max HP by 30%.")]
name[name == 'Song of Vengeance',description := description %>% str_replace("Temporarily reflects damage received by party members back upon the enemy. Excludes damage received from skill or remote attack. Continuous singing consumes additional MP.", 
                                                                       "Temporarily reflects damage received by party members back upon the enemy. Excludes damage received from skill or remote attack. Continuous singing while singing/dancing is in effect will consume additional MP.") %>% 
                                                           str_replace("Temporarily reflects damage received by party members back upon the enemy. Excludes damage received from skill or remote attack. Continuous singing while singing/dancing is in effect will consume additional MP.", 
                                                                       "Gives the ability to transfer 20% of received standard short-range damage back to the enemy.")]
name[name == 'Song of Flame Guard',description := description %>% str_replace("Temporarily increases party members' resistance to attacks by fire. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases party members' resistance to attacks by fire. Continuous singing consumes additional MP. Enchant Time: the skill's duration is increased.") %>% 
                                                           str_replace("Temporarily increases party members' resistance to attacks by fire. Continuous singing consumes additional MP. Enchant Time: the skill's duration is increased.", 
                                                                       "Increases Resistance to Fire attacks by 30% for 2 minutes")]
name[name == 'Song of Storm Guard',description := description %>% str_replace("Temporarily increases party members' resistance to attacks by wind. Continuous singing consumes additional MP.", 
                                                                       "Temporarily increases party members' resistance to attacks by wind. Continuous singing consumes additional MP.") %>% 
                                                           str_replace("Temporarily increases party members' resistance to attacks by wind. Continuous singing consumes additional MP.", 
                                                                       "Increases Resistance to Wind attacks by 30%.")]
name[name == 'Song of Renewal',description := description %>% str_replace("Temporarily decreases party members' MP consumption and re-use time while using physical/magic skills. If one sings while sing/dance state is in effect, additional MP will be consumed.", 
                                                                       "Decreases physical/magic skill MP consumption rate by 5% and re-use time by 30%.")]
name[name == 'Song of Meditation',description := description %>% str_replace("Temporarily increases party members' MP regeneration rate and decreases MP consumption rate when using a magic skill. Continuous singing consumes additional MP.", 
                                                                       "Increases MP Recovery Bonus by 20%, and decreases magic skill MP consumption by 10%.")]
name[name == 'Song of Champion',description := description %>% str_replace("Temporarily decreases party members' MP consumption/re-use time when using physical skills. Continuous singing consumes additional MP.", 
                                                                       "Decreases MP consumption by 20% and reuse time by 30% for physical/sing/dance skill use.")]

#buffs
# Magic Barrier
# name[name %in% c('Iron Will', 'Spirit Barrier', 'Chant of Fire', "The Glory of Pa'agrio", 'Magic Barrier', 'Servitor Magic Shield'), description] %>% unique
name[name %in% c('Iron Will', 'Spirit Barrier', 'Chant of Fire', "The Glory of Pa'agrio", 'Magic Barrier', 'Servitor Magic Shield'), description := description %>%
       str_replace("Temporarily increases M. Def. Effect 1.", "Increases M. Def. by 15%.") %>%
       str_replace("Temporarily increases M. Def. Effect 2.", "Increases M. Def. by 23%.") %>% 
       str_replace("Temporarily increases M. Def. Effect 3.", "Increases M. Def. by 30%.") %>% 
       str_replace("Temporarily increases party member's magic resistance. Effect 1.", "Increases M. Def. by 15%.") %>% 
       str_replace("Temporarily increases party member's magic resistance. Effect 2.", "Increases M. Def. by 23%.") %>% 
       str_replace("Temporarily increases party member's magic resistance. Effect 3.", "Increases M. Def. by 30%.") %>% 
       str_replace("Temporarily increases alliance members' M. Def. Effect 1.", "Increases M. Def. by 15%.") %>% 
       str_replace("Temporarily increases alliance members' M. Def. Effect 2.", "Increases M. Def. by 23%.") %>% 
       str_replace("Temporarily increases alliance members' M. Def. Effect 3.", "Increases M. Def. by 30%.") %>% 
       str_replace("Temporarily increases servitor's M. Def. Effect 2.", "Increases servitor's M. Def. by 23%.") %>% 
       str_replace("Temporarily increases servitor's M. Def. Effect 3.", "Increases servitor's M. Def. by 30%.") %>% 
       str_replace("Effect 3.", "") %>%
       str_replace("Temporarily increases one's M. Def.", "Increases M. Def. by 30%.") %>% 
       str_replace("Temporarily increases M. Def.", "Increases M. Def. by 30%.") %>% 
       str_replace("Temporarily increases party members' M. Def.", "Increases M. Def. by 30%.") %>%
       str_replace("Temporarily increases alliance members' M. Def.", "Increases M. Def. by 30%.") %>%
       str_replace("Temporarily increases servitor's M. Def.", "Increases servitor's M. Def. by 30%.") %>% 
       str_replace("%.  ", "%. ") # panaikina dviguba tarpa :|
]

# Shield
# name[name %in% c('Shield', 'Defense Aura', 'Soul Shield', "Blessings of Pa'agrio", "Servitor Physical Shield", 'Holiday Shield', 'Shield for Beginners'), description] %>% unique
name[name %in% c('Shield', 'Defense Aura', 'Soul Shield', "Blessings of Pa'agrio", "Servitor Physical Shield", 'Holiday Shield', 'Shield for Beginners'), description := description %>%
       str_replace_all(c("Temporarily increases P. Def. Effect 1." = "Increases P. Def. by 8%.",
                         "Temporarily increases clan member's defense. Effect 1." = "Increases P. Def. by 8%.",
                         "Temporarily increases defense. Effect 1." = "Increases P. Def. by 8%.",
                         "Temporarily increases P. Def. Effect 2." = "Increases P. Def. by 12%.",
                         "Temporarily increases clan member's defense. Effect 2." = "Increases P. Def. by 12%.",
                         "Temporarily increases defense. Effect 2." = "Increases P. Def. by 12%.", 
                         "Increases P. Def. by 12%." = "Increases P. Def. by 12%.",
                         "Temporarily increases P. Def. Effect 3." = "Increases P. Def. by 15%.", 
                         "Temporarily increases clan member's defense. Effect 3." = "Increases P. Def. by 15%.",
                         "Temporarily increases defense. Effect 3." = "Increases P. Def. by 15%.", 
                         "Temporarily increases servitor's P. Def. Effect 1."= "Increases servitor's P. Def. by 8%.",
                         "Temporarily increases servitor's P. Def. Effect 2."= "Increases servitor's P. Def. by 12%.",
                         "Temporarily increases servitor's P. Def. Effect 3."= "Increases servitor's P. Def. by 15%.",
                         "Effect 3." = "",
                         "Temporarily increases P. Def." =  "Increases P. Def. by 15%.",
                         "Temporarily increases servitor's P. Def." = "Increases servitor's P. Def. by 15%.",
                         "Temporarily increases alliance members' P. Def." = "Increases P. Def. by 15%.")
       )
]
name[name == 'Shield for Beginners']

# greater shields
name[name %in% c('Greater Shield', 'Earth Chant'), description := description %>%
       str_replace_all(c("Temporarily increases your target's P. Def. Consumes 1 Spirit Ore. Effect 1.", "Temporarily increases your party's P. Def. Consumes 4 Spirit Ore. Effect 1."), 
                       "Increases P. Def. by 5%.") %>%
       str_replace_all(c("Temporarily increases your target's P. Def. Consumes 2 Spirit Ore. Effect 2.", "Temporarily increases your party's P. Def. Consumes 8 Spirit Ore. Effect 2."), 
                       "Increases P. Def. by 10%.") %>% 
       str_replace_all(c("Temporarily increases your target's P. Def. Consumes 3 Spirit Ore. Effect 3.", "Temporarily increases your party's P. Def. Consumes 12 Spirit Ore. Effect 3."), 
                       "Increases P. Def. by 15%.")
]
# greater mights
name[name %in% c("Greater Might", "War Chant"), description := description %>%
       str_replace_all(c("Temporarily increases your target's P. Atk. Consumes 1 Spirit Ore. Effect 1.","Temporarily increases your party's P. Atk. Consumes 4 Spirit Ore. Effect 1."), 
                       "Increases P. Atk. by 4%.") %>% 
       str_replace_all(c("Temporarily increases your target's P. Atk. Consumes 2 Spirit Ore. Effect 2.","Temporarily increases your party's P. Atk. Consumes 8 Spirit Ore. Effect 2."), 
                       "Increases P. Atk. by 7%.") %>% 
       str_replace_all(c("Temporarily increases your target's P. Atk. Consumes 3 Spirit Ore. Effect 3.","Temporarily increases your party's P. Atk. Consumes 12 Spirit Ore. Effect 3."), 
                       "Increases P. Atk. by 10%.")
]
# Berserker SPirit
1 %in% 1:124
name[name %in% c('Berserker Spirit', "The Rage of Pa'agrio"), description] %>% unique
name[name %in% c('Berserker Spirit', "The Rage of Pa'agrio"), description := description %>%
       str_replace_all(c("Temporarily reduces P. Def., M. Def. and Evasion while increasing P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Can be used on one's party members. Effect 1." = "Decreases P. Def. by 5%, M. Def. by 10% and Evasion by 2, and increases P. Atk. by 5%, M. Atk. by 10%, Atk. Spd. by 5%, Casting Spd. by 5% and Speed by 5.",
                         "Temporarily reduces P. Def., M. Def. and Evasion, and increases P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Effect 1."                                       = "Decreases P. Def. by 5%, M. Def. by 10% and Evasion by 2, and increases P. Atk. by 5%, M. Atk. by 10%, Atk. Spd. by 5%, Casting Spd. by 5% and Speed by 5.",
                         "Temporarily reduces alliance members' P. Def., M. Def. and Evasion, and increases P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Effect 1."                     = "Decreases P. Def. by 5%, M. Def. by 10% and Evasion by 2, and increases P. Atk. by 5%, M. Atk. by 10%, Atk. Spd. by 5%, Casting Spd. by 5% and Speed by 5.",
                         "Temporarily reduces P. Def., M. Def. and Evasion while increasing P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Can be used on one's party members. Effect 2." = "Decreases P. Def. by 8%, M. Def. by 16% and Evasion by 4, and increases P. Atk. by 8%, M. Atk. by 16%, Atk. Spd. by 8%, Casting Spd. by 8%, and Speed by 8.",
                         "Temporarily reduces P. Def., M. Def. and Evasion, and increases P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Effect 2."                                       = "Decreases P. Def. by 8%, M. Def. by 16% and Evasion by 4, and increases P. Atk. by 8%, M. Atk. by 16%, Atk. Spd. by 8%, Casting Spd. by 8%, and Speed by 8.",
                         "Temporarily reduces alliance members' P. Def., M. Def. and Evasion, and increases P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Effect 2."                     = "Decreases P. Def. by 8%, M. Def. by 16% and Evasion by 4, and increases P. Atk. by 8%, M. Atk. by 16%, Atk. Spd. by 8%, Casting Spd. by 8%, and Speed by 8.",
                         "Effect 2."                                                                                                                                                            = "",
                         "Temporarily reduces alliance members' P. Def., M. Def. and Evasion, and increases P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed."                               = "Decreases P. Def. by 8%, M. Def. by 16% and Evasion by 4, and increases P. Atk. by 8%, M. Atk. by 16%, Atk. Spd. by 8%, Casting Spd. by 8%, and Speed by 8.",
                         "Temporarily reduces P. Def., M. Def. and Evasion while increasing P. Atk., M. Atk., Atk. Spd., Casting Spd. and Speed. Can be used on one's party members."           = "Decreases P. Def. by 8%, M. Def. by 16% and Evasion by 4, and increases P. Atk. by 8%, M. Atk. by 16%, Atk. Spd. by 8%, Casting Spd. by 8%, and Speed by 8.",
                         "Temporarily reduces P. Def., M. Def., and Evasion and increases P. Atk., M. Atk., Atk. Spd., Casting Spd., and Speed. Can be used on one's party members."            = "Decreases P. Def. by 8%, M. Def. by 16% and Evasion by 4, and increases P. Atk. by 8%, M. Atk. by 16%, Atk. Spd. by 8%, Casting Spd. by 8%, and Speed by 8.")
                       )
]

# Haste



name[str_detect(description, 'Atk. Spd') & level == 1, name]
name[description=='Increases Atk. Spd.']

name[name %in% c('Haste', 'Servitor Haste', 'Chant of Fury', 'Herb of Atk. Spd.', 'Potion of Alacrity', 'Greater Swift Attack Potion', 'Scroll of Haste', 'Scroll of Haste - Event Use', 'Holiday Haste','Haste for Beginners', "Master's Blessing - Haste"), description] %>% unique
name[name %in% c('Haste', 'Servitor Haste', 'Chant of Fury', 'Herb of Atk. Spd.', 'Potion of Alacrity', 'Greater Swift Attack Potion', 'Scroll of Haste', 'Scroll of Haste - Event Use', 'Holiday Haste','Haste for Beginners', "Master's Blessing - Haste"), description := description %>%
       str_replace_all(c("Temporarily increases Atk. Spd. Effect 1." = "Increases Atk.Spd. by 15%.",
                         "Temporarily increases servitor's Atk. Spd. Effect 1." = "Increases servitor's Atk.Spd. by 15%.",
                         "Temporarily increases party members' Atk. Spd. Effect 1." = "Increases Atk.Spd. by 15%.",
                         "Temporarily increases Atk. Spd. Effect 2." = "Increases Atk.Spd. by 33%.",
                         "Temporarily increases servitor's Atk. Spd. Effect 2." = "Increases servitor's Atk.Spd. by 33%.",
                         "Temporarily increases servitor's Atk. Spd." = "Increases servitor's Atk.Spd. by 33%.",
                         "Temporarily increases Atk. Spd.  Effect 2." = "Increases Atk.Spd. by 33%.",
                         "A GM is always by your side. Increases Atk. Spd. temporarily. Effect 2." = "Increases Atk.Spd. by 33%.",
                         "Temporarily increases party members' Atk. Spd. Effect 2." = "Increases Atk.Spd. by 33%.",
                         "Increases Atk. Spd." = "Increases Atk.Spd. by 33%.",
                         " Effect 2." = "",
                         "Temporarily increases Atk. Spd." = "Increases Atk.Spd. by 33%.",
                         "Temporarily increases party members' Atk. Spd." = "Increases Atk.Spd. by 33%.")
                       )
]

name[str_detect(description, 'M. Atk') & level == 1, name] %>% unique
name[name %in% c('Empower', 'Bright Servitor', "The Soul of Pa'agrio", 'Scroll of Mystic Empower', 'Herb of Magic', 'Scroll of Empower - Event Use','Empower for Beginners','Holiday Empower', "Master's Blessing - Empower"), description] %>% unique
name[name %in% c('Empower', 'Bright Servitor', "The Soul of Pa'agrio", 'Scroll of Mystic Empower', 'Herb of Magic', 'Scroll of Empower - Event Use','Empower for Beginners','Holiday Empower', "Master's Blessing - Empower"), description := description %>%
       str_replace_all(c("Temporarily increases M. Atk. Effect 1." = "Increases M. Atk. by 55%.",
                         "Temporarily increases M. Atk. Effect 2." = "Increases M. Atk. by 65%.",
                         "Temporarily increases M. Atk.  Effect 3." = "Increases M. Atk. by 75%.",
                         "Temporarily increases M. Atk. Effect 3." = "Increases M. Atk. by 75%.",
                         "Increases M. Atk. Effect 3." = "Increases M. Atk. by 75%.",
                         "Herb increases M. Atk." = "Increases M. Atk. by 75%.",
                         "A GM is always by your side. Increases M. Atk. temporarily. Effect 3." = "Increases M. Atk. by 75%.",
                         "Temporarily increases servitor's M. Atk. Effect 1." = "Increases servitor's M. Atk. by 55%.",
                         "Temporarily increases servitor's M. Atk. Effect 2." = "Increases servitor's M. Atk. by 65%.",
                         "Temporarily increases servitor's M. Atk. Effect 3." = "Increases servitor's M. Atk. by 75%.",
                         "Temporarily increases alliance members' M. Atk. Effect 3." = "Increases M. Atk. by 75%.",
                         " Effect 3." = "",
                         "Temporarily increases M. Atk." = "Increases M. Atk. by 75%.",
                         "Temporarily increases a servitor's M. Atk." = "Increases M. Atk. by 75%."
                         )
                       )
]
# WW
name[str_detect(description, "Speed") & level == 1, name]
name[name %in% c('Sprint', 'Wind Walk', "Pa'agrian Haste", "Haste Potion", "Greater Haste Potion", "Scroll of Wind Walk", "Herb of Speed", "Scroll of Wind Walk - Event Use", "Wind Walk for Beginners", "Pet Wind Walk", "Master's Blessing - Wind Walk"), description] %>% unique
name[name %in% c('Sprint', 'Wind Walk', "Pa'agrian Haste", "Haste Potion", "Greater Haste Potion", "Scroll of Wind Walk", "Herb of Speed", "Scroll of Wind Walk - Event Use", "Wind Walk for Beginners", "Pet Wind Walk", "Master's Blessing - Wind Walk"), description := description %>%
       str_replace_all(c("Temporarily increases Speed. Effect 1." = "Increases Speed by 20.",
                         "Temporarily increases alliance members' Speed. Effect 1." = "Increases Speed by 20.",
                         "Increases Speed. Effect 1." = "Increases Speed by 20.",
                         "Temporarily increases movement speed. Effect 1." = "Increases Speed by 20.",
                         
                         "Temporarily increases Speed.  Effect 2." = "Increases Speed by 33.",
                         "Temporarily increases movement speed. Effect 2." = "Increases Speed by 33.",
                         "Increases Speed temporarily." = "Increases Speed by 33.",
                         "A GM is always by your side. Increases Speed temporarily. Effect 2." = "Increases Speed by 33.",
                         "Increases Speed. Effect 2." = "Increases Speed by 33.",
                         "Temporarily increases Speed. Effect 2." = "Increases Speed by 33.",
                         "Temporarily increases alliance members' Speed. Effect 2." = "Increases Speed by 33.",
                         " Effect 2." = "",
                         "Temporarily increases one's Speed." = "Increases Speed by 33.",
                         "Temporarily increases Speed." = "Increases Speed by 33.",
                         "Temporarily increases alliance members' Speed." = "Increases Speed by 33."
                         )
                       )
]
# btb
name[str_detect(description, "HP") & level == 1, name]
name[name %in% c("Battle Roar", "Blessed Body", "Bless the Body", "Body of Avatar", "Master's Blessing - Blessed Body", "Pet Blessed Body", "Blessed Body for Beginners"), description] %>% unique
name[name %in% c("Battle Roar", "Blessed Body", "Bless the Body", "Body of Avatar", "Master's Blessing - Blessed Body", "Pet Blessed Body", "Blessed Body for Beginners"), description := description %>%
       str_replace_all(c("Temporarily increases maximum HP and restores HP. Effect 1." = "Increases maximum HP by 10%.",
                         "Temporarily increases maximum HP. Effect 1." = "Increases maximum HP by 10%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP. Effect 1." = "Increases maximum HP by 10%.",
                         "Temporarily increases maximum HP and restores HP. Effect 2." = "Increases maximum HP by 15%.",
                         "Temporarily increases maximum HP. Effect 2." = "Increases maximum HP by 15%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP. Effect 2." = "Increases maximum HP by 15%.",
                         "Temporarily increases maximum HP and restores HP. Effect 3." = "Increases maximum HP by 20%.",
                         "Temporarily increases maximum HP. Effect 3." = "Increases maximum HP by 20%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP. Effect 3." = "Increases maximum HP by 20%.",
                         "Temporarily increases maximum HP and restores HP. Effect 4." = "Increases maximum HP by 25%.",
                         "Temporarily increases maximum HP. Effect 4." = "Increases maximum HP by 25%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP. Effect 4." = "Increases maximum HP by 25%.",
                         "Temporarily increases maximum HP and restores HP. Effect 5." = "Increases maximum HP by 30%.",
                         "Temporarily increases maximum HP. Effect 5." = "Increases maximum HP by 30%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP. Effect 5." = "Increases maximum HP by 30%.",
                         "Temporarily increases maximum HP and restores HP. Effect 6." = "Increases maximum HP by 35%.",
                         "Temporarily increases maximum HP. Effect 6." = "Increases maximum HP by 35%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP. Effect 6." = "Increases maximum HP by 35%.",
                         " Effect 6." = "",
                         "Temporarily increases maximum HP and regenerates HP." = "Increases maximum HP by 35%.",
                         "Temporarily increases maximum HP." = "Increases maximum HP by 35%.",
                         "For a certain time period, increases one's party members' maximum HP and restores the increased portion of their HP." = "Increases maximum HP by 35%.",
                         "Increases maximum HP temporarily." = "Increases maximum HP by 35%.",
                         "A GM is always by your side. Increases maximum HP temporarily. Effect 6." = "Increases maximum HP by 35%."
                         )
                       )
]
name[description == "Temporarily increases one's servitor's P. Atk. Enchant Cost: MP consumption is decreased.  Effect 6.", name]
# Acumen
name[str_detect(description, "Casting Spd.") & level == 1, .(name)]
name[name %in% c('Acumen', "The Wisdom of Pa'agrio", 'Scroll of Greater Acumen', 'Magic Haste Potion', "Herb of Casting Spd.", "Scrol of Acumen - Event Use", "Acumen for Beginners", "Acumen", "Pet Acumen", "Golden Pig Acumen", "Master's Blessing - Acumen"), description] %>% unique
name[name %in% c('Acumen', "The Wisdom of Pa'agrio", 'Scroll of Greater Acumen', 'Magic Haste Potion', "Herb of Casting Spd.", "Scrol of Acumen - Event Use", "Acumen for Beginners", "Acumen", "Pet Acumen", "Golden Pig Acumen", "Master's Blessing - Acumen"), description := description %>%
       str_replace_all(c("Temporarily increases Casting Spd. Effect 1." = "Increases Casting Spd. by 15%.",
                         "Temporarily increases alliance members' Casting Spd. Effect 1." = "Increases Casting Spd. by 15%.",
                         "Temporarily increases Casting Spd. Effect 2." = "Increases Casting Spd. by 23%.",
                         "Temporarily increases alliance members' Casting Spd. Effect 2." = "Increases Casting Spd. by 23%.",
                         "Temporarily increases Casting Spd. Effect 3." = "Increases Casting Spd. by 30%.",
                         "Temporarily increases alliance members' Casting Spd. Effect 3." = "Increases Casting Spd. by 30%.",

                         "Increases Casting Spd. Effect 2." = "Increases Casting Spd. by 23%.",
                         "Increases Casting Spd. Effect 3." = "Increases Casting Spd. by 30%.",
                         "A GM is always by your side. Increases Casting Spd. temporarily. Effect 3." = "Increases Casting Spd. by 30%.",
                         " Effect 3." = "",
                         "Temporarily increases alliance members' Casting Spd." = "Increases Casting Spd. by 30%.",
                         "Temporarily increases Casting Spd." = "Increases Casting Spd. by 30%.",
                         "Increases Casting Spd. temporarily." = "Increases Casting Spd. by 30%."

                         )
                       )
]
# Clarity
name[str_detect(description, "consumption") & level == 1, .(name)]
name[name %in% c("Clarity", "Master's Blessing - Clarity"), description] %>% unique
name[name %in% c("Clarity", "Master's Blessing - Clarity"), description := description %>%
       str_replace_all(c("Temporarily decreases your target's MP consumption rate for skills. Consumes 1 Spirit Ore. Effect 1." = "Decrases skill mp consumption by 4%",
                         "Temporarily decreases your target's MP consumption rate for skills. Consumes 2 Spirit Ore. Effect 2." = "Decrases skill mp consumption by 8%",
                         "Temporarily decreases your target's MP consumption rate for skills. Consumes 3 Spirit Ore. Effect 3." = "Decrases skill mp consumption by 10%",
                         "A GM is always by your side. Temporarily decreases the target's MP consumption for skills. Effect 3." = "Decrases skill mp consumption by 10%"
                         )
                       )
]
# done: mdef, pdef, haste, bers, gmight, gshield, empower, "prohecies", ww,btb, acumen,Clarity
# done: snipe, dead eye, rapid fire, rapid shot

# tbd: btm,mental,resistshock,concentration,vr, DW, Focus, Guidance, Agility, Bless shield, adavanced block, 
# tbd: Kates bufai, arklio bufai
# tbd: resist elements/debuff/cancel
# tbd: malaria, flu, rheumatism, cholera
# tbd: totemai, frenzy,rage,zealot,ai, ud, ue, dash, mortal blow, blinding blow, 
# tbd: heroic (valor, bers, ud, fear, silence).
# tbd: foi, celestial, 

# tbd: debufai: surenderiai fire/wind/water/earth/poison, gloom, silence
# tbd: power break, hex, armor crush, varlordu stunas range, 
# tbd: bleed, poisson, sting, flames
### buff description changing template.
name[str_detect(description, "Speed") & level == 1, name]
name[name %in% c(), description] %>% unique
name[name %in% c(), description := description %>%
       str_replace_all(c("" = "",
                         "" = "",
                         "" = "",
                         "" = "",
                         " Effect 3." = "",
                         "" = "",
                         "" = "",
                         "" = "",
                         "" = "",
                         "" = "",
                         "" = "",
                         "" = "",
                         "" = "")
                       )
]

### Bufai vienetiniai:
name[str_detect(name, 'Magnu')]

name[name == 'Chant of Victory', description := "Max HP +20%, Atk. Spd. +20%, Critical +20%, Critical Damage +20%, P.Atk. +10%, P.Def. +20%, Casting Spd. +20%, M.Def. +20%, M.Atk. +20%, Accuracy +4,Debuff Resistance +20%, Speed -20%."]
name[name == "Magnus' Chant", description := "Casting Speed +20%, M.Def +20%, M.Atk +30%, Max MP +20%, MP Regen +50%, Resist Fire/Water/Earth/Wind +10%, MP Consumption -20%."]
name[name == 'Prophecy of Fire', description := "Max HP +20%, HP regen per tick +20%, P.Atk. +10%, P.Def. +20%, Accuracy +4, Atk. Spd. +20%, Debuff Resistance +10%, Speed -10%."]
name[name == "Prophecy of Water", description := "Casting Spd. +20%, M.Def. +20%, M.Atk. +20%, MP regen per tick +20%, Magic Critical Rate +100%, Debuff Resistance +10%, Speed -20%."]
name[name == "Prophecy of Wind", description := "Atk Speed +20%, Vampyric Rage +5%, Critical Rate and Critical Damage from behind +20%, Accuracy +4, Evasion +4, Debuff Resistance +10%."]
### Fighteriu vienetiniai:
name[name == 'Fist Fury', description = 'Increases Atk. Spd. by 25%.']
name[name == 'Duelist Spirit' & level == 1, description := "Increases Atk. Spd. by 8% and PvP damage by 5%."]
name[name == 'Duelist Spirit' & level == 1, description := "Increases Atk. Spd. by 12% and PvP attack damage by 10%."]

name[name == 'Rapid Fire' & level == 1, description := "Increases P. Atk. by 62 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 2, description := "Increases P. Atk. by 67 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 3, description := "Increases P. Atk. by 73 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 4, description := "Increases P. Atk. by 78 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 5, description := "Increases P. Atk. by 83 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 6, description := "Increases P. Atk. by 89 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 7, description := "Increases P. Atk. by 94 and Atk. Spd. by 20%. Decreases range by 50%."]
name[name == 'Rapid Fire' & level == 8, description := "Increases P. Atk. by 100 and Atk. Spd. by 20%. Decreases range by 50%."]

name[name == 'Dead Eye' & level == 1, description := "Increases Accuracy by 1, P. Atk. by 124, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 2, description := "Increases Accuracy by 1, P. Atk. by 134, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 3, description := "Increases Accuracy by 2, P. Atk. by 145, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 4, description := "Increases Accuracy by 2, P. Atk. by 155, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 5, description := "Increases Accuracy by 2, P. Atk. by 166, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 6, description := "Increases Accuracy by 3, P. Atk. by 177, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 7, description := "Increases Accuracy by 3, P. Atk. by 188, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]
name[name == 'Dead Eye' & level == 8, description := "Increases Accuracy by 3, P. Atk. by 199, and Critical Damage by 20%. Decreases Atk. Spd. by 10%."]

name[name == 'Snipe' & level == 1, description := "Increases Accuracy by 3, P. Atk. by 124, range by 50, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 2, description := "Increases Accuracy by 3, P. Atk. by 134, range by 50, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 3, description := "Increases Accuracy by 4, P. Atk. by 145, range by 100, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 4, description := "Increases Accuracy by 4, P. Atk. by 155, range by 100, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 5, description := "Increases Accuracy by 5, P. Atk. by 166, range by 150, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 6, description := "Increases Accuracy by 5, P. Atk. by 177, range by 150, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 7, description := "Increases Accuracy by 6, P. Atk. by 188, range by 200, and Critical Rate by 20%. Movement is restricted."]
name[name == 'Snipe' & level == 8, description := "Increases Accuracy by 6, P. Atk. by 199, range by 200, and Critical Rate by 20%. Movement is restricted."]

name[name == 'Rapid Shot', description := description %>%
       str_replace_all(c("Increases speed of arrow launch. Effect 1." = "Increases bow Atk. Spd. by 8%.",
                         "Increases speed of arrow launch. Effect 2." = "Increases bow Atk. Spd. by 12%.",
                         "Effect 2." = "",
                         "Temporarily increases one's speed of arrow attack." = "Increases bow Atk. Spd. by 12%."))
     ]
### HS debufai
# name[name == 'Hot Springs Flu' & level == 1, description := "Increases Atk. Spd. by and decreases P. Atk. by "]

###
name %>% 
#write.table(file = "C:\\Users\\sarun_000\\Desktop 3\\skillname output.txt", quote = FALSE, col.names = FALSE, row.names = FALSE, sep = '\t', na = '')
write.table(file = "skillname output.txt", quote = FALSE, col.names = FALSE, row.names = FALSE, sep = '\t', na = '')
