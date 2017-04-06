all_as_buffs <- FALSE
cat_available_at_buffer <- FALSE
show_cov <- FALSE
change_buff_icons_to_wc_ol_icons <- TRUE
ol_patch <- FALSE
# Input: skillgrp.txt
# Output: skillgrp output.txt
# 
library(dplyr)
library(data.table)
library(stringr)


grp <- data.table(read.table("skillgrp.txt", header = T, sep = '\t', quote = ""))
name <- data.table(read.table("skillname.txt", header = T, sep = '\t', quote = "")) %>% select(id, level, name)
merged <- data.table(right_join(grp, name, by = c('skill_id' = 'id', 'skill_level' = 'level')) %>% arrange(skill_id, skill_level))
name <- data.table(read.table("skillname.txt", header = T, sep = '\t', quote = ""))
setkey(merged, skill_id, skill_level)

# flip all buffs <-> debuff
merged[,extra_eff := as.numeric(!extra_eff)]
if(all_as_buffs) merged[,extra_eff := 0]

# keep important buffs as buffs.
buff_to_stay_buffs <- c(
  'Noblesse Blessing', 'Spell Force', 'Invocation'
  , 'Salvation', 'Soul of the Phoenix', 'Celestial Shield', 'Flames of Invincibility', 'Sonic Barrier', 'Force Barrier'
  ,'Ultimate Evasion', 'Ultimate Defense', 'Vengeance',  'Touch of Life'
  #, 'Physical Mirror', 'Magical Mirror'
  #, 'Aura Flare'
  #,'Arcane Agility', 'Arcane Wisdom', 'Riposte Stance', 'Parry Stance', 'Transfer Pain','Arcane Power'
  
  ,'Blessing of Protection','Lesser Healing Potion','Healing Potion','Greater Healing Potion', 'Greater Heal', 'Greater Group Heal'
  ,'Dash','Mirage','Stealth', 'Counterattack', 'Dodge', 'Blinding Blow'
  ,'Angelic Icon', 'Zealot', 'Frenzy', 'Guts'
  ,'Duelist Spirit', 'War Cry', 'Rage', 'Focus Death', 'Focus Chance', 'Focus Power',  'Dead Eye',  'Rapid Fire','Snipe'
  ,'Wolf Spirit Totem', 'Bear Spirit Totem', 'Puma Spirit Totem', 'Ogre Spirit Totem', 'Rabbit Spirit Totem', 'Bison Spirit Totem', 'Hawk Spirit Totem','Force Meditation'
  ,'Detect Animal Weakness', 'Detect Plant Weakness', 'Detect Beast Weakness', 'Eye of Slayer', 'Eye of Hunter'
  ,'Lionheart' 
  #,'Battle Roar', 'Thrill Fight'
  #,'Vicious Stance', 'War Frenzy', 'Accuracy', 'Focus Attack', 'Polearm Accuracy', 'Silent Move', 'Fake Death', 'Guard Stance', 'Soul Guard', 'Soul Cry','Focus Skill Mastery'
  #,'Rapid Shot', 'Deflect Arrow', 'Majesty'
  
  ,'AntiBuff-Shield','Block Buff', 'Buff Block'
  ,'Fear', 'Mass Fear', 'Punch of Doom'
)
if (cat_available_at_buffer){
  buff_to_stay_buffs <- c(buff_to_stay_buffs,'Blessing of Seraphim') # mp regen horse
  #buff_to_stay_buffs <- c(buff_to_stay_buffs,'Gift of Queen') # p.atk cat
} else {
  buff_to_stay_buffs <- c(buff_to_stay_buffs, 'Gift of Seraphim', 'Blessing of Queen')
}
if (show_cov){
  buff_to_stay_buffs <- c(buff_to_stay_buffs,'Chant of Victory', 'Prophecy of Water', 'Prophecy of Fire', 'Prophecy of Wind', "Victory of Pa'agrio", "Magnus\' Chant")
}

merged[merged$name %in% buff_to_stay_buffs,]$extra_eff <- 0 # merged[merged$name %in% buff_to_stay_buffs,] %>% select(extra_eff, name)
rm(buff_to_stay_buffs)
### HS debuff icons and and casting animations.
merged[name == 'Hot Springs Rheumatism' & skill_level == 4, icon_name := 'icon.skill1077 '] # Focus icon
merged[name == 'Hot Springs Rheumatism' & skill_level == 4, desc := 'skill.mu.4325']        # Focus animation
merged[name == 'Hot Springs Rheumatism' & skill_level != 4, desc := 'skill.sa.1007']        # Chant of ... animation

merged[name == 'Hot Springs Cholera' & skill_level == 4, icon_name := 'icon.skill1240']     # Guidance icon
merged[name == 'Hot Springs Cholera' & skill_level == 4, desc := 'skill.sub.1240']          # Guidance animation
merged[name == 'Hot Springs Cholera' & skill_level != 4, desc := 'skill.sa.1007']           # Chant of ... animation

merged[name == 'Hot Springs Flu' & skill_level == 4, icon_name := 'icon.skill1086']         # Haste icon
merged[name == 'Hot Springs Flu' & skill_level == 4, desc := 'skill.su.1086']               # Haste animation
merged[name == 'Hot Springs Flu' & skill_level != 4, desc := 'skill.sa.1007']               # Chant of ... animation

merged[name == 'Hot Spring Malaria' & skill_level == 4, icon_name := 'icon.skill1085']     # Acumen icon
merged[name == 'Hot Spring Malaria' & skill_level == 4, desc := 'skill.su.1085']           # Acumen animation
merged[name == 'Hot Spring Malaria' & skill_level != 4, desc := 'skill.sa.1007']           # Chant of ... animation

merged[name %in% c('Hot Springs Rheumatism', 'Hot Springs Cholera', 'Hot Springs Flu', 'Hot Spring Malaria') & skill_level == 4, extra_eff := 0] # 4lv => bufai
merged[name %in% c('Hot Springs Rheumatism', 'Hot Springs Cholera', 'Hot Springs Flu', 'Hot Spring Malaria') & skill_level != 4, extra_eff := 1] # ne 4lv => debufai

# Skill animation change
animation <- list(avatar = 'skill.mo.121', aura_burn = 'skill.el.1172', none = '')

merged[name == 'Major Group Heal', desc := animation$avatar]
merged[name == 'Major Heal', desc := animation$avatar]
merged[name == 'Greater Battle Heal', desc := animation$avatar]
merged[name == "The Honor of Pa'agrio", desc := animation$avatar]
merged[name == 'Seal of Gloom', desc := animation$none]
merged[name == 'Seal of Mirage', desc := animation$none]
merged[name == 'Seal of Chaos', desc := animation$none]
merged[name == 'Seal of Scourge', desc := animation$none]
merged[name == 'Seal of Flame', desc := animation$none]
merged[name == 'Aura Flare', desc := animation$aura_burn] # aura burn animation

merged[name == 'Guts', desc := 'skill.hero.395'] # Hero ud animation
merged[name == 'Frenzy', desc := 'skill.mu.4365']             # Hero bers animation
merged[name == 'Bison Spirit Totem', desc := 'skill.mu.4365'] # Hero bers animation
merged[name == 'Snipe', desc := 'skill.mu.4365']              # Hero bers animation
merged[name == 'Zealot', desc := 'skill.hero.396']            # Angelic Icon animation

merged[name == 'Item Skill: Celestial Shield', desc := 'skill.moment.443'] # Celestial shield animation
merged[name == 'Sonic Barrier', desc := 'skill.moment.443'] # Celestial shield animation
merged[name == 'Force Barrier', desc := 'skill.moment.443'] # Celestial shield animation
merged[grep('Refresh', name), desc := 'skill.ph.363'] # Song of champion animation
rm(animation)

## Augmentu ikonos
merged[grep('Item Skill: ', name), extra_eff := 0] # visi augmentai - bufai.

### buff tipo augmentu ikonos:
merged[name == 'Item Skill: Duel Might',      icon_name := 'icon.skill0297'] # Duelist Spirit
merged[name == 'Item Skill: Empower',         icon_name := 'icon.skill1059'] # Greater Empower icon
merged[name == 'Item Skill: Wild Magic',      icon_name := 'icon.skill1303'] # Wild Magic icon
merged[name == 'Item Skill: Shield',          icon_name := 'icon.skill1040']
merged[name == 'Item Skill: Magic Barrier',   icon_name := 'icon.skill1036']
merged[name == 'Item Skill: Stealth',         icon_name := 'icon.skill0411']
merged[name == 'Item Skill: Spell Refresh',   icon_name := 'icon.skill0164'] # Quick Recovery icon
merged[name == 'Item Skill: Skill Refresh',   icon_name := 'icon.skill0364'] # Song of Champion icon
merged[name == 'Item Skill: Refresh',         icon_name := 'icon.skill0349'] # Song of Renewal icon
merged[name == 'Item Skill: Might',           icon_name := 'icon.skill1068'] # Might
merged[name == 'Item Skill: Focus',           icon_name := 'icon.skill1077'] # Focus
merged[name == 'Item Skill: Blessed Body',    icon_name := 'icon.skill1045']
merged[name == 'Item Skill: Battle Roar',     icon_name := 'icon.skill0121']
merged[name == 'Item Skill: Prayer',          icon_name := 'icon.skill1307']
merged[name == 'Item Skill: Blessed Soul',    icon_name := 'icon.skill1048']
merged[name == 'Item Skill: Mana Gain',       icon_name := 'icon.skill3080']
merged[name == 'Item Skill: Agility',         icon_name := 'icon.skill1087']
merged[name == 'Item Skill: Guidance',        icon_name := 'icon.skill1240']
merged[name == 'Item Skill: Vampiric Rage',   icon_name := 'icon.skill1268']
merged[name == 'Item Skill: Reflect Damage',  icon_name := 'icon.skill0086']
merged[name == 'Item Skill: Weight Limit',    icon_name := 'icon.skill0150']
### debuff tipo augmentu ikonos:
merged[name == 'Item Skill: Slow',            icon_name := 'icon.skill1160']
merged[name == 'Item Skill: Winter',          icon_name := 'icon.skill1206']
merged[name == 'Item Skill: Stun',            icon_name := 'icon.skill0092']
merged[name == 'Item Skill: Hold',            icon_name := 'icon.skill1201']
merged[name == 'Item Skill: Sleep',           icon_name := 'icon.skill1069']
merged[name == 'Item Skill: Paralyze',        icon_name := 'icon.skill1170']
merged[name == 'Item Skill: Medusa',          icon_name := 'icon.skill0367']
merged[name == 'Item Skill: Fear',            icon_name := 'icon.skill1169']
merged[name == 'Item Skill: Poison',          icon_name := 'icon.skill1168']
merged[name == 'Item Skill: Bleed',           icon_name := 'icon.skill0096']
merged[name == 'Item Skill: Silence',         icon_name := 'icon.skill1064']
merged[name == 'Item Skill: Doom',            icon_name := 'icon.skill1336']
merged[name == 'Item Skill: Duel Weakness',   icon_name := 'icon.skill0116']

### mobu apibudinimo skilu ikonos
# merged[name == 'Extremely Weak P. Atk.',    icon_name := ''] 
merged[name == 'Very Weak P. Atk.',         icon_name := 'icon.skill0298'] # Rabit Spirit Totem
merged[name == 'Weak P. Atk.',              icon_name := 'icon.skill0115'] # Power Break
merged[name == 'Slightly Weak P. Atk.',     icon_name := 'icon.skill1164'] # Curse Weakness
# merged[name == 'Average P. Atk.',         icon_name := ''] # stays the same.
merged[name == 'Slightly Strong P. Atk.',   icon_name := 'icon.skill0271'] # Dance of Warrior
merged[name == 'Strong P. Atk.',            icon_name := 'icon.skill1068'] # Might icon
merged[name == 'Very Strong P. Atk.',       icon_name := 'icon.skill1388'] # Greater Might icon
merged[name == 'Extremely Strong P. Atk.',  icon_name := 'icon.skill0176'] # Frenzy icon
# merged[name == 'Ultra Strong P. Atk.',      icon_name := '']


# merged[name == 'Extremely Weak M. Atk.',      icon_name := '']
merged[name == 'Very Weak M. Atk.',           icon_name := 'icon.skill1248'] # Seal of Suspension
merged[name == 'Weak M. Atk.',                icon_name := 'icon.skill1064'] # Silence
merged[name == 'Slightly Weak M. Atk.',       icon_name := 'icon.skill1351'] # Mage Bane
# merged[name == 'Average M. Atk.',    icon_name := ''] # stays the same
merged[name == 'Slightly Strong M. Atk.',     icon_name := 'icon.skill0273'] # Dance of Mystic
merged[name == 'Strong M. Atk.',              icon_name := 'icon.skill0378'] # Empower
merged[name == 'Very Strong M. Atk.',         icon_name := 'icon.skill1059'] # The Soul of Pa'agrio
# merged[name == 'Extremely Strong M. Atk.',    icon_name := 'icon.etc_herb_blue_i00'] # M. Atk herb
# merged[name == 'Ultra Strong M. Atk.',    icon_name := '']


# merged[name == 'Extremely Weak P. Def.',    icon_name := '']
merged[name == 'Very Weak P. Def.',        icon_name := 'icon.armor_t55_u_i00'] # Devotion Set
merged[name == 'Weak P. Def.',             icon_name := 'icon.skill0362'] # Armor Crush
merged[name == 'Slightly Weak P. Def.',    icon_name := 'icon.skill0122'] # Hex
# merged[name == 'Average P. Def.',    icon_name := '']
merged[name == 'Slightly Strong P. Def.',  icon_name := 'icon.skill1040'] # Shield
merged[name == 'Strong P. Def.',           icon_name := 'icon.skill1389'] 
merged[name == 'Very Strong P. Def.',      icon_name := 'icon.skill1283'] # Soul Guard
merged[name == 'Extremely Strong P. Def.', icon_name := 'icon.skill0139'] # Guts
# merged[name == 'Ultra Strong P. Def.',    icon_name := '']

# merged[name == 'Extremely Weak M. Def.',    icon_name := '']
merged[name == 'Very Weak M. Def.',         icon_name := 'icon.skill0361'] # Shock Blast
merged[name == 'Weak M. Def.',              icon_name := 'icon.skill1263'] # Curse Gloom
merged[name == 'Slightly Weak M. Def.',     icon_name := 'icon.skill1062'] # Berserker Spirit
# merged[name == 'Average M. Def.',    icon_name := '']
merged[name == 'Slightly Strong M. Def.',   icon_name := 'icon.skill0146'] # Anti Magic
merged[name == 'Strong M. Def.',            icon_name := 'icon.skill1036'] # Magic Barrier
merged[name == 'Very Strong M. Def.',       icon_name := 'icon.skill1352'] # Elemental Protection
merged[name == 'Extremely Strong M. Def.',  icon_name := 'icon.skill0368'] # Vengeance
# merged[name == 'Ultra Strong M. Def.',    icon_name := '']

merged[skill_id == 4085, icon_name := 'icon.skill1242'] # skill <critical power>.icon = <death whisper>.icon
merged[skill_id == 4086, icon_name := 'icon.skill1077'] # skill <critical chance>.icon = <focus>.icon
merged[skill_id == 4275, icon_name := 'icon.skill1043'] # skill <Sacred Attack Weak Point>.icon = <Holy Weapon>.icon
merged[skill_id == 4333, icon_name := 'icon.skill1393'] # skill <Resist Dark Attack>.icon = <Unholy Resistance>.icon
#merged[skill_id == 4086, icon_name := 'icon.skill1077'] # skill <critical chance>.icon = <focus>.icon
#merged[skill_id == 4086, icon_name := 'icon.skill1077'] # skill <critical chance>.icon = <focus>.icon
#merged[skill_id == 4086, icon_name := 'icon.skill1077'] # skill <critical chance>.icon = <focus>.icon
merged[skill_id == 4278, icon_name := 'icon.skill0070'] # skill <dark attack>.icon = <Drain Health>.icon


# PRP bufu ikonos pakeistos i atitinkamu ol bufu ikonas
if (change_buff_icons_to_wc_ol_icons){
  merged[skill_id == 1085, icon_name := 'icon.skill1004'] # Acumen
  merged[skill_id == 1059, icon_name := 'icon.skill1365'] # Empower
  merged[skill_id == 1062 & skill_level >= 2, icon_name := 'icon.skill1261'] # Berserker Spirit # bers1 paliekam ta pacia ikona.
  merged[skill_id == 1204, icon_name := 'icon.skill1282'] # Wind Walk
  merged[skill_id == 1036, icon_name := 'icon.skill1008'] # Magic Barrier
  merged[skill_id == 1040 & skill_level >= 3, icon_name := 'icon.skill1005'] # Shield
  merged[skill_id == 1389, icon_name := 'icon.skill1391'] # Greater Shield
  
  merged[skill_id == 1388, icon_name := 'icon.skill1390'] # Greater Might
  merged[skill_id == 1268, icon_name := 'icon.skill1310'] # Vampiric Rage
  merged[skill_id == 1068, icon_name := 'icon.skill1007'] # Might
  merged[skill_id == 1086, icon_name := 'icon.skill1251'] # Haste
  merged[skill_id == 1077, icon_name := 'icon.skill1308'] # Focus
  merged[skill_id == 1242, icon_name := 'icon.skill1253'] # Death Whisper
}

# ol_patch
if (ol_patch){
  acumen_empower_bufai <- c(
    c('Acumen', "The Wisdom of Pa'agrio", 'Scroll of Greater Acumen', 'Magic Haste Potion', "Herb of Casting Spd.", "Scrol of Acumen - Event Use", "Acumen for Beginners", "Acumen", "Pet Acumen", "Golden Pig Acumen", "Master's Blessing - Acumen")
    ,c('Empower', 'Greater Empower', 'Bright Servitor', "The Soul of Pa'agrio", 'Scroll of Mystic Empower', 'Herb of Magic', 'Scroll of Empower - Event Use','Empower for Beginners','Holiday Empower', "Master's Blessing - Empower")
    ,c('Sprint', 'Wind Walk', "Pa'agrian Haste", "Haste Potion", "Greater Haste Potion", "Scroll of Wind Walk", "Herb of Speed", "Scroll of Wind Walk - Event Use", "Wind Walk for Beginners", "Pet Wind Walk", "Master's Blessing - Wind Walk")
  )
  merged[merged$name %in% acumen_empower_bufai,]$extra_eff <- 0 # merged[merged$name %in% buff_to_stay_buffs,] %>% select(extra_eff, name)
  rm(acumen_empower_bufai)
}
# _output skillgrp.txt
merged %>% select(-name) %>%
  #write.table(file = "C:\\Users\\sarun_000\\Desktop 3\\skillgrp output.txt", quote = FALSE, col.names = FALSE, row.names = FALSE, sep = '\t', na = '')
  write.table(file = "_output skillgrp.txt", quote = FALSE, col.names = FALSE, row.names = FALSE, sep = '\t', na = '')

#TBD:
#
# corect writing to file ? maybe done ?

