alpha <- FALSE
#  ------------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
unchanged <- sysmsg <- read_delim("systemmsg-e.txt", '\t', col_types = list(col_integer(), col_integer(), col_character(), col_integer(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_integer(), col_integer(), col_integer(), col_integer(), col_integer(), col_character(), col_character()))
if (alpha) sysmsg <- sysmsg %>% mutate(message = paste0('<',id, '>', message))
# Functions ---------------------------------------------------------------
find_id <- function(string) {
  Result <- unchanged %>% filter(str_detect(str_to_lower(message),str_to_lower(string))) %>% .[["id"]]
  if (length(Result) == 0) warning(paste0('Could not find lines with string "', string, '".'), call. = FALSE)
  return(Result)
}
change_color <- function(vID = NULL, value = NULL){
  if(!is.null(value)){
    if(is.null(vID)) vID <- sysmsg$id
    if(is.character(vID)) vID <- find_id(vID)
    value <- value %>% str_replace('0x', '') %>% str_to_upper()
    if (str_detect(value, '[^[0-9A-F]]')) stop('Values overreach hexadecimal maximum [0-9A-F]')# yra neleistinu simboliu
    
    if (str_length(value) < 6) stop('String doesnt reach length of 6, minimum requirement.')
    if (str_length(value) > 6) warning('String exceeds length of 6.')
    sysmsg <<- sysmsg %>% mutate(`rgba[0]` = replace(`rgba[0]`, id %in% vID, str_sub(value, 5, 6)))
    sysmsg <<- sysmsg %>% mutate(`rgba[1]` = replace(`rgba[1]`, id %in% vID, str_sub(value, 3, 4)))
    sysmsg <<- sysmsg %>% mutate(`rgba[2]` = replace(`rgba[2]`, id %in% vID, str_sub(value, 1, 2)))
  }
  return(vID)
}
change_sound <- function(vID = NULL, value = NULL){
  if(!is.null(value)){
    if(is.null(vID)) vID <- sysmsg$id
    if(is.character(vID)) vID <- find_id(vID)
    sysmsg <<- sysmsg %>% mutate(item_sound = replace(item_sound, id %in% vID, value))
  }
  return(value)
}
change_text <- function(vID = NULL, value = NULL){
  if(!is.null(value)){
    if(is.null(vID)) vID <- sysmsg$id
    if(is.character(vID)) vID <- find_id(vID)
    if(value == '') change_sound(vID, '')
    sysmsg <<- sysmsg %>% mutate(message = replace(message, id %in% vID, value))
    
  }
  return(vID)
}
change_group <- function(vID = NULL, value = NULL) {
  # 0,1,2  - system
  # 3      - damage
  # 4      - table [ok][ok/cancel]
  # 5      - damage and consumables
  # 6      - petition
  # 7      - consumables
  if(!is.null(value)){
    if(is.null(vID)) vID <- sysmsg$id
    if(is.character(vID)) vID <- find_id(vID)
    if (class(value)=='character'){
      # system = 0
      # damage = 3
      # consumables = 7
      value <- switch(str_sub(str_to_lower(value),1,1),s = 0,d = 3,t = 4,b = 5,p = 6,c = 7,stop('Not supported variable group value.'))
    } else {
      if (class(value)=='numeric'){
        if(value > 7 || value < 0) stop('Value out of supported range.')
      } else
        stop(paste0('Class of value not supported. class = ', class(value), ', value = ', value))
    }
    sysmsg <<- sysmsg %>% mutate(group = replace(group, id %in% vID, value))
  }
  return(vID)
}
change_on_screen <- function(vID = NULL, value = NULL, pos = NULL, time = 0, do_fadeout = 0){
  if(!is.null(value)){
    if(is.null(vID)) vID <- sysmsg$id
    if(is.character(vID)) vID <- find_id(vID)
    do_fadeout <- as.numeric(do_fadeout)
    sysmsg <<- sysmsg %>% mutate(sub_msg = replace(sub_msg, id %in% vID, value))
    sysmsg <<- sysmsg %>% mutate(`UNK_1[0]` = replace(`UNK_1[0]`, id %in% vID, pos))
    sysmsg <<- sysmsg %>% mutate(`UNK_1[2]` = replace(`UNK_1[2]`, id %in% vID, time))
    sysmsg <<- sysmsg %>% mutate(`UNK_1[3]` = replace(`UNK_1[3]`, id %in% vID, do_fadeout))
  }
}
change_line <- function(vID = NULL, text = NULL, group = NULL, color = NULL){
  change_text(vID, text)
  change_group(vID, group)
  change_color(vID, color)
}
find_line <- function(string) {
  if(is.numeric(string)){
    ind <- string 
  } else
    ind <- find_id(string)
  sysmsg %>% filter(id %in% ind)
}
# Default colours ---------------------------------------------------------
## Uninportant
uninportant <- '222222'
grey <- '333333'
red <- 'FF0000'
redish <- '661111'
green <- '00FF00'
blue <- '0000FF'

drop <- 'FFFF00'
adena <- drop
spoil <- 'cc9900'
enchant <- drop
# other
  expsp <- '999900'
  party <- '5500AA'
  clan <- '000000'
players <- '12569F'
  trade <- green
  shop <- green
## Fighting       
###
  equip <- '59A655'
  equip_off <- '101010'
important_things <- '555500'
white <- 'FFFFFF'
### Damage
kibo_debufas <- '11dd41'
damage_out <- '4444dd'
damage_in <- 'FF0000'
### HP/MP/CP
mp_recovery <- '2E8AB6'
mp_lackof <- '0000FF'
hp_recovery <- 'B6772E'
hp_lackof <- 'FF0000'
### buffs
buff_on <- '333366'
buff_off <- '660033'
# Changes -----------------------------------------------------------------
### Server stuff
   1 %>% change_line('The server will be coming down in $s1 second(s).  Please find a safe place to log out.', 'system', red)
   3 %>% change_line('$s1 is not currently logged in.', 'system', red)
  94 %>% change_line("swxkatu", 's', redish) # Welcome to the L2NEO! www.l2neo.com
 101 %>% change_line("You cannot exit while in combat.", 's', red)
 102 %>% change_line("You cannot restart while in combat.", 's', red)
 125 %>% change_line("Exit?") # Do you wish to exit the game?
 126 %>% change_line("Restart?") # Do you wish to exit to the character select screen?
 127 %>% change_line("You have been disconnected from the server.") # You have been disconnected from the server. Please login again.
 176 %>% change_line("That person is in message refusal mode.", 's', red)
 214 %>% change_line("Your title has been changed.", 's', red) %>% change_on_screen(value = 'Your title has been changed.', pos = 5, time = 2, do_fadeout = 0)
### Remove
  34 %>% change_line("") # Welcome to the World of Lineage II.
  41 %>% change_line("") # You carefully nock an arrow.
  47 %>% change_line("") # You begin to use a(n) $s1.
  48 %>% change_line("") # $s1 is not available at this time: being prepared for reuse.
 109 %>% change_line("") # Invalid target.
 110 %>% change_line("") # [+] $s1  # The effects of $s1 flow through you.
 144 %>% change_line("") # That is the incorrect target.
 342 %>% change_line("") # Power of the spirits enabled.
 533 %>% change_line("") # Power of Mana enabled.
1017 %>% change_line("") # Pet's critical hit!
1028 %>% change_line("") # Summoned monster's critical hit!
### OnScreen only
 181 %>% change_line("", color = red) %>% change_on_screen(value = "Can't see.", pos = 7, do_fadeout = 0, time = 1) # Cannot see target.
### Pick
  28 %>% change_line('You have obtained $s1 adena.', 's', adena)
  29 %>% change_line('You have obtained $s2 $s1.', 's', drop)
  30 %>% change_line('You have obtained $s1.', 's', drop)
  52 %>% change_line("You have earned $s1 adena.", 's', adena)
  53 %>% change_line("You have earned $s2 $s1(s).", 's', drop)
  54 %>% change_line("You have earned $s1.", 's', drop)
 608 %>% change_line("$s1 has obtained $s3 $s2 by using Sweeper.", 's', drop)
  55 %>% change_line("You have failed to pick up $s1 adena.", 'c', red)
  56 %>% change_line("You have failed to pick up $s1.", 'c', red)
  57 %>% change_line("You have failed to pick up $s2 $s1(s).", 'c', red)
  58 %>% change_line("You have failed to earn $s1 adena.", 'c', red)
  59 %>% change_line("You have failed to earn $s1.", 'c', red)
  60 %>% change_line("You have failed to earn $s2 $s1(s).", 'c', red)
  62 %>% change_line("Your $s1 has been successfully enchanted.", 's', enchant)
  63 %>% change_line("Your +$S1 $S2 has been successfully enchanted.", 's', enchant)
  64 %>% change_line("The enchantment has failed!  Your $s1 has been crystallized.", 's', red)
  65 %>% change_line("The enchantment has failed!  Your +$s1 $s2 has been crystallized.", 's', red)
 277 %>% change_line("You have learned $s1.", 's', important_things)
 298 %>% change_line("You have dropped $s1.", 's', red)
 299 %>% change_line("$s1 has obtained $s3 $s2.", 's', drop)
 300 %>% change_line("$s1 has obtained $s2.", 's', drop)
 369 %>% change_line("You have obtained a +$s1 $s2.", 's', drop)
 375 %>% change_line("You have dropped +$s1 $s2.", 's', red)
 376 %>% change_line("$s1 has obtained +$s2$s3.", 's', drop)
 370 %>% change_line("Failed to pick up $s1.", 's', redish)
 371 %>% change_line("Acquired +$s1 $s2.", 's', drop)
 372 %>% change_line("Failed to earn $s1.", 's', redish)
 608 %>% change_line("$s1 spoiled $s3 $s2.", 's', spoil) # $s1 has obtained $s3 $s2  by using Sweeper.
 609 %>% change_line("$s1 spoiled $s2.", 's', spoil) # $s1 has obtained $s2 by using Sweeper.
 672 %>% change_line("$s1 adena disappeared.", 'c') 
### EXP SP
  45 %>% change_line("You have earned $s1 experience.", 'c', expsp)
  95 %>% change_line("You have earned $s1 experience and $s2 SP.", 'c', expsp)
 331 %>% change_line("You have acquired $s1 SP.", 'c', expsp)
 362 %>% change_line("You have acquired $s1 bonus experience from a successful over-hit.", 'c', expsp)
### Trade
 121 %>% change_line("$s1 has confirmed the trade.", 's', color = green)
 123 %>% change_line("Your trade is successful.", 's', color = green)
 124 %>% change_line("$s1 has canceled the trade.", 's', color = red)
 142 %>% change_line("You are already trading with someone.", 's', color = redish)
 143 %>% change_line("$s1 is already trading with another person. Please try again later.", 's', color = redish)
 174 %>% change_line("That person's inventory is full.", 's', color = red)
### Players
 131 %>% change_line("$s1 has logged in.", 's', players)
 145 %>% change_line("That player is not online.", 's', players)
 503 %>% change_line("$s1 (Friend) has logged in.", 's', players)
1359 %>% change_line("$s1 has logged in.", 's', players)
### Party
 105 %>% change_line("You have invited $s1 to your party.", 's', party)
 106 %>% change_line("You have joined $s1's party.", 's', party)
 107 %>% change_line("$s1 has joined the party.", 's', green)
 108 %>% change_line("$s1 has left the party.", 's', red)
 152 %>% change_line("You have invited the wrong target.", 's', redish)
 153 %>% change_line("$s1 is busy. Please try again later.", 's', redish)
 154 %>% change_line("Only the leader can give out invitations.", 's', redish)
 155 %>% change_line("The party is full.", 's', red)
 160 %>% change_line("$s1 is a member of another party and cannot be invited.", 's', red)
 161 %>% change_line("That player is not currently online.", 's', redish)
 164 %>% change_line("Waiting for another reply.", 's', party)
 185 %>% change_line("You must first select a user to invite to your party.", 's', redish)
 200 %>% change_line("You have withdrawn from the party.", 's', party)
 201 %>% change_line("$s1 was expelled from the party.", 's', party)
 202 %>% change_line("You have been expelled from the party.", 's', party)
 203 %>% change_line("The party has dispersed.", 's', party)
 305 %>% change_line("The player declined to join your party.", 's', party)
1384 %>% change_line("$s1 has become the party leader.", 's', party)
### Clan
 191 %>% change_line("Clan member $s1 has been expelled.", 's', red)
 193 %>% change_line("Clan has dispersed.", 's', red)
 222 %>% change_line("$s1 has joined the clan.", 's', green)
 223 %>% change_line("$s1 has withdrawn from the clan.", 's', red)
 304 %>% change_line("Clan member $s1 has logged into game.", 'c', players)
### Damage
  35 %>% change_line('You hit for $s1 damage.', 'd', damage_out)
  36 %>% change_line('$s1 hit you for $s2 damage.', 'd', damage_in)
  37 %>% change_line('$s1 hit you for $s2 damage.', 'd', damage_in)
1866 %>% change_line('MP reduced by $s1.', 's', damage_in) # MP was reduced by $s1.
 296 %>% change_line('You received $s1 damage from taking a high fall.', 'd', damage_in)
 297 %>% change_line('You have taken $s1 damage because you were unable to breathe.', 'd', damage_in)
 333 %>% change_line("You have received $s1 damage by Core's barrier.", 'd', damage_in)
 812 %>% change_line('The secret trap has inflicted $s1 damage on you.', 'd', damage_in)
1014 %>% change_line('Your pet gained $s1 experience points.', 'c', white)
1015 %>% change_line('Your pet hit for $s1 damage.', 'd', damage_out)
1016 %>% change_line('Your pet received $s2 damage caused by $s1.', 'd', damage_in)
1026 %>% change_line('The summoned monster gave damage of $s1.', 'd', damage_out)
1027 %>% change_line('The summoned monster received damage of $s2 caused by $s1.', 'd', damage_in)
1069 %>% change_line('$s2 MP by $s1.', 'd', mp_recovery) # $s2 MP has been restored by $s1.

### Fails
  22 %>% change_line('Your target is out of range.', 's', white)
  23 %>% change_line('Not enough HP.', 'd', hp_lackof)
  24 %>% change_line('Not enough MP.', 'd', mp_lackof)
  43 %>% change_line("You have missed.", 'd', white)
  50 %>% change_line("Your target cannot be found.", 'd', red)
  51 %>% change_line("You cannot use this on yourself.", 's', red)
 112 %>% change_line("You have run out of arrows.", 's', red)
 113 %>% change_line("$s1 cannot be used due to unsuitable terms.", 'd', red)
 156 %>% change_line("Drain was only 50 percent successful.", 'd', red)
 129 %>% change_line("Your inventory is full.", 's', red)
 130 %>% change_line("Your warehouse is full.", 's', red)
 
  27 %>% change_line('Your casting has been interrupted.', 'd', important_things)
 139 %>% change_line("$s1 has resisted your $s2.", 'd', red) 
 158 %>% change_line("Your attack has failed.", 'd', red) 
1597 %>% change_line("$s1 failed.", 'd', red)
### Successes 

1066 %>% change_line('+$s1 HP.', 's', hp_recovery) # $s1 HP has been restored.
1405 %>% change_line('+$s1 CP.', 's', hp_recovery) # $s1 CPs have been restored.

  25 %>% change_line('Rejuvenating HP.', 'c', white)
  26 %>% change_line('Rejuvenating MP.', 'c', white)
  42 %>% change_line("You have avoided $s1's attack.", 'c', white)
  44 %>% change_line("Critical hit!", 'd', white)
1280 %>% change_line("Magic Critical Hit!", 'd', white)
  96 %>% change_line("Your level has increased!", 's', expsp)
 111 %>% change_line("Your shield defense has succeeded.", 's', white)
 157 %>% change_line("You resisted $s1's drain.", 'd', green)
 159 %>% change_line("You have resisted $s1's magic.", 'd', green)
 361 %>% change_line("Over-hit!", 'c', white)
 
 357 %>% change_line("It has already been spoiled.", 'd', green)
 612 %>% change_line("Spoil activated!", "d", kibo_debufas) %>% change_on_screen(value = "active!", pos = 8, time = 1)
1595 %>% change_line("$s1 has succeeded.", "d", kibo_debufas) %>% change_on_screen("$s1 on!", pos = 8, time = 1)
### Equip
  49 %>% change_line("You have equipped your $s1.", 's', equip)
 368 %>% change_line("Equipped +$s1 $s2.", 's', equip)
 417 %>% change_line("$s1  has been disarmed.", 's', equip)
 377 %>% change_line("$S1 $S2 disappeared.", 's', grey)
1064 %>% change_line("The equipment, +$s1 $s2, has been removed.", 's', equip_off)
1433 %>% change_line('$s1 activated.', 's', equip) # The automatic use of $s1 has been activated.
1434 %>% change_line('$s1 deactivated.', 's', equip_off) # The automatic use of $s1 has been deactivated.
### Zariche
1815 %>% change_line("$s2 was dropped in the $s1 region.", 'c', white) %>% change_on_screen(value = '$s2 dropped $s1.', pos = 8, time = 3)
1816 %>% change_line("The owner of $s2 has appeared in the $s1 region.", 'c', white) %>% change_on_screen(value = '$s2 in $s1.', pos = 8, time = 3)
1817 %>% change_line("$s2's owner has logged into the $s1 region.", 'c', white) %>% change_on_screen(value = '$s2 logged in $s1.', pos = 8, time = 3)
1818 %>% change_line("$s1 has disappeared.", 'c', white) %>% change_on_screen(value = '$s1 disappeared.', pos = 8, time = 3)
1819 %>% change_line("An evil is pulsating from $s2 in $s1.", 'c', white) %>% change_on_screen(value = '$s2 at $s1.', pos = 8, time = 3)
1820 %>% change_line("$s1 is currently asleep.", 'c', white) %>% change_on_screen(value = '$s1 sleeping.', pos = 8, time = 3)
1821 %>% change_line("$s2's evil presence is felt in $s1.", 'c', white) %>% change_on_screen(value = '$s2 at $s1.', pos = 8, time = 3)
1822 %>% change_line("$s1 has been sealed.", 'c', white) %>% change_on_screen(value = '$s1 sealed.', pos = 8, time = 3)
### Special cases
  46 %>% change_line("You use $s1.", 'c', white)
  92 %>% change_line("[-] $s1", 'c', buff_off) # $s1 has worn off.
1068 %>% change_line("$s1 MP", 'c', mp_recovery) # $s1 MP has been restored.

 104 %>% change_line("You may not equip items while casting or performing a skill.", 's', redish)
 301 %>% change_line("$s2 $s1 has disappeared.", 'c')
 302 %>% change_line("$s1 has disappeared.", 'c')
 303 %>% change_line("Select item to enchant.", 'c')
 323 %>% change_line("Your force has increased to $s1 level.", 'c', white)
 324 %>% change_line("Your force has reached maximum capacity.", 'c', white)
 343 %>% change_line("Sweeper failed, target not spoiled.", 's', redish)
 347 %>% change_line("Incorrect item count.", 's', red)
 351 %>% change_line("Incorrect item count.", 's', red)
 419 %>% change_line("$s1 minute(s) of usage time left.", 's', red)


### Unchanged
 614 %>% change_line(">614<$s1 $s2")
1995 %>% change_line("$s1 channel filtering option")
1260 %>% change_line("Seven Signs: Preparations have begun for the next quest event.", 's')
1433 %>% change_line("Auto $s1 has been activated.", 'c', grey)

# Ungrouped ---------------------------------------------------------------
1204 %>% change_line("The screenshot has been saved. ($s1 $s2x$s3)", 's', white)
1208 %>% change_line("$s1 died and dropped $s3 $s2.", 'c', important_things)
1209 %>% change_line("Congratulations. Your raid was successful.", 's', important_things)
1134 %>% change_line("HP increased. Effect of $s1 no longer active.", 's') # Since HP has increased, the effect of $s1 will disappear.
85 %>% change_line("", 'd', red) %>% change_on_screen("Peaceful zone.", 8, 1, 0) # You may not attack this target in a peaceful zone.

if(FALSE){
  # work stuff --------------------------------------------------------------
  i <- 1280
  sysmsg[i <- i+1,] %>% select(id, message, group, sub_msg); paste0(i-1, ' %>% change_line("',sysmsg[[i,'message']],'", \'\', )') %>% writeClipboard()
  sysmsg[i,'message']
  sysmsg %>% filter(str_detect(message, 'by using Sweeper')) %>% select(id, message)
  # end ---------------------------------------------------------------------
  anti_join(sysmsg, unchanged) %>% inner_join(unchanged, by = "id") %>% select(id, old = message.y, new = message.x)
}


sysmsg %>% 
  write.table(file = "_output sysmsg-e.txt", quote = FALSE, col.names = TRUE, row.names = FALSE, sep = '\t', na = '')

