###################################
#         CLEAN UP PROCESS        #
###################################

# Monsters of D&D
monsters <- read.csv("C:/Documents/datasheets/monsters.csv")

# Drop unnecessary columns
monster <- subset(monsters, select=-c(X,str_mod,dex_mod,con_mod,int_mod,wis_mod,cha_mod, 
                                      senses,str_save,dex_save,con_save,int_save,wis_save,cha_save,
                                      history,perception,stealth,persuasion,insight,deception,arcana,
                                      religion,acrobatics,athletics,
                                      intimidation,swim,fly,climb,burrow,languages,number_legendary_actions))

#### Languages (0 for no, 1 for yes) ####
monster$language <- ifelse(monsters$languages=="", 0, 1)

#### Legendary Beasts (0 for no, 1 for yes) ####
monster$legendary <- ifelse(monsters$number_legendary_actions==0, 0, 1)

# Optional: remove old monsters
rm(monsters)

#### Challenge Rating ####
convert_to_numeric <- function(x) {
  if (grepl("/", x)) {
    # If it's a fraction (like "1/4"), split and divide
    parts <- unlist(strsplit(x, "/"))
    return(as.numeric(parts[1]) / as.numeric(parts[2]))
  } else {
    # Otherwise, convert to numeric directly and format to two decimals
    return(as.numeric(x))}}

monster$cr <- format(monster$cr, nsmall = 2)
monster$cr <- sapply(monster$cr, convert_to_numeric)

summary(monster$cr)

#### Alignment ####
unique(monster$alignment)
#which(monster$alignment=="shapechanger)")
monster[which(monster$alignment=="shapechanger)"),c(1,2,3,4,5)]

monster$alignment[174] <- "lawful evil"; monster$monster_type[174] <- "Fiend (devil)"
monster$alignment[300] <- "neutral good"; monster$monster_type[300] <- "Humanoid (human shapechanger)"
monster$alignment[301] <- "neutral evil"; monster$monster_type[301] <- "Humanoid (human shapechanger)"
monster$alignment[302] <- "lawful evil"; monster$monster_type[302] <- "Humanoid (human shapechanger)"
monster$alignment[303] <- "neutral"; monster$monster_type[303] <- "Humanoid (human shapechanger)"
monster$alignment[304] <- "chaotic evil"; monster$monster_type[304] <- "Humanoid (human shapechanger)"

monster$alignment[159] <- "any alignment"

monster$alignment[66] <- "neutral good or neutral evil"

simplify_alignment <- function(alignment) {
  alignment <- tolower(alignment)  # ensure consistent casing
  
  case_when(
    alignment %in% c("lawful good")                      ~ "Lawful Good",
    alignment %in% c("neutral good")                     ~ "Neutral Good",
    alignment %in% c("chaotic good")                     ~ "Chaotic Good",
    alignment %in% c("lawful neutral")                   ~ "Lawful Neutral",
    alignment %in% c("neutral")                          ~ "True Neutral",
    alignment %in% c("chaotic neutral")                  ~ "Chaotic Neutral",
    alignment %in% c("lawful evil")                      ~ "Lawful Evil",
    alignment %in% c("neutral evil")                     ~ "Neutral Evil",
    alignment %in% c("chaotic evil")                     ~ "Chaotic Evil",
    alignment %in% c("unaligned")                        ~ "Unaligned",
    
    alignment %in% c("any alignment")                    ~ "Any",
    alignment %in% c("any evil alignment")               ~ "Evil (unspecified)",
    alignment %in% c("any chaotic alignment")            ~ "Chaotic (unspecified)",
    alignment %in% c("any non-good alignment")           ~ "Non-Good",
    alignment %in% c("any non-lawful alignment")         ~ "Non-Lawful",
    alignment %in% c("neutral good or neutral evil")     ~ "Neutral (Good or Evil)",
    
    TRUE ~ "Other"
  )
}

monster$alignment <- simplify_alignment(monster$alignment)

alignment_levels <- c(
  "Lawful Good", "Neutral Good", "Chaotic Good",
  "Lawful Neutral", "True Neutral", "Chaotic Neutral",
  "Lawful Evil", "Neutral Evil", "Chaotic Evil",
  "Unaligned", "Any", "Evil (unspecified)", "Chaotic (unspecified)",
  "Non-Good", "Non-Lawful", "Neutral (Good or Evil)", "Other"
)

monster$alignment <- factor(monster$alignment,
                                  levels = alignment_levels,
                                  ordered = TRUE)

#### Size ####
unique(monster$size)

#### Monster_type ####
unique(monster$monster_type)

#### AC and HP ####
summary(monster$ac)
summary(monster$hp)

which(monster$hp==676,)
monster[281,]

#### Ability Checks ####
summary(monster$strength)
summary(monster$dex)
summary(monster$con)
summary(monster$intel)
summary(monster$wis)
summary(monster$cha)

#### Speed ####
summary(monster$speed)


write.csv(monster, file="C:/Documents/Applied Stats/DATA 824 Data Viz/Assignments/Final project/Final-project/monsters_cleaned.csv", row.names = TRUE)