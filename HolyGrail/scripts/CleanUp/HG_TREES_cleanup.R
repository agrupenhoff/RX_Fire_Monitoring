
library(tidyverse)
library(tibble)
library(dplyr)


HG_trees <- read.csv("HolyGrail/data/clean/HG_Trees_final.csv")

##MAKE SURE ALL TREE SPECIES CODES ARE CORRECT
HG_trees$species <- toupper(HG_trees$species) #make all codes uppercase

#recode everything to USDA plant list
unique(HG_trees$species)
str(HG_trees)

HG_trees$species <- recode(HG_trees$species, "PIMONO" = "PIMO1", 
                                             "QBMA" = "ABMA",
                                             "UNK" = "UNKNOWN",
                                             "ADCO" = "ABCO",
                              "ACEMAC" = "ACMA3",
                              "CONU" = "CONU4",
                              "POTR" = "POTR5",
                              "PIMO" = "PIMO2",
                              "DEAD " = "UNKDEAD",
                              "AMBA" = "ABMA",
                              "POTR" = "POTR5",
                              "ARBMEN" = "ARME",
                              "PISME" = "PSME",
                              "UNK SNAG" = "UNKDEAD",
                              "PÌPO" = "PIPO",
                              "POINCON" = "PICO",
                              "ALNINC" = "ALIN2",
                              "UNKNCONIFER" = "UNKNOWN",
                              "UNKCONIFER" = "UNKNOWN",
                              "UNKDEAD" = "UNKNOWN",
                              "FABMA" = "ABMA",
                              "CADE" = "CADE27",
                              " " = "UNKNOWN",
                              "QU LOBED" = "QUERCUS",
                              "QU ENTIRE" = "QUERCUS",
                           "QU ENTIRE " = "QUERCUS",
                              "YP" = "PINUS",
                              "OTHER" = "UNKNOWN",
                              "TSME" = "TSUMER",
                           "NA" = "UNKNOWN",
                           "FIR" = "ABIES",
                           "PINE" = "PINUS")

HG_trees_clean <- HG_trees %>% 
  mutate_if(is.character, list(~na_if(.,"")))

#recode tree status to be same
unique(HG_trees_clean$status)
HG_trees_clean$status <- toupper(HG_trees_clean$status) #make all codes uppercase
HG_trees_clean$status <- recode(HG_trees_clean$status, "DEAD " = "DEAD",
                                  "D"= "DEAD",
                                  "L" ="LIVE",
                                  "D3"= "DEAD",
                                "D2"= "DEAD",
                                "D1"= "DEAD",
                                "LIVE "= "LIVE",
                                "LUVE"= "LIVE",
                                "I" = "LIVE",     #infested but live, caples
                                "M" = "LIVE",     #marginal crown but live, caples
                                "no" = "NA")

unique(HG_trees_clean$status)

HG_trees_clean_final <- HG_trees_clean %>% 
  filter(status != "FT",
         status != "FNT",
         status != "F",
         status != "STUMP",
         status != "NO",
         status != "NA",
         status != "C")

unique(HG_trees_clean_final$status)
#change plotid stuff (if need be) HERE


write.csv(HG_trees_clean_final, "HolyGrail/data/clean/trees/HG_Trees_final.csv")

