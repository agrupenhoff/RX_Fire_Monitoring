
library(tidyverse)
library(tibble)
library(dplyr)

getwd()
HG_trees <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_Trees.csv")

##MAKE SURE ALL TREE SPECIES CODES ARE CORRECT
HG_trees$species <- toupper(HG_trees$species) #make all codes uppercase

#recode everything to USDA plant list
unique(HG_trees$species)
str(HG_trees)

HG_trees$species <- recode(HG_trees$species, 
                                              "ABCO " = "ABCO",
                                              "UMBCAL " = "UMBCAL",
                                              "ABMA " = "ABMA",
                                              "CADE " = "CADE",
                                              "PILA " = "PILA",
                                                   "PIJE " = "PIJE",
                                                   "QUKE " = "QUKE")
                           
                           

HG_trees_clean <- HG_trees %>% 
  mutate_if(is.character, list(~na_if(.,"")))

unique(HG_trees_clean$species)

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
         status != "C") %>% 
  drop_na(dbh_cm)

str(HG_trees_clean_final)
unique(HG_trees_clean_final$status)
unique(HG_trees_clean_final$plotid)

#change plotid stuff (if need be) HERE


write.csv(HG_trees_clean_final, "HolyGrail/data/clean/trees/HG_Trees_final_clean.csv")

