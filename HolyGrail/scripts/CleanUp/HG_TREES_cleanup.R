
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(knitr)
library(kableExtra)
library(rio)
library(plyr)

HG_trees <- read.csv("HolyGrail/data/clean/HolyGrail_Trees_final.csv")

##MAKE SURE ALL TREE SPECIES CODES ARE CORRECT
HG_trees$species <- toupper(HG_trees$species) #make all codes uppercase

#recode everything to USDA plant list
unique(HG_trees$species)
str(HG_trees)


HG_trees$species <- recode(HG_trees$species, "PIMONO" = "PIMO", 
                                              "CADE" = "CADE27",
                                             "QBMA" = "ABMA",
                                             "UNK" = "UNKNOWN",
                                             "ADCO" = "ABCO",
                              "ACEMAC" = "ACMA3",
                              "CONU" = "CONU4",
                              "POTR" = "POTR5",
                              "PIMO" = "PIMO3",
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
                              "FABMA" = "ABMA")

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
                                "LUVE"= "LIVE")

unique(HG_trees_clean$species)


export(HG_trees_clean, "HolyGrail/data/clean/HolyGrail_Trees_final.csv")

