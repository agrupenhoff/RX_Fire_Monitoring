library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(knitr)
library(kableExtra)
library(rio)
library(lme4)
library(wesanderson)
library(ggpubr)

###Pull together species comp data and merge to compiled file

bearmtn_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/bearmtn_2019_pre_spp.csv")
bearmtn_2019_pre_spp <- bearmtn_2019_pre_spp %>% 
  rename(observer = obs)
bliss_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/bliss_2019_pre_spp.csv")
burton_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/burton_2019_pre_spp.csv")
burton_2020_post_spp <- read_csv("HolyGrail/data/raw/species_comp/burton_2020_post_spp.csv")
calaveras_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/calaveras_2020_pre_spp.csv")
downie_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/downie_2020_pre_spp.csv")
drycreek_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/drycreek_2020_pre_spp.csv")
FM_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/FM_2019_pre_spp.csv")
FM_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/FM_2020_pre_spp.csv")
indy_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/indy_2020_pre_spp.csv")
lbw_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/lbw_2020_pre_spp.csv")
modoc_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/modoc_2020_pre_spp.csv")
slypark_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/slypark_2019_pre_spp.csv")
springs_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/springs_2019_pre_spp.csv")
springs_2020_post_spp <- read_csv("HolyGrail/data/raw/species_comp/springs_2020_post_spp.csv")
sugarpine_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/sugarpine_2020_pre_spp.csv")
val_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/val_2019_pre_spp.csv")


HG_species <- bind_rows(bearmtn_2019_pre_spp,bliss_2019_pre_spp,burton_2019_pre_spp,
                        burton_2020_post_spp, calaveras_2020_pre_spp, downie_2020_pre_spp ,
                        drycreek_2020_pre_spp, FM_2019_pre_spp, FM_2020_pre_spp,
                        indy_2020_pre_spp, lbw_2020_pre_spp, modoc_2020_pre_spp ,
                        slypark_2019_pre_spp,springs_2019_pre_spp, springs_2020_post_spp, sugarpine_2020_pre_spp,
                        val_2019_pre_spp)

HG_species <- HG_species %>% 
  select(site, year, pre_post_fire, postTime, plot_id, observer,
         species, status, lifeform, layerCode, percent, notes) 

#replace trace values
HG_species1 <- HG_species
HG_species1$percent[HG_species1$percent == "-"] <- "0"
HG_species1$percent[HG_species1$percent == "tr"] <- "0.05"
HG_species1$percent[HG_species1$percent == "<1"] <- "0.05"
HG_species1$percent[HG_species1$percent == "tr?"] <- "0.05"
HG_species1$percent[HG_species1$percent == "tt"] <- "0.05"
HG_species1$percent[HG_species1$percent == "tf"] <- "0.05"
HG_species1$percent[HG_species1$percent == "t"] <- "0.05"
HG_species1$percent[HG_species1$percent == "t4"] <- "0.05"
HG_species1$percent[HG_species1$percent == "na"] <- "0.05"

HG_species1 <- HG_species1 %>% 
  replace_na(list(percent = 0.05))
HG_species1$percent <- as.numeric(HG_species1$percent)
  
glimpse(HG_species1)


export(HG_species1, "HolyGrail/data/clean/HG_species_clean.csv")

HG_species1 <- import("HolyGrail/data/clean/HG_species_clean.csv")

unique(HG_species1$percent)
