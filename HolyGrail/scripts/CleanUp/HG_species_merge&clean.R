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


antelope_drycreek_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/antelope_drycreek_2020_pre_spp.csv")
bearmtn_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/bearmtn_2019_pre_spp.csv",
                                 col_types = cols(postTime = col_character()))
bearmtn_2019_pre_spp
bearmtn_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/bearmtn_2021_post_spp.csv")
bliss_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/bliss_2019_pre_spp.csv",
                               col_types = cols(postTime = col_character()))
bliss_2019_pre_spp
bouverie_riolindo_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/bouverie_riolindo_2021_pre_spp.csv",
                                           col_types = cols(postTime = col_character()))
bouverie_riolindo_2021_pre_spp
burton_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/burton_2019_pre_spp.csv",
                                col_types = cols(postTime = col_character()))
burton_2019_pre_spp
burton_2019_pre_spp <- burton_2019_pre_spp %>% 
  rename(obs = observer)
burton_2020_post_spp <- read_csv("HolyGrail/data/raw/species_comp/burton_2020_post_spp.csv")
calaveras_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/calaveras_2020_pre_spp.csv")
caples_2021_post_spp <- read_csv("HolyGrail/data/raw/species_comp/caples_2021_post_spp.csv")
concow_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/concow_2021_pre_spp.csv")
downie_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/downie_2020_pre_spp.csv")
FM_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/FM_2019_pre_spp.csv")
FM_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/FM_2020_pre_spp.csv")
henrycoe_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/henrycoe_2021_pre_spp.csv")
indy_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/indy_2020_pre_spp.csv")
klamath_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/klamath_2021_pre_spp.csv")
lbw_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/lbw_2020_pre_spp.csv")
modoc_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/modoc_2020_pre_spp.csv")
shaver_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/shaver_2021_pre_spp.csv")
slypark_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/slypark_2019_pre_spp.csv")
slypark_2021_post_spp <- read_csv("HolyGrail/data/raw/species_comp/slypark_2021_post_spp.csv")
springs_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/springs_2019_pre_spp.csv")
springs_2020_post_spp <- read_csv("HolyGrail/data/raw/species_comp/springs_2020_post_spp.csv")
sugarpine_2020_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/sugarpine_2020_pre_spp.csv")
val_2019_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/val_2019_pre_spp.csv")
val_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/val_2021_pre_spp.csv")
wilderRanch_2021_pre_spp <- read_csv("HolyGrail/data/raw/species_comp/wilderRanch_2021_pre_spp.csv")

#change sampling time to pre_post_fire & postFire (aka time post fire)
caples_2021_post_spp_clean <- caples_2021_post_spp %>% 
  mutate(postTime = case_when(
    SamplingTime == 1 ~ "immediate",
    SamplingTime == 2 ~ "1yr")) %>% 
  mutate(pre_post_fire = case_when(
    SamplingTime == 0 ~ "prefire",
    SamplingTime == 1 ~ "postfire",
    SamplingTime == 2 ~ "postfire")) %>% 
  mutate(year = case_when(
    SamplingTime == 0 ~ 2018,
    SamplingTime == 1 ~ 2019,
    SamplingTime == 2 ~ 2020)) %>% 
  mutate(site = "caples") %>% 
  mutate(plotid_full = paste(site, plotid, sep = "_"))

str(caples_2021_post_spp_clean)

#drop things you don't want
dropped <- names(caples_2021_post_spp_clean) %in% c("plotid", "SamplingTime", "USDACode", "SpeciesName")

caples_2021_post_spp_subset <- caples_2021_post_spp_clean[!dropped]
caples_2021_post_spp_subset <- caples_2021_post_spp_subset %>% 
  rename(plotid = plotid_full)
str(caples_2021_post_spp_subset)
caples_2021_post_spp_subset$percent <- as.character(caples_2021_post_spp_subset$percent)

str(FM_2019_pre_spp)

HG_species <- bind_rows(antelope_drycreek_2020_pre_spp, bearmtn_2019_pre_spp, bearmtn_2021_pre_spp,
                        bliss_2019_pre_spp, bouverie_riolindo_2021_pre_spp, burton_2019_pre_spp,
                        burton_2020_post_spp, calaveras_2020_pre_spp, caples_2021_post_spp_subset,
                        concow_2021_pre_spp, downie_2020_pre_spp, FM_2019_pre_spp, FM_2020_pre_spp,
                        henrycoe_2021_pre_spp, indy_2020_pre_spp, klamath_2021_pre_spp, 
                        lbw_2020_pre_spp, modoc_2020_pre_spp , shaver_2021_pre_spp,
                        slypark_2019_pre_spp, slypark_2021_post_spp, springs_2019_pre_spp, 
                        springs_2020_post_spp, sugarpine_2020_pre_spp,
                        val_2019_pre_spp, val_2021_pre_spp, wilderRanch_2021_pre_spp)

HG_species <- HG_species %>% 
  select(site, year, pre_post_fire, pre_post_thin, postTime, plotid, obs,
         species, status, lifeform, layer_code, percent, notes) %>% 
  drop_na(percent)

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
HG_species1$percent[HG_species1$percent == "3?"] <- "3"
HG_species1$percent[HG_species1$percent == "5?"] <- "5"
HG_species1$percent[HG_species1$percent == "1?"] <- "1"
HG_species1$percent[HG_species1$percent == ".0.75"] <- ".75"


HG_species1$plotid <- recode(HG_species1$plotid, "springs1" = "springs01",
                             "springs2"= "springs02",
                             "springs3" ="springs03",
                             "springs4"= "springs04",
                             "springs5"= "springs05",
                             "springs6"= "springs06",
                             "springs7"= "springs07",
                             "springs8"= "springs08",
                             "springs9"= "springs09",
                             "spring26" = "springs26")
str(HG_species1)
HG_species1$percent <- as.numeric(HG_species1$percent)
  
glimpse(HG_species1)


export(HG_species1, "HolyGrail/data/clean/HG_species_clean.csv")

