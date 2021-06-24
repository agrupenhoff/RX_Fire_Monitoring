

library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(knitr)
library(kableExtra)
library(rio)
library(plyr)

HG_trees <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_Trees.csv")

#ADD NEW TREE DATA HERE

#enter Caples Data here
caples_trees_all <- read.csv("HolyGrail/data/raw/RAW_data_ALL/TREES/Caples_TreeData_all.csv")

                  #change sampling time to pre_post_fire & postFire (aka time post fire)
                  caples_trees_clean <- caples_trees_all %>% 
                    mutate(postFire = case_when(
                      samplingTime == 1 ~ "immediate",
                      samplingTime == 2 ~ "1yr")) %>% 
                    mutate(pre_post_fire = case_when(
                      samplingTime == 0 ~ "prefire",
                      samplingTime == 1 ~ "prefire",
                      samplingTime == 2 ~ "postfire")) %>% 
                    mutate(year = case_when(
                      samplingTime == 0 ~ "2013-2018",
                      samplingTime == 1 ~ "2019",
                      samplingTime == 2 ~ "2020"))
                  
                  # Select variables to drop
                  dropped <- names(caples_trees_clean) %in% c("ï..project", "Large.Small.ALL2019",
                                                              "EveryOther_Y.N", "scorch_min_ht",  "id",
                                                              "bolechar_min","bolechar_max", "torch_min_ht"
                  )
                  
                  caples_trees_subset <- caples_trees_clean[!dropped]


#JOIN new data to HOLY GRAIL
HG_trees_new <- merge(HG_trees, caples_trees_subset, all=TRUE)


#convertvalues to numeric
HG_trees_FINAL <- HG_trees_new %>% 
  drop_na("dbh_cm") %>% 
  mutate(dbh_cm=as.numeric(dbh_cm),
  ht_m=as.numeric(ht_m),
  ht2crown_m=as.numeric(ht2crown_m),
  livecrownratio_perc=as.numeric(livecrownratio_perc),
  ht2dead_m=as.numeric(ht2dead_m),
  crownwidth_m=as.numeric(crownwidth_m),
  scorch_ht_m=as.numeric(scorch_ht_m),
  scorch_percent=as.numeric(scorch_percent),
  torch_ht_m=as.numeric(torch_ht_m),
  torch_percent=as.numeric(torch_percent),
  bolechar_m=as.numeric(bolechar_m))


#export & save new Trees file
export(HG_trees_FINAL, "HolyGrail/data/clean/HolyGrail_Trees_final.csv")
