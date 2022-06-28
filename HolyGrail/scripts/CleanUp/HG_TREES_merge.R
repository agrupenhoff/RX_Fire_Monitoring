

library(tidyverse)
library(tibble)
library(dplyr)

HG_trees <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_Trees.csv")

#ADD NEW TREE DATA HERE

#Not needed 2019-2021


#convertvalues to numeric
HG_trees_FINAL <- HG_trees %>% 
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
export(HG_trees_FINAL, "HolyGrail/data/clean/HG_Trees_final.csv")
