library(tidyverse)
library(tibble)
library(dplyr)
library(rio)

###FINE FUELS FIRST
HG_finefuels <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_FineFuels.csv")

#ADD NEW FUELS DATA HERE

            #enter Caples Data here
            caples_finefuels <- read.csv("HolyGrail/data/raw/RAW_data_ALL/CaplesFineFuels.csv")
            
            #change sampling time to pre_post_fire & postFire (aka time post fire)
            caples_finefuels_clean <- caples_finefuels %>% 
              mutate(postTime = case_when(
                SamplingTime == 1 ~ "immediate",
                SamplingTime == 2 ~ "1yr")) %>% 
              mutate(pre_post_fire = case_when(
                SamplingTime == 0 ~ "prefire",
                SamplingTime == 1 ~ "prefire",
                SamplingTime == 2 ~ "postfire")) %>% 
              mutate(year = case_when(
                SamplingTime == 0 ~ "2013-2018",
                SamplingTime == 1 ~ "2019",
                SamplingTime == 2 ~ "2020")) %>% 
              mutate(slope_percent = slope)
            
            #drop things you don't want
            dropped <- names(caples_finefuels_clean) %in% c("ï..ID", "SamplingTime","slope")
            
            caples_finefuels_subset <- caples_finefuels_clean[!dropped]
            
            
  #JOIN new data to HOLY GRAIL
  HG_finefuels_merge <- merge(HG_finefuels, caples_finefuels_subset, all=TRUE)
  HG_finefuels_merge <- HG_finefuels_merge %>% 
    drop_na(year)

  #export & save new finefuels file
  export(HG_finefuels_merge, "HolyGrail/data/raw/CPFMP_HolyGrail_FineFuels_Merge.csv")  

  
####################################################
###################################################
  
  
## CWD now
HG_CWD <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_CWD.csv")
  
                #ADD NEW FUELS DATA HERE
                
                #enter Caples Data here
                caples_CWD <- read.csv("HolyGrail/data/raw/RAW_data_ALL/Caples_CWD.csv")
                
                #change sampling time to pre_post_fire & postFire (aka time post fire)
                caples_CWD_clean <- caples_CWD %>% 
                  mutate(postTime = case_when(
                    SamplingTime == 1 ~ "immediate",
                    SamplingTime == 2 ~ "1yr")) %>% 
                  mutate(pre_post_fire = case_when(
                    SamplingTime == 0 ~ "prefire",
                    SamplingTime == 1 ~ "prefire",
                    SamplingTime == 2 ~ "postfire")) %>% 
                  mutate(year = case_when(
                    SamplingTime == 0 ~ "2013-2018",
                    SamplingTime == 1 ~ "2019",
                    SamplingTime == 2 ~ "2020")) %>% 
                  mutate(azimuth = Azimuth) %>% 
                  rename(plot_id = plotid)
                
                #drop things you don't want
                dropped <- names(caples_CWD_clean) %in% c("ï..ID", "SamplingTime","Azimuth")
                
                caples_CWD_subset <- caples_CWD_clean[!dropped]
  
  
  #JOIN new data to HOLY GRAIL
  HG_CWD_merge <- merge(HG_CWD, caples_CWD_subset, all=TRUE)
  
  #export & save new finefuels file
  export(HG_CWD_merge, "HolyGrail/data/raw/CPFMP_HolyGrail_CWD_Merge.csv")  
  