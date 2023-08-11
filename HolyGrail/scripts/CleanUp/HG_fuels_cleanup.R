library(tidyverse)
library(tibble)
library(dplyr)
library(rio)

###FINE FUELS FIRST
HG_finefuels <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_FineFuels_Merge.csv")
HG_CWD <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_CWD_Merge.csv")  

####### 
####### CWD CLEAN UP

#change rows with no CWD (written as na, no cwd, or none) to include NA then replace with 0
#make sure azimuth values all match - make all letter

              #check all values in intersect 
              unique(HG_CWD$intersect_cm)
              unique(HG_CWD$azimuth)
              
              HG_CWD_NA <- HG_CWD %>% 
                mutate_if(is.character, list(~na_if(.,"na"))) %>% 
                mutate_if(is.character, list(~na_if(.,"SEwPete;NWnocwd"))) %>% 
                mutate_if(is.character, list(~na_if(.,"none"))) %>% 
                mutate_if(is.character, list(~na_if(.,""))) %>% 
                mutate_if(is.character, list(~na_if(.,"no cwd"))) %>% 
                mutate_if(is.character, list(~na_if(.,"nocwd"))) %>% 
                mutate_if(is.character, list(~na_if(.,"-"))) %>% 
                mutate(intersect_cm = ifelse(is.na(intersect_cm), 0, intersect_cm)) %>% 
                drop_na(azimuth)
              
              unique(HG_CWD_NA$intersect_cm)
              unique(HG_CWD_NA$azimuth)
              
              #get azimuth the same
              HG_CWD_NA$azimuth <- toupper(HG_CWD_NA$azimuth) #make all codes uppercase
              HG_CWD_NA$azimuth <- recode(HG_CWD_NA$azimuth, "N" = "0",
                                 "E"= "90",
                                 "W" ="270",
                                 "S"= "180")
              
             
              HG_CWD_NA$intersect_cm <- as.numeric(HG_CWD_NA$intersect_cm)
              
              str(HG_CWD_NA)
              unique(HG_CWD_NA$intersect_cm)
              unique(HG_CWD_NA$azimuth)
              

  
#sum CWD values 

HG_CWD_sum <- HG_CWD_NA %>% 
  mutate(decay_type = case_when(
    decay >= 4  ~ "rotten",
    decay < 4 ~ "sound")) %>% 
  mutate(site_plotid_time_azimuth = paste(site, plot_id, year, pre_post_fire, postTime, azimuth, sep="_")) %>%
  mutate(diam_cm_square = (intersect_cm)^2) %>% 
  group_by(site_plotid_time_azimuth, decay_type) %>% 
  summarise(sum_d2_1000_cm2 = sum(diam_cm_square)) %>% 
  replace_na(list(rotten =0, sound =0)) %>% 
  drop_na("decay_type") ## remove all rows with no decay type

unique(HG_CWD_sum$decay_type)

 
HG_CWD_sum <- HG_CWD_sum %>% 
  pivot_wider(names_from="decay_type", values_from = "sum_d2_1000_cm2") 

HG_CWD_clean <- HG_CWD_sum %>% 
  replace_na(list(sound=0, rotten=0)) %>% 
  rename(sum_d2_1000r_cm2 = rotten) %>% 
  rename(sum_d2_1000s_cm2 = sound) 
  

write.csv(HG_CWD_clean, "HolyGrail/data/raw/CPFMP_HolyGrail_CWD_clean.csv")  

####################################################
###################################################
  
######
###### FINE FUELS CLEAN UP

str(HG_finefuels)
unique(HG_finefuels$azimuth)

#change na values
#make sure azimuth values all match - make all letter
HG_finefuels$slope_percent <- as.numeric(HG_finefuels$slope_percent)

              HG_finefuels_NA <- HG_finefuels %>% 
                mutate_if(is.character, list(~na_if(.,"-"))) %>% 
                mutate_if(is.character, list(~na_if(.,""))) %>% 
                drop_na(azimuth) %>% 
                mutate(slope_percent = ifelse(is.na(slope_percent), 0, slope_percent)) 
               
                
              #get azimuth the same
              HG_finefuels_NA$azimuth <- toupper(HG_finefuels_NA$azimuth) #make all codes uppercase
              HG_finefuels_NA$azimuth <- recode(HG_finefuels_NA$azimuth, "N" = "0",
                                          "E"= "90",
                                          "W" ="270",
                                          "S"= "180")
           
              #check
              unique(HG_finefuels_NA$azimuth)
              str(HG_finefuels_NA)
              
              #change to numeric 
              HG_finefuels_NA$count_x1h <- as.numeric(HG_finefuels_NA$count_x1h)
              HG_finefuels_NA$count_x10h <- as.numeric(HG_finefuels_NA$count_x10h)
              HG_finefuels_NA$count_x100h <- as.numeric(HG_finefuels_NA$count_x100h)
              HG_finefuels_NA$duff1_cm <- as.numeric(HG_finefuels_NA$duff1_cm)
              HG_finefuels_NA$duff2_cm <- as.numeric(HG_finefuels_NA$duff2_cm)
              HG_finefuels_NA$duff3_cm <- as.numeric(HG_finefuels_NA$duff3_cm)
              HG_finefuels_NA$litter1_cm <- as.numeric(HG_finefuels_NA$litter1_cm)
              HG_finefuels_NA$litter2_cm <- as.numeric(HG_finefuels_NA$litter2_cm)
              HG_finefuels_NA$litter3_cm <- as.numeric(HG_finefuels_NA$litter3_cm)
              HG_finefuels_NA$fuel1_cm <- as.numeric(HG_finefuels_NA$fuel1_cm)
              HG_finefuels_NA$fuel2_cm <- as.numeric(HG_finefuels_NA$fuel2_cm)
              HG_finefuels_NA$fuel3_cm <- as.numeric(HG_finefuels_NA$fuel3_cm)


# FINE FUELS: average litter, duff, fuel depth && slope correction
HG_FWD_clean <- HG_finefuels_NA %>% 
  mutate( duff_depth_cm = rowMeans(select(.,starts_with("duff")), na.rm=TRUE),
          litter_depth_cm = rowMeans(select(.,starts_with("litter")), na.rm=TRUE),
          fuel_depth_cm = rowMeans(select(.,starts_with("fuel")), na.rm=TRUE)) %>% 
  mutate(site_plotid_time_azimuth = paste(site, plotid, year, pre_post_fire, postTime, azimuth, sep="_")) %>% 
  drop_na(count_x1h)

write_csv(HG_FWD_clean, "HolyGrail/data/raw/CPFMP_HolyGrail_FWD_clean.csv") 

#compile FWD and CWD together
HG_Fuels_ALL <- left_join(HG_FWD_clean, HG_CWD_clean, by="site_plotid_time_azimuth")

#fix postTime values
unique(HG_Fuels_ALL$postTime)
HG_Fuels_ALL$postTime <- recode(HG_Fuels_ALL$postTime,
                            "1year" = "1yr")



HG_FWD_CWD_final <- HG_Fuels_ALL %>% 
  replace_na(list(sum_d2_1000r_cm2=0, sum_d2_1000s_cm2=0)) %>% 
  mutate(plot_id_time = paste(plotid, pre_post_fire, postTime))

unique(HG_FWD_CWD_final$plotid)



write_csv(HG_FWD_CWD_final, "HolyGrail/data/clean/fuels/HG_FWD_CWD_final.csv") 
