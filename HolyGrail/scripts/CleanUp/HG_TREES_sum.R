library(tidyverse)
library(tibble)
library(dplyr)
library(rio)


HG_trees_data <- read.csv("HolyGrail/data/clean/HG_Trees_final.csv")

#create function to calculate basal area (m^2)

basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2

#add basal area, tree density, and tree type
HG_trees_basal <- HG_trees_data %>% 
  filter(dbh_cm != 999.0) %>% 
  mutate(BA_m2 = basal.area.fn(dbh_cm)) %>% 
  mutate(plotid_time = paste(site, plotid, pre_post_fire, year, sep = "_")) %>% 
  add_count(plotid_time,status, species, name="n_trees_by_plot") %>% 
  mutate_if(is.character, list(~na_if(.,""))) 

str(HG_trees_basal)


##Aggregate by plot live/dead by species
HG_trees_PlotSpeciesStatus <- HG_trees_basal %>% 
  group_by(plotid_time, species, status) %>% 
  summarise(BA_m2_sum = sum(BA_m2, na.rm=TRUE),
            n_trees = mean(n_trees_by_plot)) %>% 
  mutate(n_trees_acre = n_trees*10,
         BA_m2_acre = BA_m2_sum*10,
         n_trees_ha = n_trees*4.04686,
         BA_m2_ha = BA_m2_sum*4.04686) %>%   
  separate(plotid_time, c("site","plotid","pre_post_fire","year"), sep="_",
           remove=TRUE,extra="merge")

export(HG_trees_PlotSpeciesStatus,"HolyGrail/data/clean/HG_trees_PlotSpeciesStatus.csv" )
              
#Aggregate by plot TOTAL live/dead
HG_trees_totalPlot <- HG_trees_aggregate_plotspecies %>% 
                mutate(plotid_time = paste(site, plotid, pre_post_fire, year, sep = "_")) %>% 
                group_by(plotid_time, status) %>% 
                add_count(plotid_time,status, name="n_trees_total_plot") %>%
                summarise(BA_m2_acre_plot = sum(BA_m2_acre),
                          n_trees_totalPlot = sum(n_trees)) %>% 
                separate(plotid_time, c("site","plotid","pre_post_fire","year"), sep="_",
                       remove=TRUE,extra="merge")
      
export(HG_trees_totalPlot,"HolyGrail/data/clean/HG_Trees_totalPlot.csv" )
              
              
#by diameter class
HG_trees_diamclass <- HG_trees_basal %>% 
          mutate(plotid_time = paste(site, plotid, pre_post_fire, year, sep = "_")) %>% 
          mutate(diamclass = case_when(
              dbh_cm < 20 ~ "[<20]",
              dbh_cm  >= 20 & dbh_cm  < 40 ~ "[20-40]",
              dbh_cm  >= 40 & dbh_cm  < 60 ~ "[40-60]",
              dbh_cm >= 60 & dbh_cm  < 80 ~ "[60-80]",
              dbh_cm  >= 80 & dbh_cm  < 100 ~ "[80-100]",
              dbh_cm  >= 100  ~ "[>100]")) %>% 
        add_count(plotid_time, status, diamclass, name="n_trees_diamclass") %>% 
        group_by(plotid_time,diamclass,status) %>% 
        summarise(n_trees_diamclass = mean(n_trees_diamclass))
        
HG_trees_diamclass_sum <- HG_trees_diamclass %>% 
        filter(status == "LIVE") %>% 
        select(plotid_time, diamclass, n_trees_diamclass) %>% 
        pivot_wider(names_from = diamclass, values_from = n_trees_diamclass,
                    values_fill = 0) %>% 
        pivot_longer(-plotid_time, names_to= "diamclass", values_to = "n_trees_diamclass") %>% 
        mutate(n_trees_diamclass_acre = n_trees_diamclass*10) %>% 
        separate(plotid_time, c("site","plotid","pre_post_fire","year"), sep="_",
               remove=TRUE,extra="merge")
    
###THIS IS LIVE ONLY!!!
              
              
export(HG_trees_diamclass_sum,"HolyGrail/data/clean/HG_Trees_diamclass_SUM.csv" )



    #SCORCH & TORCH POST FIRE SUMMARY
HG_trees_severityIndicies <- HG_trees_basal %>% 
    filter(pre_post_fire == "postfire",
           status == 'LIVE') %>% 
    mutate(plotid_time = paste(site, plotid, year, postFire, sep = "_")) %>%
    group_by(plotid_time, species, status) %>% 
    summarise(avg_scorch_ht_m = mean(scorch_ht_m),
              avg_scorch_percent = mean(scorch_percent),
              avg_torch_ht_m = mean(torch_ht_m),
              avg_torch_percent = mean(torch_percent),
              avg_bolechar_m = mean(bolechar_m)) %>% 
  separate(plotid_time, c("site","plotid","year","postFire"), sep="_",
           remove=TRUE,extra="merge")
  
export(HG_trees_severityIndicies,"HolyGrail/data/clean/HG_Trees_severityIndicies.csv" ) 
              
              ###############################################################################################
              ##############################################################################################
              

