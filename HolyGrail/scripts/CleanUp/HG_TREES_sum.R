library(tidyverse)
library(tibble)
library(dplyr)
library(rio)


HG_trees_data <- read.csv("HolyGrail/data/clean/trees/HG_Trees_final.csv")
HG_trees_data$site <- tolower(HG_trees_data$site)
HG_trees_data$plotid <- tolower(HG_trees_data$plotid)

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

##########################
#############################
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

export(HG_trees_PlotSpeciesStatus,"HolyGrail/data/clean/trees/HG_trees_PlotSpeciesStatus.csv" )
              
#Aggregate by plot TOTAL live/dead
HG_trees_totalPlot <- HG_trees_PlotSpeciesStatus %>% 
                mutate(plotid_time = paste(site, plotid, pre_post_fire, year, sep = "_")) %>% 
                group_by(plotid_time, status) %>% 
                add_count(plotid_time,status, name="n_trees_total_plot") %>%
                summarise(BA_m2_ha_plot = sum(BA_m2_ha),
                          n_trees_totalPlot = sum(n_trees)) %>% 
                separate(plotid_time, c("site","plotid","pre_post_fire","year"), sep="_",
                       remove=TRUE,extra="merge")
      
export(HG_trees_totalPlot,"HolyGrail/data/clean/trees/HG_Trees_totalPlot.csv" )

##########################
########BASAL AREA PROPORTION
HGtrees_basalareaTotal <- HG_trees_totalPlot %>% 
  filter(status == "LIVE") %>% 
  mutate(plotid_time = paste(site, plotid, pre_post_fire, year, sep = "_")) %>% 
  select(plotid_time, BA_m2_ha_plot)

HGtrees_basalareaSpecies <- HG_trees_PlotSpeciesStatus %>% 
  filter(status == "LIVE") %>% 
  mutate(plotid_time = paste(site, plotid, pre_post_fire, year, sep = "_")) %>% 
  #add codes that relate to litter/duff coef
  mutate(spp = case_when(
                  species == "PSME" ~ "PSME",
                  species == "PISA" ~ "PISA",
                  species == "SEGI" ~ "SEGI",
                  species == "CADE27" ~ "CADE",
                  species == "CADE" ~ "CADE",
                  species == "CADE" ~ "CADE ",
                  species == "PIJE" ~ "PIJE",
                  species == "PIAT" ~ "PIAT",
                  species == "PICO" ~ "PICO",
                  species == "PIPO" ~ "PIPO",
                  species == "ABMA" ~ "ABMA",
                  species == "PIMO1"~"PIMO1",
                  species == "PILA" ~ "PILA",
                  species == "PIPOW" ~ "PIWA",
                  species == "JUOC" ~ "JUOC",
                  species == "PIMO2" ~ "PIMO2",
                  species == "ABCO" ~ "ABCO",
                  species == "PIAL" ~ "PIAL",
                  TRUE ~ "ALLSPP"
  )) %>% 
  select(plotid_time, species, spp, BA_m2_ha)

#sum just by litter.duff coef
HGtrees_BAspecies <- HGtrees_basalareaSpecies %>% 
  group_by(plotid_time, spp) %>% 
  summarise(BA_m2_ha_spp=sum(BA_m2_ha))

HGtrees_basalareaSpeciesTotal <- left_join(HGtrees_BAspecies, HGtrees_basalareaTotal,
                                           by = "plotid_time")

HGtrees_basalareaSpeciesProportion <- HGtrees_basalareaSpeciesTotal %>% 
  mutate(pBA_m2_ha = BA_m2_ha_spp/BA_m2_ha_plot) 

export(HGtrees_basalareaSpeciesProportion, "HolyGrail/data/clean/trees/HGtrees_basalareaSpeciesProportion.csv")

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
        
###THIS IS LIVE ONLY!!!
HG_trees_diamclass_sumLIVE <- HG_trees_diamclass %>% 
        filter(status == "LIVE") %>% 
        select(plotid_time, diamclass, n_trees_diamclass) %>% 
        pivot_wider(names_from = diamclass, values_from = n_trees_diamclass,
                    values_fill = 0) %>% 
        pivot_longer(-plotid_time, names_to= "diamclass", values_to = "n_trees_diamclass") %>% 
        mutate(n_trees_diamclass_acre = n_trees_diamclass*10) %>% 
        separate(plotid_time, c("site","plotid","pre_post_fire","year"), sep="_",
               remove=TRUE,extra="merge")
#DEAD ONLY
HG_trees_diamclass_sumDEAD <- HG_trees_diamclass %>% 
  filter(status == "DEAD") %>% 
  select(plotid_time, diamclass, n_trees_diamclass) %>% 
  pivot_wider(names_from = diamclass, values_from = n_trees_diamclass,
              values_fill = 0) %>% 
  pivot_longer(-plotid_time, names_to= "diamclass", values_to = "n_trees_diamclass") %>% 
  mutate(n_trees_diamclass_acre = n_trees_diamclass*10) %>% 
  separate(plotid_time, c("site","plotid","pre_post_fire","year"), sep="_",
           remove=TRUE,extra="merge")
    

export(HG_trees_diamclass_sumLIVE,"HolyGrail/data/clean/trees/HG_trees_diamclass_sumLIVE.csv" )
export(HG_trees_diamclass_sumDEAD,"HolyGrail/data/clean/trees/HG_trees_diamclass_sumDEAD.csv" )



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
  
export(HG_trees_severityIndicies,"HolyGrail/data/clean/trees/HG_Trees_severityIndicies.csv" ) 
              
              ###############################################################################################
              ##############################################################################################
              

