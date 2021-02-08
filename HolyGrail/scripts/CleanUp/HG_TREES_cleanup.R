
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


HG_trees <- read.csv("HolyGrail/data/raw/HolyGrail_Trees.csv")
HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass.csv")
HG_Trees_totalPlot <- read.csv("HolyGrail/data/clean/HG_Trees_totalPlot.csv")
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN.csv")

#convertvalues to numeric
#HG_trees <- HG_trees %>% mutate(
            #dbh_cm=as.numeric(dbh_cm),
            #ht_m=as.numeric(ht_m),
            #ht2crown_m=as.numeric(ht2crown_m),
            #livecrownratio_perc=as.numeric(livecrownratio_perc),
            #ht2dead_m=as.numeric(ht2dead_m),
            #crownwidth_m=as.numeric(crownwidth_m),
            #scorch_ht_m=as.numeric(scorch_ht_m),
            #scorch_percent=as.numeric(scorch_percent),
            #torch_ht_m=as.numeric(torch_ht_m),
            #torch_percent=as.numeric(torch_percent),
            #bolechar_m=as.numeric(bolechar_m)
                                                #)
  

#create function to calculate basal area (m^2)

      #basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2

#add basal area, tree density, and tree type
#HG_trees_aggregate <- HG_trees %>% 
#  mutate(BA_m2 = basal.area.fn(dbh_cm)) %>% 
#  add_count(plotid,status, species, name="n_trees_by_plot") %>% 
#  mutate(plotid_time = paste(plotid, pre_post_fire, postFire))

##Aggregate by plot live/dead by species
#HG_trees_aggregate_plot_species <- HG_trees_aggregate %>% 
#  group_by(plotid_time, status, species) %>% 
#  add_count(plotid_time,status, species, name="n_trees_by_plot") %>% 
#  summarise(BA_m2_sum = sum(BA_m2),
#            n_trees = mean(n_trees_by_plot),
#            mean_dbh_cm=mean(dbh_cm),
#            max_dbh_cm=max(dbh_cm),
#            min_dbh_cm=min(dbh_cm),
#            mean_ht2c=mean(ht2crown_m),
#            mean_ht=mean(ht_m),
#            mean_ht2d=mean(ht2dead_m)) %>% 
#  mutate(n_trees_ha = n_trees*4.04686,
 #        BA_m2_ha = BA_m2_sum*4.04686) # 10 acres = 4.04686 ha

#export(HG_trees_aggregate_plot_species,"HolyGrail/data/clean/HG_Trees_plot_species.csv" )

##Aggregate by plot TOTAL live/dead
#HG_trees_aggregate_totalPlot <- HG_Trees_plot_species %>% 
#  group_by(plotid_time, status) %>% 
#  add_count(plotid_time,status, name="n_trees_total_plot") %>%
#  summarise(BA_m2_sum = sum(BA_m2_sum),
#            n_trees = mean(n_trees_total_plot),
#            mean_dbh_cm=mean(mean_dbh_cm),
#           max_dbh_cm=max(max_dbh_cm),
#            min_dbh_cm=min(min_dbh_cm),
#            mean_ht2c=mean(mean_ht2c),
#            mean_ht=mean(mean_ht),
#            mean_ht2d=mean(mean_ht2d)) %>% 
#  mutate(n_trees_ha = n_trees*4.04686,
#         BA_m2_ha = BA_m2_sum*4.04686) 
  

#HG_trees_aggregate_totalPlot <- HG_trees_aggregate_totalPlot %>% 
#  separate(plotid_time, 
#           c("plotid","pre_post_fire","postFire"), 
#           sep=" ",
#           remove=TRUE,
#           extra="merge")

#HG_trees_diamclass <- HG_trees %>% 
# mutate(diamclass = case_when(
#    dbh_cm < 20 ~ "[<20]",
#    dbh_cm  >= 20 & dbh_cm  < 40 ~ "[20-40]",
#   dbh_cm  >= 40 & dbh_cm  < 60 ~ "[40-60]",
#    dbh_cm >= 60 & dbh_cm  < 80 ~ "[60-80]",
#    dbh_cm  >= 80 & dbh_cm  < 100 ~ "[80-100]",
#    dbh_cm  >= 100  ~ "[>100]"
#  ))


#export(HG_trees_diamclass,"HolyGrail/data/clean/HG_Trees_diamclass.csv" )




###############################################################################################
##############################################################################################

