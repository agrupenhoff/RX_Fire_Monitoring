
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

######JUST VALENTINE RESERVE

VR_plottype <- import("HolyGrail/data/raw/VR_plottype.csv")

Val_trees_all <- HG_trees_diamclass %>% 
  filter(site == 'Valentine') 
 
Val_trees_all$dbh_cm <- as.numeric(Val_trees_all$dbh_cm)
str(Val_trees_all)
Val_trees_veg <-  left_join(Val_trees_all, VR_plottype, by='plotid')

######### DBH CLASS SIZE FOR VAL

#DBH class size
Val_trees_live2019 <- Val_trees_veg %>% 
  filter(status == 'L') %>% 
  group_by(plotid, diamclass) %>% 
  add_count(plotid,diamclass, name="n_trees_diamclass") %>% 
  select(plotid, diamclass, n_trees_diamclass, PlotType) %>% 
  mutate(plotid_diam = paste(plotid, diamclass)) %>% 
  group_by(plotid_diam) %>% 
  summarise(n_trees2019 = mean(n_trees_diamclass)) %>% 
  mutate(n_trees2019ha = (n_trees2019*4.05)) ### 1/10 acre; 1 acre = 0.405 ha


val_trees_live2020 <- Val_trees_veg %>% 
  filter(status == "L",
         presentPostThin == "Yes") %>% 
  group_by(plotid, diamclass) %>% 
  add_count(plotid,diamclass, name="n_trees_diamclass") %>% 
  select(plotid, diamclass, n_trees_diamclass, PlotType) %>% 
  mutate(plotid_diam = paste(plotid, diamclass)) %>% 
  group_by(plotid_diam) %>% 
  summarise(n_trees2020 = mean(n_trees_diamclass)) %>% 
  mutate(n_trees2020ha = (n_trees2020*4.05))

#merge 2019 & 2020 together
val_trees_diamclass <- left_join(Val_trees_live2019,val_trees_live2020, by=c("plotid_diam"))

val_trees_diamclass <- val_trees_diamclass %>% 
  separate(plotid_diam, c("plotid","diam_class"), sep=" ")

#add plot type
val_trees_diamveg <- left_join(val_trees_diamclass, VR_plottype, by="plotid")
val_trees_diamveg <- val_trees_diamveg %>% 
  mutate(diam_type = paste(plotid,diam_class,PlotType)) %>% 
  select(diam_type, n_trees2019ha, n_trees2020ha) %>% 
  pivot_longer(cols= -diam_type, names_to= "condition", values_to ="n_trees_ha") %>% 
  separate(diam_type, c("plotid", "diam_class","plotType"),sep=" ")%>% 
  filter(plotType == "MixedConifer"|
           plotType == "UpperMontane") %>% 
  mutate(diam_type = paste(diam_class, plotType))


#add NRV values
NRV_diamclass <- NRV_diamclass %>% 
  mutate(diam_type = paste(diam_class, PlotType))
val_trees_NRV <- left_join(val_trees_diamveg, NRV_diamclass, by="diam_type")

#CLEAN ER UP
val_trees_NRV_clean <- val_trees_NRV %>% 
  select(plotid, diam_class.x,plotType,condition,n_trees_ha, NRV, NRV_upper, NRV_lower) 


#####PLOT!!!!!!

VR_diamclass <- ggplot(data=val_trees_NRV_clean)+
  geom_boxplot(aes(x=diam_class.x, y=n_trees_ha, fill=condition))+
  facet_wrap(~ plotType, scales = "free_y") 
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))+
  xlab("Diameter Class")+
  ylab("# Trees / Ha")
VR_diamclass


