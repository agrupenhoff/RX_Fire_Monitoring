library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(rio)
library(lme4)
library(wesanderson)
library(stringr)

###### PULL JUST SPRINGS FROM HOLY GRAIL

#HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass.csv")
        #make plot id lower case
        #HG_Trees_diamclass$plotid <- tolower(HG_Trees_diamclass$plotid)
        #export(HG_Trees_diamclass, "HolyGrail/data/clean/HG_Trees_diamclass.csv" )
#HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
        #make plot id lower case
        #HG_Trees_plot_species$plotid <- tolower(HG_Trees_plot_species$plotid)
        #HG_Trees_plot_species$plotid_time <- tolower(HG_Trees_plot_species$plotid_time)
        #export(HG_Trees_plot_species, "HolyGrail/data/clean/HG_Trees_plot_species.csv" )
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN.csv")
springs_trt <- import("SiteLocation/Springs_Fire/data/clean/Springs_trt.csv")

springs_trees_veg <- import("SiteLocation/Springs_Fire/data/clean/springs_trees_veg.csv")
springs_trees_plotspp <- import("SiteLocation/Springs_Fire/data/clean/springs_trees_plotspecies.csv")

        #create function to calculate basal area (m^2)
        
        basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2
        
        springs_trees_veg <- springs_trees_veg %>% 
          mutate(BA_m2 = basal.area.fn(dbh_cm),
                 BA_m2_acre = BA_m2*10)

####SPRINGS TREES BY DIAMETER CLASS
springs_trees_diam <- HG_Trees_diamclass %>% 
  filter(site == 'springsfire') 

springs_trees_diam$dbh_cm <- as.numeric(springs_trees_diam$dbh_cm)
str(springs_trees_diam)
springs_trees_veg <-  left_join(springs_trees_diam, springs_trt, by='plotid')


#SPRINGS TREES BY SPECIES
springs_trees_plotspp <- HG_Trees_plot_species %>% 
  filter(str_detect(plotid_time, "springs"))

#LIVE TREES################################
springs_trees_prefire <- springs_trees_veg %>% 
  filter(status == 'live',
         pre_post_fire == 'prefire') %>% 
  select(plotid, species, dbh_cm, ht2crown_m, ht_m, diamclass, burn_unit, priorburn_rx, PriorBurn)

springs_trees_postfire <- springs_trees_veg %>% 
  filter(status == 'live',
         pre_post_fire == 'postfire'|postFire == '1yr') %>% 
  select(plotid, species, dbh_cm, ht2crown_m, ht_m,diamclass, burn_unit, priorburn_rx, PriorBurn)



#export(springs_trees_veg, "SiteLocation/Springs_Fire/data/clean/springs_trees_veg.csv")
#export(springs_trees_plotspp, "SiteLocation/Springs_Fire/data/clean/springs_trees_plotspecies.csv")

#################
###########CHANGE BY SPECIES BY VEG TYPE                  

springs_trees_veg$species <- tolower(springs_trees_veg$species)

springs_trees_prefirespp <- springs_trees_veg %>% 
  filter(status == 'live',
         pre_post_fire == 'prefire') %>% 
  group_by(plotid, species) %>% 
  summarise(Prefire_BA_m2ac = sum(BA_m2_acre))%>% 
  mutate(plotidspp = paste(plotid, species)) %>% 
  select(plotidspp, Prefire_BA_m2ac )

springs_trees_postfirespp <- springs_trees_veg %>% 
  filter(status == 'live',
         pre_post_fire == 'postfire'|postFire == '1yr') %>% 
  group_by(plotid, species) %>% 
  summarise(Postfire_BA_m2ac = sum(BA_m2_acre)) %>% 
  mutate(plotidspp = paste(plotid, species)) %>% 
  select(plotidspp, Postfire_BA_m2ac )

springs_trees_BAspp <- left_join(springs_trees_prefirespp, springs_trees_postfirespp, by="plotidspp")

springs_trees_BAspp <- springs_trees_BAspp %>% 
  select(plotidspp, Prefire_BA_m2ac, Postfire_BA_m2ac) %>% 
  pivot_longer(-plotidspp, names_to = 'PrePostFire', values_to = 'BA_m2_acre') 

springs_trees_BAspp <- springs_trees_BAspp %>% 
  separate(plotidspp, c("plotid","species"))

springs_trees_BAspp <- left_join(springs_trees_BAspp, springs_trt, by="plotid")

springs_BAspp_plot <- ggplot(data=springs_trees_BAspp)+
  geom_boxplot(aes(x=species, y=BA_m2_acre, fill=PrePostFire))+
  facet_wrap(~ priorburn_rx, scales = "free_y") +
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Species")+
  ylab("Basal Area (m2/acre)")
springs_BAspp_plot

##################
################## DBH CLASS SIZE FOR VAL - Number of Trees / ha

#DBH class size
springs_trees_prefire <- springs_trees_veg %>% 
  filter(status == 'live',
         pre_post_fire == 'prefire') %>% 
  group_by(plotid, diamclass) %>% 
  add_count(plotid,diamclass, name="n_trees_diamclass") %>% 
  select(plotid, diamclass, n_trees_diamclass, priorburn_rx) %>% 
  mutate(plotid_diam = paste(plotid, diamclass)) %>% 
  group_by(plotid_diam) %>% 
  summarise(n_treesPreFire = mean(n_trees_diamclass)) %>% 
  mutate(n_treesPreFireHa = (n_treesPreFire*4.05)) ### 1/10 acre; 1 acre = 0.405 ha


springs_trees_postfire <- springs_trees_veg %>% 
  filter(status == "live",
         pre_post_fire == 'postfire'|postFire == '1yr') %>% 
  group_by(plotid, diamclass) %>% 
  add_count(plotid,diamclass, name="n_trees_diamclass") %>% 
  select(plotid, diamclass, n_trees_diamclass, priorburn_rx) %>% 
  mutate(plotid_diam = paste(plotid, diamclass)) %>% 
  group_by(plotid_diam) %>% 
  summarise(n_treesPostFire = mean(n_trees_diamclass)) %>% 
  mutate(n_treesPostFireha = (n_treesPostFire*4.05))

#merge 2019 & 2020 together
springs_trees_diamclass <- left_join(springs_trees_prefire,springs_trees_postfire, by=c("plotid_diam"))

springs_trees_diamclass <- springs_trees_diamclass %>% 
  separate(plotid_diam, c("plotid","diam_class"), sep=" ")

#add plot type
springs_trees_diamveg <- left_join(springs_trees_diamclass, springs_trt, by="plotid")
springs_trees_diamveg <- springs_trees_diamveg %>% 
  mutate(diam_type = paste(plotid,diam_class,priorburn_rx)) %>%   
  select(diam_type, n_treesPreFireHa, n_treesPostFireha) %>% 
  pivot_longer(cols= -diam_type, names_to= "condition", values_to ="n_trees_ha") %>% 
  separate(diam_type, c("plotid", "diam_class","priorburn_rx"),sep=" ")
  mutate(diam_type = paste(diam_class, plotType))


#add NRV values
NRV_diamclass <- NRV_diamclass %>% 
  filter(PlotType == 'MixedConifer')
springs_trees_NRV <- left_join(springs_trees_diamveg, NRV_diamclass, by="diam_class")

#CLEAN ER UP
springs_trees_NRV_clean <- springs_trees_NRV %>% 
  select(plotid, diam_class,priorburn_rx,condition,n_trees_ha, NRV, NRV_upper, NRV_lower) 

springs_trees_NRV_clean <- springs_trees_NRV_clean %>% 
  mutate(diam_class = factor(diam_class, levels=c('[<20]',
                                                      '[20-40]',
                                                      '[40-60]',
                                                      '[60-80]',
                                                      '[80-100]',
                                                      '[>100]'))) %>% 
  mutate(condition = factor (condition, levels=c('n_treesPreFireHa',
                                                 'n_treesPostFireha')))

export(springs_trees_NRV_clean, "SiteLocation/Springs_Fire/data/clean/springs_trees_NRVclean.csv")

###SEPERATE OUT NRV VALUES TO SEPARATE DATA FRAME trick to make sure it has a legend

NRV_springs_diamclass <- springs_trees_NRV_clean %>% 
  select(diam_class, priorburn_rx, starts_with("NRV")) %>% 
  distinct() %>%  #justs gives unique values for diamclass & plotTYpe
  mutate(NRV = 'range')

#####PLOT!!!!!!

springs_diamclass_plot <- ggplot(data=springs_trees_NRV_clean)+
  geom_boxplot(aes(x=diam_class, y=n_trees_ha, fill=condition))+
  geom_errorbar(data= NRV_springs_diamclass,
                aes(x=diam_class, ymin= NRV_lower, ymax=NRV_upper,
                    group = diam_class,
                    color= NRV), #link to group in NRV dataframe
                width=0.2,
                size = 1.5)+
  
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Diameter Class")+
  ylab("# Trees / Ha")
springs_diamclass_plot


ggsave( "SiteLocation/Springs_fire/images/springs_diamclassTrees.png",
        plot = springs_diamclass_plot)

VR_trees_sum <- VR_trees_NRVclean %>% 
  group_by(diam_class.x, plotType, condition) %>% 
  summarise(n_trees_ha = mean(n_trees_ha, na.rm=TRUE))



#################



