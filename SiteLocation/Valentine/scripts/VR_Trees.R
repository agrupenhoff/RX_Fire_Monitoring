

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

###### PULL JUST VALENTINE RESERVE FROM HOLY GRAIL

                  HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass.csv")
                  HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
                  NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN.csv")
                  
                  VR_plottype <- import("SiteLocation/Valentine/data/clean/VR_plottype.csv")
                  VR_trees_veg <- import("SiteLocation/Valentine/data/clean/VR_trees_veg.csv")
                  
                  
                  #VR_trees_diam <- HG_Trees_diamclass %>% 
                  #  filter(site == 'Valentine') 
                  
                  ##VR_trees_diam$dbh_cm <- as.numeric(VR_trees_diam$dbh_cm)
                  #str(VR_trees_diam)
                  #VR_trees_veg <-  left_join(VR_trees_diam, VR_plottype, by='plotid')
                  
                  #VR_trees_plotspp <- HG_Trees_plot_species %>% 
                  #  filter(str_detect(plotid_time, "VR"))
                  
                  #export(VR_trees_veg, "SiteLocation/Valentine/data/clean/VR_trees_veg.csv")
                  #export(VR_plottype, "SiteLocation/Valentine/data/clean/VR_plottype.csv")
                  
#################
###########CHANGE BY SPECIES BY VEG TYPE                  
      #create function to calculate basal area (m^2)
                  
      basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2
                  
VR_trees_veg <- VR_trees_veg %>% 
  mutate(BA_m2 = basal.area.fn(dbh_cm),
         BA_m2_acre = BA_m2*10)

Val_trees_2019spp <- VR_trees_veg %>% 
  filter(status == 'L') %>% 
  group_by(plotid, species) %>% 
  summarise(Prethin_BA_m2ac = sum(BA_m2_acre)) %>% 
  mutate(plotidspp = paste(plotid, species)) %>% 
  select(plotidspp, Prethin_BA_m2ac )

Val_trees_2020spp <- VR_trees_veg %>% 
  filter(status == 'L',
         presentPostThin == 'yes') %>% 
  group_by(plotid, species) %>% 
  summarise(Postthin_BA_m2ac = sum(BA_m2_acre)) %>% 
  mutate(plotidspp = paste(plotid, species)) %>% 
  select(plotidspp, Postthin_BA_m2ac )

Val_trees_BAspp <- left_join(Val_trees_2019spp, Val_trees_2020spp, by="plotidspp")

Val_trees_BAspp <- Val_trees_BAspp %>% 
  select(plotidspp, Prethin_BA_m2ac, Postthin_BA_m2ac) %>% 
  pivot_longer(-plotidspp, names_to = 'PrePostThin', values_to = 'BA_m2_acre') 

Val_trees_BAspp <- Val_trees_BAspp %>% 
    separate(plotidspp, c("plotid","species"))

Val_trees_BAspp <- left_join(Val_trees_BAspp, VR_plottype, by="plotid")

VR_BAspp_plot <- ggplot(data=Val_trees_BAspp)+
  geom_boxplot(aes(x=species, y=BA_m2_acre, fill=PrePostThin))+
  facet_wrap(~ PlotType, scales = "free_y") +
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Species")+
  ylab("Basal Area (m2/acre)")
VR_BAspp_plot
                  
##################
################## DBH CLASS SIZE FOR VAL - Number of Trees / ha

#DBH class size
Val_trees_live2019 <- VR_trees_veg %>% 
  filter(status == 'L') %>% 
  group_by(plotid, diamclass) %>% 
  add_count(plotid,diamclass, name="n_trees_diamclass") %>% 
  select(plotid, diamclass, n_trees_diamclass, PlotType) %>% 
  mutate(plotid_diam = paste(plotid, diamclass)) %>% 
  group_by(plotid_diam) %>% 
  summarise(n_trees2019 = mean(n_trees_diamclass)) %>% 
  mutate(n_trees2019ha = (n_trees2019*4.05)) ### 1/10 acre; 1 acre = 0.405 ha


val_trees_live2020 <- VR_trees_veg %>% 
  filter(status == "L",
         presentPostThin == "yes") %>% 
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

val_trees_NRV_clean <- val_trees_NRV_clean %>% 
  mutate(diam_class.x = factor(diam_class.x, levels=c('[<20]',
                                                      '[20-40]',
                                                      '[40-60]',
                                                      '[60-80]',
                                                      '[80-100]',
                                                      '[>100]')))

###SEPERATE OUT NRV VALUES TO SEPARATE DATA FRAME trick to make sure it has a legend

NRV_val_diamclass <- val_trees_NRV_clean %>% 
  select(diam_class.x, plotType, starts_with("NRV")) %>% 
  distinct() %>%  #justs gives unique values for diamclass & plotTYpe
  mutate(NRV = 'range')

#####PLOT!!!!!!

VR_diamclass_plot <- ggplot(data=val_trees_NRV_clean)+
  geom_boxplot(aes(x=diam_class.x, y=n_trees_ha, fill=condition))+
  geom_errorbar(data= NRV_val_diamclass,
                aes(x=diam_class.x, ymin= NRV_lower, ymax=NRV_upper,
                    group = diam_class.x,
                    color= NRV), #link to group in NRV dataframe
                 width=0.2,
                 size = 1.5)+
  facet_wrap(~ plotType, scales = "free_y") +
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Diameter Class")+
  ylab("# Trees / Ha")
VR_diamclass_plot

ggsave(VR_diamclass_plot, "HolyGrail/graphs/Valentine/VR_diamclassTrees.png")





#################



