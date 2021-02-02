
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


VR_fuels <- read.csv("HolyGrail/data/clean/Val_CWD_FWD_final.csv")
VR_plottype <- read.csv("HolyGrail/data/raw/VR_plottype.csv")

VR_fuels <- VR_fuels %>% 
  select(plotid, fuel_type_size, mass_ton_acre) %>% 
  pivot_wider(names_from = "fuel_type_size", values_from= "mass_ton_acre") 

VR_fuels_sum <- VR_fuels %>% 
  mutate(total_fine = (mass_1hr+mass_10hr+mass_100hr),
         total_cwd = (mass_cwd_sound+mass_cwd_rotten)) %>% 
  select(plotid, mass_1hr, mass_10hr, mass_100hr, mass_cwd_sound, mass_cwd_rotten, total_fine, total_cwd, total_fuel) %>% 
  pivot_longer(cols=-plotid, names_to = "fuel_type", values_to= "mass_ton_acre")


VR_fuels_type <- left_join(VR_fuels_sum, VR_plottype, by="plotid")

export(VR_fuels_type, "HolyGrail/data/clean/VR_fuels_plottype.csv")




VR_fuelplot <- ggplot(data=VR_fuels_type, 
                      aes(x=fuel_type, y=mass_ton_acre, fill=fuel_type))+
  geom_boxplot()+
  geom_jitter(alpha=0.4)+
  facet_wrap(~ PlotType, scales = "free_y") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
VR_fuelplot

##########################CREATING CUSTOME FUEL MODEL################################
############# GROUP BY VEG TYPE

VR_fuels_grouped <- VR_fuels_type %>% 
  group_by(PlotType, fuel_type) %>% 
  summarize_all(mean, na.rm=TRUE)
  


