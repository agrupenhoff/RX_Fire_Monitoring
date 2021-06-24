
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




HG_Trees_totalPlot <- read.csv("HolyGrail/data/clean/HG_Trees_totalPlot.csv")
HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass.csv")
HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN.csv")



#separate plot id

HG_trees_aggregate_totalPlot <- HG_trees_aggregate_totalPlot %>% 
  separate(plotid_time, c("plotid","pre_post_fire","postFire"), sep="_",
           remove=TRUE,extra="merge")


HG_BAspp_plot <- ggplot(data=Val_trees_BAspp)+
  geom_boxplot(aes(x=species, y=BA_m2_acre, fill=PrePostThin))+
  facet_wrap(~ PlotType, scales = "free_y") +
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Species")+
  ylab("Basal Area (m2/acre)")
VR_BAspp_plot