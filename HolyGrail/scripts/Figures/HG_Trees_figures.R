
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



getwd()

HG_Trees_totalPlot <- read.csv("HolyGrail/data/clean/trees/HG_trees_totalPlot.csv")
HG_Trees_diamclassD <- read.csv("HolyGrail/data/clean/trees/HG_trees_diamclass_sumDEAD.csv")
HG_Trees_diamclassL <- read.csv("HolyGrail/data/clean/trees/HG_trees_diamclass_sumLIVE.csv")
HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/trees/HG_trees_PlotSpeciesStatus.csv")
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN_youngetal.csv")

str(NRV_diamclass)

NRV_diamclass_YPMC <- NRV_diamclass %>% 
  rename(diamclass = diam_class) %>% 
  distinct() %>%  #justs gives unique values for diamclass & plotTYpe
  mutate(NRV = 'range')


HG_trees_NRV_clean <- HG_Trees_diamclass %>% 
  mutate(diamclass = factor(diamclass, levels=c('[<20]',
                                                '[20-40]',
                                                '[40-60]',
                                                '[60-80]',
                                                '[80-100]',
                                                '[>100]'))) %>% 
  mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                          "postfire")))


HG_diamclass_plot <- ggplot(data=HG_trees_NRV_clean)+
  geom_boxplot(aes(x=diamclass, y=n_trees_ha, fill=pre_post_fire))+
  geom_errorbar(data= NRV_diamclass_YPMC,
                aes(x=diamclass, ymin= NRV_lower, ymax=NRV_upper,
                    group = diamclass,
                    color= NRV), #link to group in NRV dataframe
                width=0.2,
                size = 1.5)+
  facet_wrap(~ site, scales = "free_y") +
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Diameter Class")+
  ylab("# Trees / Ha")
HG_diamclass_plot








HG_BAspp_live <- HG_Trees_plot_species %>% 
  filter(status == "LIVE")
HG_BAspp_plot <- ggplot(data=HG_BAspp_live)+
  geom_boxplot(aes(x=species, y=BA_m2_ha, fill=pre_post_fire))+
  facet_wrap(~ status, scales = "free_y") +
  theme_minimal()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=10))+
  xlab("Species")+
  ylab("Basal Area (m2/acre)")

HG_BAspp_plot
