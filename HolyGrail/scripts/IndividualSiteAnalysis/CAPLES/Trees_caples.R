
library(tidyverse)
library(tibble)
library(dplyr)


HG_Trees_totalPlot <- read.csv("HolyGrail/data/clean/HG_Trees_totalPlot.csv")
HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass.csv")
HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN_youngetal.csv")
trt_utm <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")

#############FILTER SITE & CLEAN HER UP
########

#Filter out site that you are interested in HERE
caples_Trees_totalPlot <- HG_Trees_totalPlot %>% 
  filter(site == "caples")
caples_Trees_diamclass <- HG_Trees_diamclass %>% 
  filter(site == "caples")
caples_Trees_plot_species <- HG_Trees_plot_species %>% 
  filter(site == "caples")

##Add burn unit HERE


#match springs to NRV
NRV_diamclass_YPMC <- NRV_diamclass %>% 
  rename(diamclass = diam_class) %>% 
  distinct() %>%  #justs gives unique values for diamclass & plotTYpe
  mutate(NRV = 'range')

unique(caples_Trees_diamclass$year)
caples_trees_NRV_clean <- caples_Trees_diamclass %>% 
  filter(status == "LIVE") %>% 
  filter(year == "2013-2018"|
           year == "2020") %>% 
  mutate(diamclass = factor(diamclass, levels=c('[<20]',
                                                '[20-40]',
                                                '[40-60]',
                                                '[60-80]',
                                                '[80-100]',
                                                '[>100]'))) %>% 
  mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                          "postfire")))


caples_diamclass_plot <- ggplot(data=caples_trees_NRV_clean)+
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
caples_diamclass_plot


