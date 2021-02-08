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
library(ggpubr)


#########Pull springs fuels data from Holy Grail###############

#HG_fuels <- read.csv("HolyGrail/data/clean/HG_CWD_FWD_final.csv")
#HG_trt <- read.csv("HolyGrail/data/raw/HolyGrail_trt_utm.csv")

#Change plot id name to match fuels data
                #HG_trt <- HG_trt %>% 
                #  rename(plotid = plot_id)
                
                #export(HG_trt,"HolyGrail/data/raw/HolyGrail_trt_utm.csv" )

#Springs_trt <- HG_trt %>% 
#  filter(site=="springsfire") %>% 
#  select(plotid, burn, burn_unit)

#Springs_fuels_all <- HG_fuels %>% 
#  filter(site == "springsfire") 

#Springs_fuels_byunit <- left_join(Springs_fuels_all, Springs_trt, "plotid")

#export(Springs_fuels_byunit, "SiteLocation/Springs_Fire/data/clean/Springsfuels_byunit.csv")

#########################################

Springs_fuels_byunit <- read.csv("SiteLocation/Springs_Fire/data/clean/Springsfuels_byunit.csv")

#reorder fuel type to make logical sense
Springs_fuels_byunit <- Springs_fuels_byunit %>% 
  mutate(year_prepostfire = paste(year, pre_post_fire)) %>% 
  mutate(fuel_type_size = factor(fuel_type_size, levels=c('mass_1hr',
                                                      'mass_10hr',
                                                      'mass_100hr',
                                                      'mass_cwd_sound',
                                                      'mass_cwd_rotten',
                                                      'total_fuel',
                                                      'avg_duff',
                                                      'avg_litter',
                                                      'avg_fueldepth'
                                                    ))) %>% 
  mutate(year_prepostfire = factor(year_prepostfire, levels=c('2019 prefire',
                                                        '2019 postfire',
                                                        '2020 postfire'))) %>% 
  mutate(pre_post_fire = factor(pre_post_fire, levels=c('prefire',
                                                        'postfire'))) 


###########################
# PLOT ALL FUELS 

springs_fuelplot <- ggplot(data=Springs_fuels_byunit, 
                      aes(x=fuel_type_size, y=mass_ton_acre, fill=pre_post_fire))+
  geom_boxplot()+
  geom_jitter(alpha=0.4)+
  facet_wrap(~ burn_unit, scales = "free_y") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_fuelplot


#################
#COMPARE PRE AND POST SPRINGS FIRE BETWEEN TSLF

springs_fuels_burned <- Springs_fuels_byunit %>% 
  filter(burn == "yes") %>% 
  filter(burn_unit == "2007rx"|
           burn_unit == "2010rx"|
           burn_unit == "nopriorburn") %>% 
  filter(fuel_type_size == "mass_1hr"|
           fuel_type_size == "mass_10hr"|
           fuel_type_size == "mass_100hr"|
           fuel_type_size == "mass_cwd_sound"|
           fuel_type_size == "mass_cwd_rotten"|
           fuel_type_size == "total_fuel") %>% 
  filter(year_prepostfire == "2019 prefire"|
         year_prepostfire == "2019 postfire")

#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("prefire","postfire"))

springs_fuelplot_burned <- ggplot(data=springs_fuels_burned, 
                           aes(x=burn_unit, y=mass_ton_acre, fill=pre_post_fire))+
  geom_boxplot()+
  
  facet_wrap(~ fuel_type_size, scales = "free_y") +
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_fuelplot_burned

#### STATSSSSS


#######COMPARE CONTROL AND BURNED PLOTS

springs_fuels_control <- Springs_fuels_byunit %>% 
  filter(burn_unit == "2007rx"|
           burn_unit == "2010rx"|
           burn_unit == "nopriorburn"|
           burn_unit == "control") %>% 
  filter(fuel_type_size == "mass_1hr"|
           fuel_type_size == "mass_10hr"|
           fuel_type_size == "mass_100hr"|
           fuel_type_size == "mass_cwd_sound"|
           fuel_type_size == "mass_cwd_rotten"|
           fuel_type_size == "total_fuel") %>% 
  filter(year_prepostfire == "2020 postfire")

#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("yes","no"))

springs_fuelplot_control <- ggplot(data=springs_fuels_control, 
                                  aes(x=burn, y=mass_ton_acre, fill=burn))+
  geom_boxplot()+
  
  facet_wrap(~ fuel_type_size, scales = "free_y") +
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_fuelplot_control

################STATS####################################
group_by(springs_fuels_control, burn) %>% 
  summarise(
    count=n(),
    mean = mean(mass_mg_ha, na.rm=TRUE),
    sd=sd(mass_mg_ha, na.rm=TRUE)
  )


#compute analysis of variance

aov_totalcontrol<- aov(mass_ton_acre ~ burn, data=springs_fuels_control)
summary(aov_totalcontrol)

#multiple comparisons using multiple pairwise-comparisons between means of groups
TukeyHSD(aov_totalcontrol)
