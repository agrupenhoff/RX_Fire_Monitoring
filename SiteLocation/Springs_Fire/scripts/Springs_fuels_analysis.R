library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(kableExtra)
library(rio)
library(lme4)
library(ggpubr)
library(wesanderson)

springs_trt <- import("SiteLocation/Springs_Fire/data/clean/springs_trt.csv")
Springs_fuels_byunit <- read.csv("SiteLocation/Springs_Fire/data/clean/Springsfuels_byunit.csv")

#########Pull springs fuels data from Holy Grail###############

              #HG_fuels <- read.csv("HolyGrail/data/clean/HG_CWD_FWD_final.csv")
              
              #Springs_fuels_all <- HG_fuels %>% 
              #  filter(site == "springsfire") 
              
              #Springs_fuels_byunit <- left_join(Springs_fuels_all, springs_trt, "plotid")
              
              #export(Springs_fuels_byunit, "SiteLocation/Springs_Fire/data/clean/Springsfuels_byunit.csv")

#########################################




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


#########################
#PRE FIRE ONLY

springs_fuels_preburn <-  Springs_fuels_byunit %>% 
  filter(burn == "yes") %>% 
  filter(pre_post_fire == "prefire") %>% 
  filter(fuel_type_size == "mass_1hr"|
           fuel_type_size == "mass_10hr"|
           fuel_type_size == "mass_100hr"|
           fuel_type_size == "mass_cwd_sound"|
           fuel_type_size == "mass_cwd_rotten"|
           fuel_type_size == "total_fuel") 

springs_fuelplot_preburn <- ggplot(data=springs_fuels_preburn, 
                                  aes(x=priorburn_rx, y=mass_ton_acre, 
                                      fill=priorburn_rx))+
  geom_boxplot()+
  facet_wrap(~ fuel_type_size, scales = "free_y") +
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(size=12,angle = 45, hjust=1))
springs_fuelplot_preburn

springs_fuels_preburn_summarize <- springs_fuels_preburn %>% 
  group_by(priorburn_rx, fuel_type_size) %>% 
  summarize(N    = length(mass_ton_acre),
            mean = mean(mass_ton_acre),
            sd   = sd(mass_ton_acre),
            se   = sd / sqrt(N))
springs_fuels_preburn_summarize

sumFuels <- springs_fuels_burned %>% 
  filter(year_prepostfire == "2019 prefire"|
           year_prepostfire == "2019 postfire") %>% 
  group_by(fuel_type_size, PriorBurn, year_prepostfire) %>% 
  summarise(
    count=n(),
    mean = mean(mass_ton_acre, na.rm=TRUE))




#################
#COMPARE PRE AND POST SPRINGS FIRE BETWEEN TSLF

springs_fuels_burned <- Springs_fuels_byunit %>% 
  filter(burn == "yes") %>% 
  filter(fuel_type_size == "mass_1hr"|
           fuel_type_size == "mass_10hr"|
           fuel_type_size == "mass_100hr"|
           fuel_type_size == "mass_cwd_sound"|
           fuel_type_size == "mass_cwd_rotten"|
           fuel_type_size == "total_fuel") 


###PLOT ALL BURNED FUELS 
springs_fuelplot_burned <- ggplot(data=springs_fuels_burned, 
                           aes(x=priorburn_rx, y=mass_ton_acre, fill=pre_post_fire))+
  geom_boxplot()+
  facet_wrap(~ fuel_type_size, scales = "free_y") +
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()
springs_fuelplot_burned

springs_fuelplot_burned <- ggplot(data=springs_fuels_burned, 
                                  aes(x=PriorBurn, y=mass_ton_acre, fill=pre_post_fire))+
  geom_boxplot()+
  facet_wrap(~ fuel_type_size, scales = "free_y") +
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()
springs_fuelplot_burned

sumFuels <- springs_fuels_burned %>% 
  filter(year_prepostfire == "2019 prefire"|
           year_prepostfire == "2019 postfire") %>% 
  group_by(fuel_type_size, PriorBurn, year_prepostfire) %>% 
  summarise(
    count=n(),
    mean = mean(mass_ton_acre, na.rm=TRUE))




#Just total Fuel
springs_totalfuels_burned <- springs_fuels_burned %>% 
  filter(fuel_type_size == "total_fuel")

springs_TOTplot_burned <- ggplot(data=springs_totalfuels_burned, 
                                  aes(x=priorburn_rx, y=mass_ton_acre, fill=pre_post_fire))+
  geom_boxplot()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))+
  stat_compare_means(aes(label = ..p.signif..))
springs_TOTplot_burned

springs_TOTplot_burned2 <- ggplot(data=springs_totalfuels_burned, 
                                 aes(x=PriorBurn, y=mass_ton_acre, fill=pre_post_fire))+
  geom_boxplot()+
  facet_wrap(~ fuel_type_size, scales = "free_y") +
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  stat_compare_means(aes(label = ..p.signif..))
springs_TOTplot_burned2

#### STATSSSSS

##BY TSLF
  totalfuels_burn <- lmer(mass_ton_acre ~ priorburn_rx+pre_post_fire +
                          (1|burn_unit),  data = springs_totalfuels_burned)
    summary(totalfuels_burn)
              
    TOTfuels_burn.emm <- emmeans(totalfuels_burn, 
                             specs = pairwise ~ priorburn_rx:pre_post_fire)
    plot(TOTfuels_burn.emm)
             
##BY BURN OR NO BURN
    totalfuels_burn_basic <- lmer(mass_ton_acre ~ PriorBurn+pre_post_fire +
                              (1|burn_unit),  data = springs_totalfuels_burned)
    summary(totalfuels_burn_basic)
    
    TOTfuels_burnBASIC.emm <- emmeans(totalfuels_burn_basic, 
                                 specs = pairwise ~ PriorBurn:pre_post_fire)
    plot(TOTfuels_burnBASIC.emm)

####################################
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
