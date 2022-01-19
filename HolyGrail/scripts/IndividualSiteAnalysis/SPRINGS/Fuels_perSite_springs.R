library(tidyverse)
library(tibble)
library(dplyr)
library(rio)
library(lme4)
library(viridis)
library(gtsummary)
library(ggplot2)
library(RColorBrewer)

library(data.table)

outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}


HG_fuelMass_tonAcre <- read.csv("HolyGrail/data/clean/HG_FuelMass_tonAcre_plot.csv")
trt_utm <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")

#############FILTER SITE & CLEAN HER UP
########

#Filter out site that you are interested in HERE#########
springs_fuel_totalPlot <- HG_fuelMass_tonAcre %>% 
  filter(site == "springsfire")
springs_fuel_totalPlot$plotid <- recode(springs_fuel_totalPlot$plotid, "springs1" = "springs01",
                                  "springs2"= "springs02",
                                  "springs3" ="springs03",
                                  "springs4"= "springs04",
                                  "springs5"= "springs05",
                                  "springs6"= "springs06",
                                  "springs7"= "springs07",
                                  "springs8"= "springs08",
                                  "springs9"= "springs09")


unique(springs_fuel_totalPlot$plotid)

##Add burn unit HERE
str(trt_utm)
springs_trt <- trt_utm %>% 
  filter(site == "springsfire") %>% 
  select(plotid, TSLF, burn_unit, burn)
export(springs_trt, "SiteLocation/Springs_Fire/data/clean/springs_trt.csv")
unique(springs_trt$plotid)
springs_fuel_totalPlot <- left_join(springs_fuel_totalPlot, springs_trt,
                                     by="plotid")


#reorder fuel type to make logical sense
springs_fuels_byunit <- springs_fuel_totalPlot %>% 
  filter(TSLF != "2013rx") %>% 
  mutate(fuelType = factor(fuelType, levels=c('mass_1hr',
                                                          'mass_10hr',
                                                          'mass_100hr',
                                                          'mass_cwd_sound',
                                                          'mass_cwd_rotten',
                                                          'total_fine',
                                                          'total_cwd',
                                                          'total_fuel',
                                                          'duff_depth_cm',
                                                          'litter_depth_cm'
  ))) %>% 
  mutate(pre_post_fire = factor(pre_post_fire, levels=c('prefire',
                                                        'postfire'))) %>% 
  mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                       "2007rx",
                                       "2010rx",
                                       "2013rx"))) %>% 
  mutate(PriorBurn = case_when(
                TSLF == '2007rx' ~ "priorburn",
                TSLF == '2010rx' ~ "priorburn",
                TSLF == '2013rx' ~ "priorburn",
                TSLF == 'nopriorburn' ~ "nopriorburn",
  ))


#########################
#PRE FIRE ONLY

springs_fuels_preburn <-  springs_fuels_byunit %>% 
  filter(burn == "yes") %>% 
  filter(pre_post_fire == "prefire") %>% 
  filter(fuelType== "mass_1hr"|
           fuelType== "mass_10hr"|
           fuelType == "mass_100hr"|
           fuelType == "total_cwd"|
           fuelType == "total_fuel") 

springs_fuelplot_preburn <- ggplot(data=springs_fuels_preburn, 
                                   aes(x=TSLF, y=mass_tonAcre, 
                                       fill=TSLF))+
  geom_boxplot()+
  facet_wrap(~ fuelType, scales = "free_y") +
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(size=16,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        strip.text.x = element_text(
          size = 14, face = "bold.italic"))+
  xlab("Prior Treatment")+
  ylab("Fuel Mass (Tons/Acre)")
springs_fuelplot_preburn

ggsave(plot=springs_fuelplot_preburn, "HolyGrail/figures/springs/springs_fuels_preburn_ALL.png")

compare_means(mass_tonAcre ~ TSLF, data = springs_fuels_preburn, 
                         group.by = "fuelType")

springs_fuels_preburn_summarize <- springs_fuels_preburn %>% 
  group_by(TSLF, fuelType) %>% 
  summarize(N    = length(mass_tonAcre),
            mean = mean(mass_tonAcre),
            sd   = sd(mass_tonAcre),
            se   = sd / sqrt(N))
springs_fuels_preburn_summarize



#################
#COMPARE PRE AND POST SPRINGS FIRE BETWEEN TSLF

springs_fuels_burned <- springs_fuels_byunit %>% 
  filter(burn == "yes") %>% 
  filter(fuelType == "total_fuel") %>% 
  filter(pre_post_fire== "prefire"|
          pre_post_fire=="postfire"&postTime=="1yr")

outlierReplace(springs_fuels_burned, "mass_tonAcre", 
               which(springs_fuels_burned$mass_tonAcre > 
                                        15), NA)

qplot(data = springs_fuels_burned, x = mass_tonAcre) 

springs_fuels_burned <- springs_fuels_burned %>% 
  drop_na(mass_tonAcre)

springs_fuels_burn_summarize <- springs_fuels_burned  %>% 
  group_by(pre_post_fire, TSLF, fuelType) %>% 
  summarize(N    = length(mass_tonAcre),
            mean = mean(mass_tonAcre),
            sd   = sd(mass_tonAcre),
            se   = sd / sqrt(N))
springs_fuels_burn_summarize



###PLOT ALL BURNED FUELS 
springs_fuelplot_burned <- ggplot(data=springs_fuels_burned, 
                                  aes(x=PriorBurn, y=mass_tonAcre, fill=pre_post_fire))+
  geom_boxplot()+
  facet_wrap(~ fuelType, scales = "free_y") +
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x=element_text(size=18, hjust=1),
        axis.title.x = element_blank(),
        plot.title = element_blank(),        #remove plot title
        strip.text.x = element_blank(),      #remove facet wrap title
        legend.title = element_blank(),
        legend.text = element_text(size=16))+     #remove legend title
  ylab("Fuel Mass (Tons/Acre)")
springs_fuelplot_burned

compare_means(mass_tonAcre ~ pre_post_fire, data = springs_fuels_burned, 
              group.by = "TSLF")

ggsave(plot=springs_fuelplot_burned, "HolyGrail/figures/springs/springs_fuels_burned_trtBASIC.tiff", dpi=600)







####################################
#######COMPARE CONTROL AND BURNED PLOTS

springs_fuels_control <- springs_fuels_byunit %>% 
  filter(burn =="yes" & pre_post_fire=="postfire" & postTime == "1yr"
         | burn =="no") %>% 
  #filter(TSLF != "2013rx") %>% 
  filter(fuelType == "mass_1hr"|
           fuelType == "mass_10hr"|
           fuelType== "mass_100hr"|
           fuelType == "mass_cwd_sound"|
           fuelType == "mass_cwd_rotten"|
           fuelType == "total_fuel") 

#add p-values comparing groups; specify comparisons I want


springs_fuelplot_control <- ggplot(data=springs_fuels_control, 
                                   aes(x=burn, y=mass_tonAcre, fill=burn))+
  geom_boxplot()+
  
  facet_wrap(~ fuelType, scales = "free_y") +
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text=element_text(size=16,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        strip.text.x = element_text(
          size = 14, face = "bold.italic"))+
  xlab("Burned by Springs Fire")+
  ylab("Fuel Mass (Tons/Acre)")
springs_fuelplot_control

compare_means(mass_tonAcre ~ burn, data = springs_fuels_control, 
              group.by = "fuelType")


ggsave(plot=springs_fuelplot_control, "HolyGrail/figures/springs/springs_fuels_control_BASIC.png")

#summarize
springs_fuels_control_summarize <- springs_fuels_control %>% 
  group_by(burn, pre_post_fire, fuelType) %>%
  drop_na(mass_tonAcre) %>% 
  summarize(N    = length(mass_tonAcre),
            mean = mean(mass_tonAcre),
            sd   = sd(mass_tonAcre),
            se   = sd / sqrt(N))
springs_fuels_control_summarize

################STATS####################################



#compute analysis of variance

aov_totalcontrol<- aov(mass_ton_acre ~ burn, data=springs_fuels_control)
summary(aov_totalcontrol)

#multiple comparisons using multiple pairwise-comparisons between means of groups
TukeyHSD(aov_totalcontrol)

