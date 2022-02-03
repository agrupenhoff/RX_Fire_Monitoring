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
library(cowplot) #for manuscript ready figures
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(emmeans)
library(nlme)

outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}


HG_fuelMass_tonsHA_plot <- read.csv("HolyGrail/data/clean/fuels/HG_FuelMass_tonHA_plot.csv")
        HG_fuelMass_tonsHA_plot$site <- tolower(HG_fuelMass_tonsHA_plot$site)
HG_fuel_consumption_HA <- read.csv("HolyGrail/data/clean/fuels/HG_FuelMass_tonHA_consumption.csv")
        HG_fuel_consumption_HA$site <- tolower(HG_fuel_consumption_HA$site)
trt_utm <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")
        trt_utm$site <- tolower(trt_utm$site)

str(trt_utm)
unique(trt_utm$burn_year)

#############CLEAN UP TRT FILE TO RUN ANALYSIS
########


###SELECT ASPECTS OF trt wanted

trt_utm_TSLF_cut <- trt_utm %>% 
  mutate(site_plotid = paste(site, plotid, sep="_")) %>% 
  select(site_plotid, burn, burn_year, num_total_rx, num_prior_rx, TSLF_rx_only, TSLF_rx_wildfire)

HG_fuel_consumption_HA <- HG_fuel_consumption_HA %>% 
  mutate(site_plotid = paste(site, plotid, sep="_"))

HG_fuelMass_HA <- HG_fuelMass_tonsHA_plot %>% 
  mutate(site_plotid = paste(site, plotid, sep="_"))

##COMBINE FUEL DATA WITH TRT

HG_fuel_consumptionHA_trt <- left_join(HG_fuel_consumption_HA, trt_utm_TSLF_cut, by="site_plotid")
HG_fuelMass_HA_trt <- left_join(HG_fuelMass_HA, trt_utm_TSLF_cut, by="site_plotid")


#reorder fuel type to make logical sense
HG_fuel_consumptionHA_trt <- HG_fuel_consumptionHA_trt %>% 
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
  ))) 

HG_fuelMass_HA_trt <- HG_fuelMass_HA_trt %>% 
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
  mutate(TSLF_rx_wildfire = factor(TSLF_rx_wildfire, levels = c('2',
                                                                '5',
                                                                '6',
                                                                '9',
                                                                '12',
                                                                '>50')))



###fuel load by different burn times
##PREFIRE FUELS
HG_fuelMass_HA_prefire <- HG_fuelMass_HA_trt %>% 
  filter(pre_post_fire == "prefire") %>% 
  drop_na(fuelType, TSLF_rx_wildfire)

HG_fuels_prePLOT <- ggplot(data=HG_fuelMass_HA_prefire, 
                               aes(x=TSLF_rx_wildfire, y=mass_tonHA, 
                                   fill=TSLF_rx_wildfire))+
  geom_boxplot()+
  #geom_jitter(alpha=0.6)+
  facet_wrap(~ fuelType, scales = "free_y") +
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2")+
  xlab("Pre/Post Fire")+
  ylab("Fuel Mass (Tons/Acre)")
HG_fuels_prePLOT



#BURNED PLOTS ONLY
HG_fuelMass_HA_burned <- HG_fuelMass_HA_trt %>% 
  filter(pre_post_fire == "prefire"|
           pre_post_fire == "postfire" & postTime == "immediate") %>% 
  mutate(pre_post_Time = paste(pre_post_fire, postTime, sep = "_")) %>% 
  filter(burn == "yes") %>% 
  drop_na(fuelType)

HG_fuels_prepostPLOT <- ggplot(data=HG_fuelMass_HA_burned, 
                                   aes(x=pre_post_fire, y=mass_tonHA, 
                                       fill=pre_post_fire))+
  geom_violin()+
  #geom_jitter(alpha=0.6)+
  facet_wrap(~ fuelType, scales = "free_y") +
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2")+
  xlab("Pre/Post Fire")+
  ylab("Fuel Mass (Tons/Acre)")
HG_fuels_prepostPLOT


##BURNED PLOTS BY TSLF
HG_fuelMass_HA_burned_TOTAL <-  HG_fuelMass_HA_burned %>% 
  filter(fuelType == "total_cwd") 
  
  
HG_fuels_prepostPLOT2 <- ggplot(data=HG_fuelMass_HA_burned_TOTAL, 
                               aes(x=pre_post_fire, y=mass_tonHA, 
                                   fill=TSLF_rx_wildfire))+
  geom_boxplot()+
  #geom_jitter(alpha=0.6)+
 #facet_wrap(~ site, scales = "free_y") +
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2")+
  xlab("Pre/Post Fire")+
  ylab("Fuel Mass (Tons/Acre)")
HG_fuels_prepostPLOT2



#########################
##Burned Plot Consumption

consumption_total <-  HG_fuel_consumptionHA_trt%>% 
  filter(fuelType =="total_fuel")

model_totalFuel <- lme4::lmer(consumption_immediate ~ prefire_NA + (1|site),
                              data = consumption_total)

summary(model_totalFuel)
sjPlot::plot_model(model_totalFuel)
sjPlot:: tab_model(model_totalFuel)

##GET FIXED EFFECTS
effects_consumption <- effects::effect(term= "prefire_NA", mod= model_totalFuel)
summary(effects_consumption)
x_consum <- as.data.frame(effects_consumption)



##PLOT
consumption_plot <- ggplot() + 
  #2
  geom_point(data=consumption_total, aes(prefire_NA, consumption_immediate)) + 
  #3
  geom_point(data=x_consum, aes(x=prefire_NA, y=fit), color="blue") +
  #4
  geom_line(data=x_consum, aes(x=prefire_NA, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_consum , aes(x=prefire_NA, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="prefire Fuels (centered & scaled)", y="immediate consumption")

consumption_plot





####LOOK AT CONSUMPTION AS A FUNCTION OF TSLF
consumption_total <-  HG_fuel_consumptionHA_trt%>% 
  filter(fuelType =="total_fuel")

ggplot(consumption_total, aes(x=TSLF_rx_wildfire, y=consumption_immediate))+
    stat_summary(fun.data = mean_cl_boot, size=2)+
    ylim(c(0,1))

ggplot(consumption_total,aes(x=prefire_NA,y=consumption_immediate, color=TSLF_rx_wildfire))+
  stat_sum(alpha=0.3)+
  scale_y_log10()+
  scale_size_continuous(breaks=c(2,6,10),range=c(2,7))+
  geom_smooth(method="lm")


consumption <- lmer(consumption_immediate ~ prefire_NA + TSLF_rx_wildfire + (1|site),
                    data = consumption_total)
em <- emmeans(consumption, "TSLF_rx_wildfire")
contrast(em)
contrast(em, adjust = "bonferroni")

