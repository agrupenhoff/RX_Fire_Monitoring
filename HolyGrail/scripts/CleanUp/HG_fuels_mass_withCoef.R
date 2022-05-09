library(tidyverse)
library(tibble)
library(dplyr)
library(rio)


HG_fuels_data <- read.csv("HolyGrail/data/clean/fuels/HG_FWD_CWD_final.csv",
                          stringsAsFactors = TRUE)
HG_fuels_data$site <- tolower(HG_fuels_data$site)
HG_fuels_data$plotid <- tolower(HG_fuels_data$plotid)
unique(HG_fuels_data$plotid)

HGtrees_basalareaSpeciesProportion <- read.csv("HolyGrail/data/clean/trees/HGtrees_basalareaSpeciesProportion.csv")
unique(HGtrees_basalareaSpeciesProportion$spp)


##############################
#####PLOT SPECIFIC COEFF!!

#for fuels equations 
QMD <- read.csv("HolyGrail/data/clean/fuels/QMD.csv")
SEC <- read.csv("HolyGrail/data/clean/fuels/SEC.csv")
constant <- read.csv("HolyGrail/data/clean/fuels/constant.csv")
SG <- read.csv("HolyGrail/data/clean/fuels/specific_gravity.csv")
litter_duff_coef <- read.csv("HolyGrail/data/clean/fuels/litter_duff_coef.csv")
unique(litter_duff_coef$spp)

#average species specific coeff for each tree species contributing to fuel, per plot

#coef (species) = pBA * coef 
#coef (plot) = sum (coef(species))

##GET COEFFICIENTS FOR FUELS EQUATIONS

              #LITTER DUFFFFFF
              HGtrees_BAcoef_litterduff <- left_join(HGtrees_basalareaSpeciesProportion, litter_duff_coef,
                                          by = "spp")
              
              HGtrees_BAcoef_litterduff <- HGtrees_BAcoef_litterduff %>% 
                mutate(coef_spp_litter = pBA_m2_ha * litter_coeff,
                       coef_spp_duff = pBA_m2_ha * duff_coeff,
                       coef_spp_dufflitter = pBA_m2_ha * litterduff_coeff) 
              
              HGtrees_BAcoef_litterduffSUM <- HGtrees_BAcoef_litterduff %>% 
                group_by(plotid_time) %>% 
                summarise(coef_plot_litter = sum(coef_spp_litter),
                          coef_plot_duff = sum(coef_spp_duff),
                          coef_plot_litterduff = sum(coef_spp_dufflitter)) %>% 
                separate(plotid_time, c("site","plotid","postFire","year"), sep="_",
                         remove=TRUE,extra="merge") %>% 
                mutate(plotid_prepostfire = paste(plotid, postFire, sep="_"),
                       site_plotid = paste(site, plotid, sep="_")) %>% 
                select(site_plotid, coef_plot_litter, coef_plot_duff, coef_plot_litterduff)
              
              export(HGtrees_BAcoef_litterduffSUM, "HolyGrail/data/clean/fuels/HGtrees_BAcoef_litterduff_sum.csv")

              #QMD: QUADRATIC MEAN DIAMETER
              HGtrees_BAcoef_QMD <- left_join(HGtrees_basalareaSpeciesProportion, QMD,
                                                     by = "spp")
              
              HGtrees_BAcoef_QMD <- HGtrees_BAcoef_QMD %>% 
                mutate(qmd_spp_x1h = pBA_m2_ha * x1h,
                       qmd_spp_x10h = pBA_m2_ha * x10h,
                       qmd_spp_x100h = pBA_m2_ha * x100h,
                       qmd_spp_x1000h = pBA_m2_ha * x1000h) 
              
              HGtrees_BAcoef_QMDsum <- HGtrees_BAcoef_QMD %>% 
                group_by(plotid_time) %>% 
                summarise(qmd_plot_x1h = sum(qmd_spp_x1h),
                          qmd_plot_x10h = sum(qmd_spp_x10h),
                          qmd_plot_x100h = sum(qmd_spp_x100h),
                          qmd_plot_x1000h = sum(qmd_spp_x1000h)) %>% 
                separate(plotid_time, c("site","plotid","postFire","year"), sep="_",
                         remove=TRUE,extra="merge") %>% 
                mutate(plotid_prepostfire = paste(plotid, postFire, sep="_"),
                       site_plotid = paste(site, plotid, sep="_")) %>% 
                select(site_plotid, qmd_plot_x1h, qmd_plot_x10h, qmd_plot_x100h, qmd_plot_x1000h)
              
              export(HGtrees_BAcoef_QMDsum, "HolyGrail/data/clean/fuels/HGtrees_BAcoef_QMD_sum.csv")
              
              #SEC: secant of acute angle
              HGtrees_BAcoef_SEC <- left_join(HGtrees_basalareaSpeciesProportion, SEC,
                                              by = "spp")
              
              HGtrees_BAcoef_SEC <- HGtrees_BAcoef_SEC %>% 
                mutate(sec_spp_x1h = pBA_m2_ha * x1h,
                       sec_spp_x10h = pBA_m2_ha * x10h,
                       sec_spp_x100h = pBA_m2_ha * x100h,
                       sec_spp_x1000h = pBA_m2_ha * x1000h) 
              
              HGtrees_BAcoef_SECsum <- HGtrees_BAcoef_SEC %>% 
                group_by(plotid_time) %>% 
                summarise(sec_plot_x1h = sum(sec_spp_x1h),
                          sec_plot_x10h = sum(sec_spp_x10h),
                          sec_plot_x100h = sum(sec_spp_x100h),
                          sec_plot_x1000h = sum(sec_spp_x1000h)) %>% 
                separate(plotid_time, c("site","plotid","postFire","year"), sep="_",
                         remove=TRUE,extra="merge") %>% 
                mutate(plotid_prepostfire = paste(plotid, postFire, sep="_"),
                       site_plotid = paste(site, plotid, sep="_")) %>% 
                select(site_plotid, sec_plot_x1h, sec_plot_x10h, sec_plot_x100h, sec_plot_x1000h)
              
              export(HGtrees_BAcoef_SECsum, "HolyGrail/data/clean/fuels/HGtrees_BAcoef_SEC_sum.csv")
              
              #SG: specific gravity 
              HGtrees_BAcoef_SG <- left_join(HGtrees_basalareaSpeciesProportion, SG,
                                              by = "spp")
              
              HGtrees_BAcoef_SG <- HGtrees_BAcoef_SG %>% 
                mutate(SG_spp_x1h = pBA_m2_ha * x1h,
                       SG_spp_x10h = pBA_m2_ha * x10h,
                       SG_spp_x100h = pBA_m2_ha * x100h,
                       SG_spp_x1000s = pBA_m2_ha * x1000s) 
              
              HGtrees_BAcoef_SGsum <- HGtrees_BAcoef_SG %>% 
                group_by(plotid_time) %>% 
                summarise(SG_plot_x1h = sum(SG_spp_x1h),
                          SG_plot_x10h = sum(SG_spp_x10h),
                          SG_plot_x100h = sum(SG_spp_x100h),
                          SG_plot_x1000s = sum(SG_spp_x1000s)) %>% 
                separate(plotid_time, c("site","plotid","postFire","year"), sep="_",
                         remove=TRUE,extra="merge") %>% 
                mutate(plotid_prepostfire = paste(plotid, postFire, sep="_"),
                       site_plotid = paste(site, plotid, sep="_")) %>% 
                select(site_plotid, SG_plot_x1h, SG_plot_x10h, SG_plot_x100h, SG_plot_x1000s)
              
              export(HGtrees_BAcoef_SGsum, "HolyGrail/data/clean/fuels/HGtrees_BAcoef_SG_sum.csv")

## fuel analysis

##Initial fuels data 

#0-3" = (11.64 * n * d^2 * s * a * c)/ (Nl)
#CWD = (11.64 * n * sum(d^2) * s * a * c)/ (Nl)

#slope correction = (1+(presprings_fuelslope/100)^2)^(1/2)
#convert sq cm to sq in: * 0.155


#Calculate Mass in tons/acre

#add all coefficients
HG_fuels_mass <- HG_fuels_data %>% 
  mutate(site_plotid = paste(site, plotid, sep="_"))

HG_fuels_mass_litterduff <- left_join(HG_fuels_mass, HGtrees_BAcoef_litterduffSUM, by = "site_plotid")
HG_fuels_mass_QMD <- left_join(HG_fuels_mass_litterduff, HGtrees_BAcoef_QMDsum, by = "site_plotid")
HG_fuels_mass_SEC <- left_join(HG_fuels_mass_QMD, HGtrees_BAcoef_SECsum, by = "site_plotid")
HG_fuels_mass_coef <- left_join(HG_fuels_mass_SEC, HGtrees_BAcoef_SGsum, by = "site_plotid")

#replace NA with average values
HG_fuels_mass_coef <- HG_fuels_mass_coef %>% 
  mutate(coef_plot_litter = replace_na(coef_plot_litter, 0.363),
         coef_plot_duff = replace_na(coef_plot_duff, 1.75),
         coef_plot_litterduff = replace_na(coef_plot_litterduff, 1.624),
         qmd_plot_x1h = replace_na(qmd_plot_x1h, 0.12),
         qmd_plot_x10h = replace_na(qmd_plot_x10h, 1.28),
         qmd_plot_x100h = replace_na(qmd_plot_x100h, 14.52),
         qmd_plot_x1000h = replace_na(qmd_plot_x1000h, 127.24),
         sec_plot_x1h = replace_na(sec_plot_x1h, 1.03),
         sec_plot_x10h = replace_na(sec_plot_x10h, 1.02),
         sec_plot_x100h = replace_na(sec_plot_x100h, 1.02),
         sec_plot_x1000h = replace_na(sec_plot_x1000h, 1.02),
         SG_plot_x1h = replace_na(SG_plot_x1h, 0.58),
         SG_plot_x10h = replace_na(SG_plot_x10h, 0.57),
         SG_plot_x100h = replace_na(SG_plot_x100h, 0.53),
         SG_plot_x1000s = replace_na(SG_plot_x1000s, 0.47))

export(HG_fuels_mass_coef, "HolyGrail/data/clean/fuels/HG_FuelMass_withCoefficient.csv")

######################
####### CALCULATE FUEL MASS TONS/HA
HG_fuels_Mass_tonsHA <- HG_fuels_mass_coef %>% 
  mutate(slopecorrection = (1+(slope_percent/100)^2)^(1/2)) %>% 
  mutate(mass_1hr =(1.234 * count_x1h * qmd_plot_x1h * sec_plot_x1h * slopecorrection * SG_plot_x1h)/ (x1h_length_m),
         mass_10hr=(1.234 * count_x10h * qmd_plot_x10h * sec_plot_x10h * slopecorrection * SG_plot_x10h)/ (x10h_length_m),
         mass_100hr=(1.234 * count_x100h * qmd_plot_x100h * sec_plot_x100h * slopecorrection * SG_plot_x100h)/ (x100h_length_m), 
         #no QMD since i have diameter measurements
         mass_cwd_sound =(1.234 * sum_d2_1000s_cm2 * sec_plot_x1000h * slopecorrection * SG_plot_x1000s) / (x1000h_length_m*4),   
         mass_cwd_rotten=(1.234 * sum_d2_1000r_cm2 * sec_plot_x1000h * slopecorrection * 0.36) / (x1000h_length_m*4),
         duff_load_kgm2 = (coef_plot_duff * duff_depth_cm),
         litter_load_kgm2 = (coef_plot_litter * litter_depth_cm)) %>% 
  #convert litter duff from kg/m2 to mg/ha
  mutate(duff_load = (duff_load_kgm2 * 4.4609),
         litter_load = (litter_load_kgm2 * 4.4609)) %>% 
  select(site, plotid, year, pre_post_fire, postTime, azimuth, 
         duff_depth_cm, duff_load, litter_depth_cm, litter_load, fuel_depth_cm,
         mass_1hr, mass_10hr, mass_100hr, mass_cwd_sound, mass_cwd_rotten) %>% 
  mutate(total_fine = mass_1hr+mass_10hr+mass_100hr,
         total_cwd = mass_cwd_rotten+mass_cwd_sound,
         total_fuel = mass_1hr+mass_10hr+mass_100hr+mass_cwd_sound+mass_cwd_rotten+litter_load+duff_load) %>% 
  mutate(site_plotid_time = paste(site, plotid, year, pre_post_fire, postTime, sep="_"))


#1 ton (US) = 0.907 megagram
HG_fuels_Mass_mgha <- HG_fuels_Mass_tonsHA %>% 
  mutate(mass_1hr = mass_1hr/1.102,
         mass_10hr = mass_10hr/1.102,
         mass_100hr = mass_100hr/1.102,
         mass_cwd_sound = mass_cwd_sound/1.102,
         mass_cwd_rotten = mass_cwd_rotten/1.102,
         duff_load = duff_load/1.102,
         litter_load = litter_load/1.102,
         total_fine = total_fine/1.102,
         total_cwd = total_cwd/1.102,
         total_fuel = total_fuel/1.102)


#Average across each plot (Ton/HA)
HG_fuels_tonHA_plot <- HG_fuels_Mass_tonsHA %>% 
  group_by(site_plotid_time) %>% 
  summarize(mass_1hr = mean(mass_1hr),
            mass_10hr = mean(mass_10hr),
            mass_100hr = mean(mass_100hr),
            mass_cwd_sound = mean(mass_cwd_sound),
            mass_cwd_rotten = mean(mass_cwd_rotten),
            total_fine = mean(total_fine),
            total_cwd = mean(total_cwd),
            total_fuel = mean(total_fuel),
            duff_load = mean(duff_load),
            litter_load = mean(litter_load),
            litter_depth_cm = mean(litter_depth_cm),
            duff_depth_cm = mean(duff_depth_cm),
            fuel_depth_cm = mean(fuel_depth_cm)) %>% 
  pivot_longer(-site_plotid_time, names_to="fuelType", values_to="mass_tonsHA") %>% 
  separate(site_plotid_time, c("site","plotid", "year", "pre_post_fire", "postTime"), 
           sep="_") %>% 
  drop_na(mass_tonsHA)


export(HG_fuels_tonHA_plot, "HolyGrail/data/clean/fuels/HG_FuelMass_tonHA_coef.csv")


#Average across each plot (megagram/HA)
HG_fuels_mgha_plot <- HG_fuels_Mass_mgha %>% 
  group_by(site_plotid_time) %>% 
  summarize(mass_1hr = mean(mass_1hr),
            mass_10hr = mean(mass_10hr),
            mass_100hr = mean(mass_100hr),
            mass_cwd_sound = mean(mass_cwd_sound),
            mass_cwd_rotten = mean(mass_cwd_rotten),
            total_fine = mean(total_fine),
            total_cwd = mean(total_cwd),
            total_fuel = mean(total_fuel),
            duff_load = mean(duff_load),
            litter_load = mean(litter_load),
            litter_depth_cm = mean(litter_depth_cm),
            duff_depth_cm = mean(duff_depth_cm),
            fuel_depth_cm = mean(fuel_depth_cm)) %>% 
  pivot_longer(-site_plotid_time, names_to="fuelType", values_to="mass_mgha") %>% 
  separate(site_plotid_time, c("site","plotid", "year", "pre_post_fire", "postTime"), 
           sep="_") %>% 
  drop_na(mass_mgha)


export(HG_fuels_mgha_plot, "HolyGrail/data/clean/fuels/HG_FuelMass_mgHA_coef.csv")

############################
###############CONSUMPTION

str(HG_fuels_mgha_plot)
HG_fuels_mgha_plot$mass_mgha <- as.numeric(HG_fuels_mgha_plot$mass_mgha)

HG_fuels_mgha_consumption <- HG_fuels_mgha_plot %>% 
  filter (pre_post_fire == "prefire"|
            pre_post_fire == "postfire" & postTime == "immediate") %>% 
  mutate(time_fire = paste(pre_post_fire, postTime, sep = "_")) %>% 
  mutate(plotID_fuel = paste(site, plotid, fuelType, sep=" ")) %>% 
  select(plotID_fuel, time_fire, mass_mgha) 


HG_fuels_mgha_consumption_wider <- HG_fuels_mgha_consumption %>% 
  pivot_wider(names_from = "time_fire", values_from = "mass_mgha")  %>% 
  unnest(cols= c(prefire_, postfire_immediate)) %>% 
  mutate(consumption_immediate = prefire_ - postfire_immediate) %>% 
  drop_na(consumption_immediate) %>% 
  separate(plotID_fuel, c("site","plotid", "fuelType"), 
           sep=" ")



export(HG_fuels_mgha_consumption_wider, "HolyGrail/data/clean/fuels/HG_FuelMass_mgha_consumption_coef.csv" )

str(HG_fuels_tonHA_plot)
HG_fuels_tonHA_plot$mass_tonsHA <- as.numeric(HG_fuels_tonHA_plot$mass_tonsHA)

HG_fuels_tonHA_consumption <- HG_fuels_tonHA_plot %>% 
  filter (pre_post_fire == "prefire"|
            pre_post_fire == "postfire" & postTime == "immediate") %>% 
  mutate(time_fire = paste(pre_post_fire, postTime, sep = "_")) %>% 
  mutate(plotID_fuel = paste(site, plotid, fuelType, sep=" ")) %>% 
  select(plotID_fuel, time_fire, mass_tonsHA) 


HG_fuels_tonHA_consumption_wider <- HG_fuels_tonHA_consumption %>% 
  pivot_wider(names_from = "time_fire", values_from = "mass_tonsHA")  %>% 
  unnest(cols= c(prefire_, postfire_immediate)) %>% 
  mutate(consumption_immediate = prefire_ - postfire_immediate) %>% 
  drop_na(consumption_immediate) %>% 
  separate(plotID_fuel, c("site","plotid", "fuelType"), 
           sep=" ")



export(HG_fuels_tonHA_consumption_wider, "HolyGrail/data/clean/fuels/HG_FuelMass_tonHA_consumption_coef.csv" )
