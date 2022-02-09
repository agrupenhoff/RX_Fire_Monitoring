library(tidyverse)
library(tibble)
library(dplyr)
library(rio)


HG_fuels_data <- read.csv("HolyGrail/data/clean/fuels/HG_FWD_CWD_final.csv",
                          stringsAsFactors = TRUE)
HG_fuels_data$site <- tolower(HG_fuels_data$site)

##fuels data 


#for fuels equations 
QMD <- read.csv("HolyGrail/data/BrownsTransect_coef/QMD_diam_cm2.csv")


## fuel analysis

          ##Initial fuels data 
          
          #0-3" = (11.64 * n * d^2 * s * a * c)/ (Nl)
          #CWD = (11.64 * n * sum(d^2) * s * a * c)/ (Nl)
          
          #slope correction = (1+(presprings_fuelslope/100)^2)^(1/2)
          #convert sq cm to sq in: * 0.155


#Calculate Mass in tons/acre

HG_fuels_MassFWD_tonsAcre <- HG_fuels_data %>% 
  mutate(slopecorrection = (1+(slope_percent/100)^2)^(1/2)) %>% 
  mutate(mass_1hr =(11.64 * count_x1h * 0.0151 * 	0.48 * 1.13 * slopecorrection)/ (2*3.28),
         mass_10hr=(11.64 * count_x10h * 0.289 * 	0.48 * 1.13 * slopecorrection) / (2*3.28),
         mass_100hr=(11.64 * count_x100h * 2.76 * 	0.40 * 1.13 * slopecorrection) / (4*3.28), 
         mass_cwd_sound =(11.64 * sum_d2_1000s_cm2 * 0.155 * 0.40 * slopecorrection) / (11.3*4*3.28),
         mass_cwd_rotten=(11.64 * sum_d2_1000r_cm2 * 0.155 * 0.30 * slopecorrection) / (11.3*4*3.28)) %>% 
  select(site, plotid, year, pre_post_fire, postTime, plot_id_time, azimuth, 
         duff_depth_cm, litter_depth_cm, fuel_depth_cm,
         mass_1hr, mass_10hr, mass_100hr, mass_cwd_sound, mass_cwd_rotten) %>% 
  mutate(total_fine = mass_1hr+mass_10hr+mass_100hr,
         total_cwd = mass_cwd_rotten+mass_cwd_sound,
         total_fuel = mass_1hr+mass_10hr+mass_100hr+mass_cwd_sound+mass_cwd_rotten,
         site_plotid_time = paste(site, plotid, pre_post_fire, postTime))


#1 acre= 2.47 Ha
HG_fuels_MassFWD_tonsHA <- HG_fuels_MassFWD_tonsAcre %>% 
  mutate(mass_1hr = mass_1hr/2.47,
         mass_10hr = mass_10hr/2.47,
         mass_100hr = mass_100hr/2.47,
         total_fine = total_fine/2.47,
         total_cwd = total_cwd/2.47,
         total_fuel = total_fuel/2.47)

#Average across each plot (Acre)
HG_fuels_tonsAcre_plot <- HG_fuels_MassFWD_tonsAcre %>% 
  group_by(site_plotid_time) %>% 
  summarize(mass_1hr = mean(mass_1hr),
            mass_10hr = mean(mass_10hr),
            mass_100hr = mean(mass_100hr),
            total_fine = mean(total_fine),
            total_cwd = mean(total_cwd),
            total_fuel = mean(total_fuel),
            litter_depth_cm = mean(litter_depth_cm),
            duff_depth_cm = mean(duff_depth_cm),
            fuel_depth_cm = mean(fuel_depth_cm)) %>% 
  pivot_longer(-site_plotid_time, names_to="fuelType", values_to="mass_tonAcre") %>% 
  separate(site_plotid_time, c("site","plotid", "pre_post_fire", "postTime"), 
           sep=" ") %>% 
  drop_na(mass_tonAcre)

#Average across each plot (Ha)
HG_fuels_tonsHA_plot <- HG_fuels_MassFWD_tonsHA %>% 
  group_by(site_plotid_time) %>% 
  summarize(mass_1hr = mean(mass_1hr),
            mass_10hr = mean(mass_10hr),
            mass_100hr = mean(mass_100hr),
            total_fine = mean(total_fine),
            total_cwd = mean(total_cwd),
            total_fuel = mean(total_fuel),
            litter_depth_cm = mean(litter_depth_cm),
            duff_depth_cm = mean(duff_depth_cm),
            fuel_depth_cm = mean(fuel_depth_cm)) %>% 
  pivot_longer(-site_plotid_time, names_to="fuelType", values_to="mass_tonHA") %>% 
  separate(site_plotid_time, c("site","plotid", "pre_post_fire", "postTime"), 
           sep=" ") %>% 
  drop_na(mass_tonHA)

export(HG_fuels_MassFWD_tonsAcre, "HolyGrail/data/clean/fuels/HG_FuelMass_tonAcre.csv")
export(HG_fuels_tonsAcre_plot, "HolyGrail/data/clean/fuels/HG_FuelMass_tonAcre_plot.csv")
export(HG_fuels_MassFWD_tonsHA, "HolyGrail/data/clean/fuels/HG_FuelMass_tonHectare.csv")
export(HG_fuels_tonsHA_plot, "HolyGrail/data/clean/fuels/HG_FuelMass_tonHA_plot.csv")


###############CONSUMPTION

HG_fuels_tonHA_consumption <- HG_fuels_tonsHA_plot %>% 
  filter (pre_post_fire == "prefire"|
          pre_post_fire == "postfire" & postTime == "immediate") %>% 
  unite("time", pre_post_fire:postTime, na.rm = TRUE, remove = FALSE) %>% 
  mutate(plotID_mass = paste(site, plotid, fuelType)) %>% 
  select(plotID_mass, time, mass_tonHA) %>% 
  pivot_wider( names_from = "time", values_from = "mass_tonHA") 

HG_fuels_tonHA_consumption <- HG_fuels_tonHA_consumption %>% 
    mutate(consumption_immediate = prefire_ - postfire_immediate) %>% 
    drop_na(consumption_immediate) %>% 
    separate(plotID_mass, c("site","plotid", "fuelType"), 
           sep=" ")

export(HG_fuels_tonHA_consumption, "HolyGrail/data/clean/fuels/HG_FuelMass_tonHA_consumption.csv" )



         