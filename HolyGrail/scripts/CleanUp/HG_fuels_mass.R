library(tidyverse)
library(tibble)
library(dplyr)
library(rio)


HG_fuels_data <- read.csv("HolyGrail/data/clean/HG_FWD_CWD_final.csv",
                          stringsAsFactors = TRUE)
HG_trees_data <- read.csv("HolyGrail/data/clean/HolyGrail_Trees_final.csv")

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
        mass_cwd_rotten=(11.64 * sum_d2_1000r_cm2 * 0.155 * 0.30 * slopecorrection) / (11.3*4*3.28)) 

         

         
         
         