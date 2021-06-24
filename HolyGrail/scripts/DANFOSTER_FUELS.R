
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(knitr)
library(kableExtra)
library(rio)
library(plyr)
library(rlang)
library(usethis)

# install the 'devtools' package
#install.packages("devtools")

# load the 'devtools' package
#library(devtools)

# install Rfuels
#devtools::install_github('danfosterfire/Rfuels')
#Then you can load the Rfuels package with:
  
  library(Rfuels)


###FUELS DATA FOR HOLY GRAIL
HG_fuels_data <- read.csv("HolyGrail/data/clean/HG_FWD_CWD_final.csv",
                          stringsAsFactors = TRUE)

fuels_data <- HG_fuels_data %>% 
  mutate(inv_date = case_when(
          year == 2020 ~ "07/01/2020",
          year == 2019 ~ "07/01/2019",
          year == "2013-2018" ~ "07/01/2018",
          year == 2021 ~ "07/01/2021")) %>% 
  mutate(plot_id = paste(site, plotid, year, pre_post_fire, sep="_")) %>%  
  select(plot_id, inv_date, azimuth, x1h_length_m, x10h_length_m, 
         x100h_length_m, x1000h_length_m, count_x1h, count_x10h,
         count_x100h, duff_depth_cm, litter_depth_cm, 
         sum_d2_1000r_cm2, sum_d2_1000s_cm2)  
  
str(fuels_data)
unique(fuels_data$azimuth)
fuels_data$azimuth <- as.integer(fuels_data$azimuth)
fuels_data$count_x1h <- as.integer(fuels_data$count_x1h)
fuels_data$count_x10h <- as.integer(fuels_data$count_x10h)
fuels_data$count_x100h <- as.integer(fuels_data$count_x100h)

##TREE DATA FOR HOLY GRAIL
HG_trees_data <- read.csv("HolyGrail/data/clean/HolyGrail_Trees_final.csv")

trees_data <- HG_trees_data %>% 
  mutate(plot_id = paste(site, plotid, year, pre_post_fire, sep="_")) %>% 
  mutate(inv_date = case_when(
    year == 2020 ~ "07/01/2020",
    year == 2019 ~ "07/01/2019",
    year == "2013-2018" ~ "07/01/2018",
    year == 2021 ~ "07/01/2021")) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  select(plot_id, inv_date, species, dbh_cm) %>% 
  drop_na(dbh_cm, species)

unique(trees_data$species)
head(trees_data)


transect_fuel_loads =  estimate_fuel_loads(fuels_data = fuels_data,
                      trees_data = trees_data,
                      results_type = 'results_only')

  


