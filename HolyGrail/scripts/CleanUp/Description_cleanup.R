library(tidyverse)
library(tibble)
library(dplyr)
library(rio)


HG_trt <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")

HG_trt %>% 
  count(site, year)


HG_description <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_Description_clean.csv")

HG_description$site <- tolower(HG_description$site)
HG_description$plotid <- tolower(HG_description$plotid)

HG_description %>% 
  count(site, year, pre_post_fire) 
  


HG_regen <- read.csv("HolyGrail/data/raw/CPFMP_regen_combine.csv")

HG_regen %>% 
  count(site, year, pre_post_fire)
