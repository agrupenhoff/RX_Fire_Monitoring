library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(rio)
library(lme4)
library(wesanderson)
library(stringr)


HG_description <- read.csv("HolyGrail/data/raw/HolyGrail_Description.csv")

VR_description <- HG_description %>% 
  filter(site =="Valentine") %>% 
  rename(plotid = plot_id)

VR_plottype <- import("SiteLocation/Valentine/data/clean/VR_plottype.csv")

Val_description <- left_join(VR_description, VR_plottype, by = 'plotid')

glimpse(Val_description)


#####General Overview
Val_summarize <- Val_description %>% 
  group_by(PlotType, pre_post_fire) %>% 
  summarise(TotVeg = mean(TOT_VEG_percent),
            TOT_all_percent= mean(TOT_all_percent),
            DT_all_percent = mean(DT_all_percent),
            TOV_percent = mean(TOV_percent),
            TSA_percent = mean(TSA_percent),
            TSE_percent = mean(TSE_percent),
            TOS_percent = mean(TOS_percent),
            ST_percent = mean(ST_percent),
            SM_percent = mean(SM_percent),
            SL_percent = mean(SL_percent),
            TOF_percent = mean(TOF_percent),
            TOG_percent = mean(TOG_percent),
            baresoil = mean(BARESOIL), na.rm=TRUE) %>% 
  mutate(plottype_time = paste(PlotType, pre_post_fire)) %>% 
  select(plottype_time, TotVeg, )
  pivot_longer(-plottype_time, names_to = "CoverType", values_to = "percent")
  
  glimpse(Val_summarize)
