
##OBTAIN COVER VALUES

VR_plottype <- import("SiteLocation/Valentine/data/clean/VR_plottype.csv")
HG_description <- import(("HolyGrail/data/raw/HolyGrail_Description.csv"))

Val_description <- HG_description %>% 
  filter(site == "Valentine") 

Val_cover <- Val_description %>% 
  select(plotid, TOT_VEG_percent, TOT_all_percent)

Val_cover <- left_join(Val_cover, VR_plottype, by="plotid")

Val_cov_sum <- Val_cover %>% 
  group_by(PlotType) %>% 
  summarise_all(mean, na.rm=TRUE)


##BEHAVE OUTPUTS

