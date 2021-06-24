library(tidyverse)
library(sf)
library(sp)
library(maptools)


## Read in some perimeters - pre-filtered somewhat (central Sierra fires; >1984)
pers <- read_sf("severity/data/raw/fire20_1.gdb") %>% 
  mutate(Fire_Year = as.integer(YEAR_))%>% 
  dplyr::select(FIRE_NAME, AGENCY, INC_NUM, Fire_Year, Shape)


## Get plot data

plot_locations_CPFMP <-read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")

#convert coordinates

          CPFMP_plots_11 <- plot_locations_CPFMP %>% 
            filter(UTM.Zone == "11S") %>% 
            drop_na(X, Y) %>% 
            st_as_sf(coords = c("X","Y"), 
                     crs ="+proj=utm +zone=11 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
            
          CPFMP_plots_10 <- plot_locations_CPFMP %>% 
            filter(UTM.Zone == "10S"|
                     UTM.Zone == "10T") %>% 
            drop_na(X, Y) %>% 
            st_as_sf(coords = c("X","Y"), 
                     crs ='+proj=utm +zone=10 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' ) %>% 
            st_transform(crs=st_crs(CPFMP_plots_11)) ## match coordinate system with utm 11

  ## combine plot files
    plot_locations_combine <- rbind(CPFMP_plots_11, CPFMP_plots_10)
    
    st
    
    st_crs(plot_locations_combine)
    st_crs(pers)

 
## Limit extent to where intersects plots
keep <- st_intersects(pers, plot_locations_combine) %>% 
  apply(1, any)
pers1 <- pers[keep,]

## take a look
# mapview(huc, col.regions = "darkgreen") + mapview(pers1)
ggplot() +
  geom_sf(data = plot_locations_combine, fill = "darkgreen", color = NA, alpha = 0.3) +
  geom_sf(data = pers, fill = "grey30", color = NA, alpha = 0.4) +
  theme_void()
