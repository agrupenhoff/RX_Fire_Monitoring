# 1. Calculation for biomass

calc_cwd_biomass <- function(sum_dia, type, slope_factor) {
  # calculate the biomass for a single cwd entry
  
  # define specific gravity depending on decay type
  specific_gravity <- if_else(type == "Sound", 0.4, 0.3)
  
  # general cwd formula for biomass calculation
  biomass_tonsAcre <- (11.64 * sum_dia * 0.155 * specific_gravity * 1.0 * slope_factor) / (11.35 * 4 * 3.28)
  
  biomass_MgAcre <- (biomass_tonsAcre * 2.2417) / 2.47105
  
  return(biomass_MgAcre)
  
}

# 2. Slope area calculation.

calc_slope_area <- function(horz_area, slope_percent) {
  # calculate the slope area from horizontal area and percent slope
  
  # calculate slope in degrees from percent slope
  slope_degree <- atan(slope_percent/100)
  
  # calculate slope area using horizontal area and slope degrees
  slope_area <- horz_area * cos(slope_degree)
  
  return(slope_area)
}




####### Calculations:

cwd_data <- read.csv("../../data/saved/cwd_data_all.csv", stringsAsFactors = FALSE)

# calculate squared diameter for cwd biomass calculation
cwd_data$DiameterIntersectSquared_cmsq <- cwd_data$DiameterIntersect_cm^2

# assign decay type to each cwd entry
cwd_data$DecayType <- if_else(cwd_data$DecayClass %in% c(1, 2, 3), "Sound", "Rotten")

# join cwd data with slope data
cwd_data <- left_join(cwd_data, slope_data, by = "PlotID")

# calculate slope factor for cwd biomass calculation
cwd_data$SlopeFactor <- sqrt(1 + (cwd_data$slope/100)^2)

# group cwd entries by plot and calculate sum of sound and rotten cwd squared diameters
cwd_plot_data <- summarize(group_by(cwd_data, PlotID, Date, Observers, FireSev, Northing, Easting, PointAccuracy, SlopeFactor), SoundSumDiameterIntersectSquared_cmsq = sum(DiameterIntersectSquared_cmsq[which(DecayType == "Sound")]), RottenSumDiameterIntersectSquared_cmsq = sum(DiameterIntersectSquared_cmsq[which(DecayType == "Rotten")]))

# assign 1 to NULL slope factor values
cwd_plot_data$SlopeFactor[which(is.na(cwd_plot_data$SlopeFactor))] <- 1

# assign 0 to NULL sound sum diameter squared values
cwd_plot_data$SoundSumDiameterIntersectSquared_cmsq[which(is.na(cwd_plot_data$SoundSumDiameterIntersectSquared_cmsq))] <- 0

# assign 0 to NULL rotten sum diameter squared values
cwd_plot_data$RottenSumDiameterIntersectSquared_cmsq[which(is.na(cwd_plot_data$RottenSumDiameterIntersectSquared_cmsq))] <- 0

# calculate sound cwd plot biomass for each plot
cwd_plot_data$SoundCWDBiomass_MgAcre <- pmap_dbl(list(cwd_plot_data$SoundSumDiameterIntersectSquared_cmsq, "Sound", cwd_plot_data$SlopeFactor), calc_cwd_biomass)

# calculate rotten cwd plot biomass for each plot
cwd_plot_data$RottenCWDBiomass_MgAcre <- pmap_dbl(list(cwd_plot_data$RottenSumDiameterIntersectSquared_cmsq, "Rotten", cwd_plot_data$SlopeFactor), calc_cwd_biomass)
