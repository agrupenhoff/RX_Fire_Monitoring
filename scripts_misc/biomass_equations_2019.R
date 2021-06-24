# 1. Get equations.

library(RSQLite)

# connect to database
db = dbConnect(SQLite(), dbname = "scripts_misc/BiomassEqns.db")

# send query and extract as dataframe
eqforms <- dbGetQuery(conn = db, statement = "SELECT * FROM BM_EqForms")

getequations <- function(eq) {
  # convert tree biomass equations into functions
  
  functionbody <- gsub(".*= ", "", eq)
  
  parse(text = functionbody)
  
  f0 <- function(a, b, c, d, e, dia, tht) NULL
  
  body(f0) <- parse(text = functionbody)
  
  return(f0)
}

allfuncts <- lapply(eqforms[ , "equation"], getequations)

ln = log

# 2. Filter equations.

library(dplyr)
library(rlang)

# send query and extract as dataframe
eqcoefs <- dbGetQuery(conn = db, statement = "SELECT * FROM BM_EqCoefs")

# read csv with links between species and equation codes
allom <- read.csv('../../misc/allometry.csv')

# filter for only equations that match species
eqcoefs_filter <- eqcoefs[which(eqcoefs$eqn_no %in% allom$eq_no), ]

# create dataframe of equation codes and species
name_code <- data.frame(eqn_no = allom$eq_no, Species = allom$Species)

# join species names to equation coefficent data
eqcoefs_sub <- left_join(name_code, eqcoefs_filter)

# 3. Biomass calculation functions for source.

calc_tree_biomass <- function(species, dia, tht) {
  # calculate the biomass for a single tree entry
  
  # pull coefficients for given species
  eqn_coefs <- eqcoefs_sub[which(eqcoefs_sub$Species == species), ]
  
  # convert to appropriate diameter unit
  if (eqn_coefs$dia_unit == 'M') {
    dia <- dia/100
  } else {
    dia <- dia
  }
  
  # use correct equation to calculate biomass
  biomass <- allfuncts[[eqn_coefs$eq_form_id]](eqn_coefs$a, eqn_coefs$b, eqn_coefs$c, eqn_coefs$d, eqn_coefs$e, dia, tht)
  
  # convert to appropriate biomass form
  if (eqn_coefs$eq_form_id %in% c(2, 3)) {
    biomass <- exp(biomass)
  } else {
    biomass <- biomass
  }
  
  # convert to appropriate biomass unit
  if (eqn_coefs$biomass_unit == 'G') {
    biomass <- biomass/1000
  } else {
    biomass <- biomass
  }
  
  return(biomass)
}

calc_cwd_biomass <- function(sum_dia, type, slope_factor) {
  # calculate the biomass for a single cwd entry
  
  # define specific gravity depending on decay type
  specific_gravity <- if_else(type == "Sound", 0.4, 0.3)
  
  # general cwd formula for biomass calculation
  biomass_tonsAcre <- (11.64 * sum_dia * 0.155 * specific_gravity * 1.0 * slope_factor) / (11.35 * 4 * 3.28)
  
  biomass_MgAcre <- (biomass_tonsAcre * 2.2417) / 2.47105
  
  return(biomass_MgAcre)
  
}

# 4. Slope area calculation.

calc_slope_area <- function(horz_area, slope_percent) {
  # calculate the slope area from horizontal area and percent slope
  
  # calculate slope in degrees from percent slope
  slope_degree <- atan(slope_percent/100)
  
  # calculate slope area using horizontal area and slope degrees
  slope_area <- horz_area * cos(slope_degree)
  
  return(slope_area)
}

# 5. Potential Annual Heat Load

make_rad <- function(perc) {
  # convert from percent to radians
  
  rad <- perc * pi / 180
  
  return(rad)
  
}

fold_aspect <- function(asp) {
  # folded aspect
  
  rad <- make_rad(asp)
  folded_aspect <- pi - abs(rad - pi)
  
  return(folded_aspect)
  
}

pahl <- function(lat, slope, folded_asp) {
  # potential annual heat load
  
  lat <- make_rad(lat)
  slope <- make_rad(slope)
  pahl <- exp(-1.467 + 1.582 * cos(lat) * cos(slope) - 1.5 * cos(folded_asp) * sin(slope) * sin(lat) - 0.262 * sin(lat) * sin(slope) + 0.607 * sin(folded_asp) * sin(slope))
  
  return(pahl)
  
}
