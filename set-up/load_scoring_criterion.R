## Create scoring criterion
## 
## 

## load libraries
source("set-up/libraries.R")
source("data/observations/scipts/temp_data_prep.R")

## Load criterion data
gmst_dat <- read.csv("data/observations/annual_gmst_normalized.csv", stringsAsFactors = F)
co2_dat <- read.csv("data/observations/annual_co2_concentration.csv", stringsAsFactors = F)

## creating GMST criterion
# the gmst data is normalized to 1850-1900 reference
gmst_criterion <- new_criterion("gmst", years = gmst_dat$year, obs_values = gmst_dat$value)

## creating CO2 criterion
co2_criterion <- new_criterion("CO2_concentration", year = co2_dat$year, obs_values = co2_dat$value)


