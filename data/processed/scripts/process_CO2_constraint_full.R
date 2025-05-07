## Preparing CO2_constraint data from larger emissions constraint data fromat from Hector
## 
## Once Hector is installed, emissions tables are available with the package
## 
## Ex: read.csv(file.path(example_directory_path, "inst", "input", "tables", "ssp434_emiss-constraints_rf.csv"), comment.char = ";")
## 
## Here, data were hard copied over.
## 

# source libraries 
source("set-up/libraries.R")

# load data 
CO2_constrain_full <- read.csv("data/processed/raw/ssp245_emiss-constraints_rf.csv", stringsAsFactors = FALSE, comment.char = ";")

# select the Date and CO2_constrain column
# change Date column name to `year`
# filter year < 2024 
CO2_constrain_full <- CO2_constrain_full |>
  dplyr::select(Date, CO2_constrain) |>
  rename(year = Date)

# save 
# directory
processed_data_dir <- here("data", "processed")

# identify the file path
file_name <- "CO2_constrain_full.csv"
file_path <- file.path(processed_data_dir, file_name)

# save parameters
write.csv(CO2_constrain_full, file_path, row.names = FALSE)
