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

#' Process scenario CO2 constraint data
#'
#' Reads a raw emissions constraint CSV file from Hector, selects and renames the 
#' relevant columns (Date and CO2_constrain), and saves the processed data to a new csv.
#' 
#' @param input_path Character. Path to the raw csv file. 
#' @param output_path Character. Path to save the processed csv file.
#'
#' @returns No return value: writes a cleaned csv file to \code{output_path}
#' 
process_constraint_data_hist <- function(input_path, output_path) {
  
  # Read data
  data <- read.csv(input_path, stringsAsFactors = FALSE, comment.char = ";")
  
  # Select and rename columns
  data <- data %>%
    dplyr::select(Date, CO2_constrain) %>%
    rename(year = Date) %>% 
    filter(year <= 2024)
  
  # Save processed data
  write.csv(data, output_path, row.names = FALSE)
  
}

## Processing data 
# ssp names
ssp_scenarios <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")
# directory where raw data are stored
raw_dir <- here("data", "processed", "raw")
# directory where processed data are saved
processed_dir <- here("data", "processed")

# Process constraint files for all scenarios
lapply(ssp_scenarios, function(ssp) {
  
  input_path <- file.path(raw_dir, paste0(ssp, "_emiss-constraints_rf.csv"))
  output_path <- file.path(processed_dir, paste0(ssp, "_CO2_constraint-historical.csv"))
  
  process_constraint_data_hist(input_path, output_path)
})


