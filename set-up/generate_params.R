## Generate and save parameters
## 
## 
# load libraries
source("set-up/libraries.R")

# load .ini files
source("set-up/load_ini.R")

# use SSP2-4.5 as a reference to get default parameter structure
parameter_core <- newcore(ini_list[["SSP2-4.5"]])

# number of parameter sets to sample
n <- 1000
set.seed(444)

# generate PPE
params <- generate_params(core = parameter_core, draws = n)

# save 
# define directory path
params_dir <- here("data", "parameters")

# create directory if one does not exist
if (!dir.exists(params_dir)) {
  dir.create(params_dir, recursive = T)
}

# identify the file path
file_name <- "matilda_parameters.csv"
file_path <- file.path(params_dir, file_name)

# save parameters
write.csv(params, file_path, row.names = FALSE)
