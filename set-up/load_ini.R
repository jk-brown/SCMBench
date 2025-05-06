## Loading scenario .ini files 
## 
## 
# ini directory 
ini_dir <- paste0(system.file("input", package = "hector"), "/")

# read ini files into a list 
ini_list <- list("SSP1-1.9" = paste0(ini_dir, "hector_ssp119.ini"), 
                 "SSP1-2.6" = paste0(ini_dir, "hector_ssp126.ini"), 
                 "SSP2-4.5" = paste0(ini_dir, "hector_ssp245.ini"), 
                 "SSP3-7.0" = paste0(ini_dir, "hector_ssp370.ini"), 
                 "SSP5-8.5" = paste0(ini_dir, "hector_ssp585.ini"))
