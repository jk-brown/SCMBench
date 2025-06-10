## Chunk parameters and load them
## 
## 
# Load libraries
source("set-up/libraries.R")

# Define the path to the saved parameters
params_file <- here("data", "parameters", "matilda_parameters.csv")

# Read the CSV
parameters <- read.csv(params_file, stringsAsFactors = FALSE, row.names = NULL)
rownames(parameters) <- NULL # just in case there are row names

# Chunk into even parts
chunks <- 50
param_chunks <- split(parameters, cut(seq_len(nrow(parameters)), breaks = chunks, labels = FALSE))

