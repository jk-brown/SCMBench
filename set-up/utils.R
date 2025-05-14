## Function to run Hector in parallel while setting a constrained historical CO2.

#' Run Hector + Matilda in concentration-driven mode using parallel processing
#'
#' Runs a perturbed parameter ensemble (PPE) using Hector and Matilda with prescribed 
#' historical CO2 concentrations (i.e. concentration-driven mode). This function runs 
#' each scenario in parallel across SSP cores and applies the same CO2 constraint 
#' to each simulation.
#'
#' @param param_chunks A list of data frames, where each element contains a chunk of parameter sets to be run.
#' @param ini_list A named list of Hector `.ini` file paths for each SSP scenario.
#' @param constraint_df A data frame with columns \code{year} and \code{value} containing the prescribed CO₂ concentrations (ppm).
#' @param save_years A numeric vector of years to save output for (default: \code{1800:2200}).
#' @param save_vars A character vector of variable names to save from each Hector run 
#'   (default: \code{c("global_tas", "gmst", "CO2_concentration", "ocean_uptake", "RF_tot")}).
#' @param n_cores Number of CPU cores to use for parallel execution (default: all but one core).
#'
#' @return A named list of results, where each element corresponds to a scenario and contains the combined output from all parameter chunks.
#'
matilda_conc_driven <- function(param_chunks,
                                ini_list,
                                constraint_df,
                                save_years = 1850:2200,
                                save_vars = c("global_tas",
                                              "gmst",
                                              "CO2_concentration",
                                              "ocean_uptake",
                                              "RF_tot"),
                                n_cores = parallel::detectCores() - 1) {
  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  
  # Helper function to apply prescribed CO2 and run iterate_model
  run_chunk_with_prescribed_co2 <- function(core,
                                            chunk,
                                            constraint_df,
                                            save_years,
                                            save_vars) {
    setvar(
      core,
      dates = constraint_df$year,
      var = CO2_CONSTRAIN(),
      values = constraint_df$CO2_constrain,
      unit = getunits(CO2_CONSTRAIN())
    )
    reset(core)
    
    iterate_model(
      core = core,
      params = chunk,
      save_years = save_years,
      save_vars = save_vars
    )
  }
  
  # Export needed objects
  parallel::clusterExport(
    cl,
    varlist = c(
      "param_chunks",
      "ini_list",
      "constraint_df",
      "run_chunk_with_prescribed_co2",
      "newcore",
      "iterate_model",
      "setvar",
      "CO2_CONSTRAIN",
      "reset",
      "getunits"
    ),
    envir = environment()
  )
  
  # Run in parallel
  result <- parallel::parLapply(cl, names(ini_list), function(scenario_name) {
    scenario_ini <- ini_list[[scenario_name]]
    
    result_list <- lapply(param_chunks, function(chunk) {
      core <- newcore(scenario_ini, name = scenario_name)
      run_chunk_with_prescribed_co2(core, chunk, constraint_df, save_years, save_vars)
    })
    
    return(result_list)
  })
  
  names(result) <- names(ini_list)
  parallel::stopCluster(cl)
  
  return(result)
}


#' Run Matilda in Emissions-driven Mode
#'
#' Runs a perturbed parameter ensemble (PPE) using Hector and Matilda in emissions-driven mode, 
#' where CO2 concentrations are simulated internally based on fossil fuel and land-use emissions.
#' Each scenario is processed in parallel using separate CPU cores, with model runs distributed 
#' across parameter chunks.
#'
#' @param params_chunks A list of data frames, where each element contains a chunk of parameter sets to be run.
#' @param ini_list A named list of Hector `.ini` file paths for each SSP scenario.
#' @param save_years A numeric vector of years to save output for (default: \code{1850:2200}).
#' @param save_vars A character vector of variable names to save from each Hector run 
#'   (default: \code{c("global_tas", "gmst", "CO2_concentration", "ocean_uptake", "RF_tot")}).
#' @param ncores Number of CPU cores to use for parallel execution (default: all but one core).
#'
#' @return A named list of results, where each element corresponds to a scenario and contains the combined output from all parameter chunks.
#' 
matilda_emission_driven <- function(param_chunks,
                                    ini_list,
                                    save_years = 1850:2200,
                                    save_vars = c("global_tas",
                                                  "gmst",
                                                  "CO2_concentrations",
                                                  "ocean_uptake",
                                                  "RF_tot"),
                                    n_cores = parallel::detectCores() - 1) {
  # create cluster
  cl <- parallel::makeCluster(n_cores)
  
  # function to run iterate_model
  run_chunk_emission_driven <- function(core, chunk, save_years, save_vars) {
    reset(core)
    
    iterate_model(
      core = core,
      params = chunk,
      save_years = save_years,
      save_vars = save_vars
    )
  }
  
  # Export needed objects
  parallel::clusterExport(
    cl,
    varlist = c(
      "param_chunks",
      "ini_list",
      "run_chunk_emission_driven",
      "newcore",
      "iterate_model",
      "reset"
    ),
    envir = environment()
  )
  
  # Run in parallel
  result <- parallel::parLapply(cl, names(ini_list), function(scenario_name) {
    scenario_ini <- ini_list[[scenario_name]]
    
    result_list <- lapply(param_chunks, function(chunk) {
      core <- newcore(scenario_ini, name = scenario_name)
      run_chunk_emission_driven(core, chunk, save_years, save_vars)
    })
    
    return(result_list)
  })
  
  names(result) <- names(ini_list)
  parallel::stopCluster(cl)
  
  return(result)
}


## Fixing run numbers
#' Reassign run numbers sequentially across parameter chunks
#'
#' Ensures that run numbers are unique and sequential across all parameter chunks within 
#' each scenario. This is necessary after running Hector + Matilda in parallel, where 
#' each chunk starts with overlapping run_number values.
#'
#' @param result_list A named list of lists. Each top-level element corresponds to a scenario 
#'   and contains a list of data frames (one per parameter chunk), each with a \code{run_number} column.
#'
#' @return A list in the same format as the input, but with corrected, non-overlapping run numbers across chunks.
#'
fix_run_numbers <- function(result_list) {
  fixed_results <- lapply(result_list, function(scenario_chunks) {
    max_run_number <- 0
    
    for (i in seq_along(scenario_chunks)) {
      chunk <- scenario_chunks[[i]]
      
      if (!"run_number" %in% names(chunk)) {
        stop("Chunk is missing a run_number column.")
      }
      
      chunk$run_number <- chunk$run_number + max_run_number
      max_run_number <- max(chunk$run_number, na.rm = TRUE)
      
      scenario_chunks[[i]] <- chunk
    }
    
    # Bind all chunks into one data frame
    do.call(rbind, scenario_chunks)
  })
  
  return(fixed_results)
}