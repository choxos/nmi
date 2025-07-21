# Simulation Study Code for NMI Package Manuscript
# Author: Ahmad Sofi-Mahmudi
# Date: 2025-01-21

library(nmi)
library(dplyr)
library(ggplot2)
library(parallel)

# Set up simulation parameters
set.seed(2025)
n_simulations <- 1000
n_cores <- detectCores() - 1

# Simulation scenarios
scenarios <- expand.grid(
  n_treatments = c(4, 6, 8),
  n_studies_total = c(10, 15, 20),
  n_ipd_studies = c(2, 3, 4),
  effect_modification_type = c("linear", "nonlinear", "threshold", "mixed"),
  missing_data_pattern = c("mcar", "mar", "none"),
  missing_percentage = c(0, 10, 20, 30),
  network_structure = c("connected", "disconnected", "single_arm"),
  stringsAsFactors = FALSE
)

# Filter realistic combinations
scenarios <- scenarios[
  scenarios$n_ipd_studies < scenarios$n_studies_total &
  (scenarios$missing_percentage == 0 | scenarios$missing_data_pattern != "none") &
  (scenarios$missing_percentage > 0 | scenarios$missing_data_pattern == "none"),
]

#' Generate Simulation Dataset
#' 
#' @param scenario List containing simulation parameters
#' @return List with IPD and AgD datasets
generate_simulation_data <- function(scenario) {
  
  # Generate treatments
  treatments <- paste0("Trt_", LETTERS[1:scenario$n_treatments])
  
  # Generate network structure
  if (scenario$network_structure == "connected") {
    network <- generate_connected_network(treatments, scenario$n_studies_total)
  } else if (scenario$network_structure == "disconnected") {
    network <- generate_disconnected_network(treatments, scenario$n_studies_total)
  } else {
    network <- generate_single_arm_network(treatments, scenario$n_studies_total)
  }
  
  # Generate IPD studies
  ipd_studies <- sample(network$studies, scenario$n_ipd_studies)
  
  # Generate effect modification parameters
  if (scenario$effect_modification_type == "linear") {
    em_params <- generate_linear_em_params(treatments)
  } else if (scenario$effect_modification_type == "nonlinear") {
    em_params <- generate_nonlinear_em_params(treatments)
  } else if (scenario$effect_modification_type == "threshold") {
    em_params <- generate_threshold_em_params(treatments)
  } else {
    em_params <- generate_mixed_em_params(treatments)
  }
  
  # Generate IPD
  IPD <- generate_ipd_data(ipd_studies, em_params, scenario)
  
  # Generate AgD
  AgD <- generate_agd_data(network, em_params, scenario)
  
  # Introduce missing data if specified
  if (scenario$missing_percentage > 0) {
    if (scenario$missing_data_pattern == "mcar") {
      IPD <- introduce_mcar_missingness(IPD, scenario$missing_percentage)
      AgD <- introduce_mcar_missingness(AgD, scenario$missing_percentage)
    } else if (scenario$missing_data_pattern == "mar") {
      IPD <- introduce_mar_missingness(IPD, scenario$missing_percentage)
      AgD <- introduce_mar_missingness(AgD, scenario$missing_percentage)
    }
  }
  
  return(list(
    IPD = IPD,
    AgD = AgD,
    network = network,
    em_params = em_params,
    scenario = scenario
  ))
}

#' Generate Connected Network
generate_connected_network <- function(treatments, n_studies) {
  studies <- paste0("Study_", 1:n_studies)
  
  # Ensure connectivity by creating spanning tree
  comparisons <- data.frame(
    study = character(0),
    trt1 = character(0),
    trt2 = character(0)
  )
  
  # Create spanning tree
  for (i in 2:length(treatments)) {
    comparisons <- rbind(comparisons, data.frame(
      study = studies[i-1],
      trt1 = treatments[1],
      trt2 = treatments[i]
    ))
  }
  
  # Add additional random comparisons
  remaining_studies <- studies[length(treatments):n_studies]
  for (study in remaining_studies) {
    trt_pair <- sample(treatments, 2)
    comparisons <- rbind(comparisons, data.frame(
      study = study,
      trt1 = trt_pair[1],
      trt2 = trt_pair[2]
    ))
  }
  
  return(list(studies = studies, comparisons = comparisons))
}

#' Generate Disconnected Network
generate_disconnected_network <- function(treatments, n_studies) {
  # Split treatments into two components
  split_point <- floor(length(treatments) / 2)
  comp1_treatments <- treatments[1:split_point]
  comp2_treatments <- treatments[(split_point + 1):length(treatments)]
  
  studies <- paste0("Study_", 1:n_studies)
  comparisons <- data.frame(
    study = character(0),
    trt1 = character(0),
    trt2 = character(0)
  )
  
  # Component 1 studies
  n_comp1_studies <- floor(n_studies / 2)
  comp1_studies <- studies[1:n_comp1_studies]
  
  for (study in comp1_studies) {
    if (length(comp1_treatments) >= 2) {
      trt_pair <- sample(comp1_treatments, 2)
      comparisons <- rbind(comparisons, data.frame(
        study = study,
        trt1 = trt_pair[1],
        trt2 = trt_pair[2]
      ))
    }
  }
  
  # Component 2 studies
  comp2_studies <- studies[(n_comp1_studies + 1):n_studies]
  
  for (study in comp2_studies) {
    if (length(comp2_treatments) >= 2) {
      trt_pair <- sample(comp2_treatments, 2)
      comparisons <- rbind(comparisons, data.frame(
        study = study,
        trt1 = trt_pair[1],
        trt2 = trt_pair[2]
      ))
    }
  }
  
  return(list(studies = studies, comparisons = comparisons))
}

#' Generate Linear Effect Modification Parameters
generate_linear_em_params <- function(treatments) {
  list(
    baseline_effects = rnorm(length(treatments), 0, 0.3),
    linear_slopes = rnorm(length(treatments), 0, 0.1),
    em_mean = 0,
    em_sd = 1
  )
}

#' Generate IPD Data
generate_ipd_data <- function(ipd_studies, em_params, scenario) {
  IPD <- data.frame()
  
  for (study in names(ipd_studies)) {
    n_patients <- sample(80:200, 1)
    
    # Generate covariates
    age <- rnorm(n_patients, 65, 10)
    sex <- rbinom(n_patients, 1, 0.5)
    
    # Generate treatment assignment
    treatments_in_study <- ipd_studies[[study]]
    treatment <- sample(treatments_in_study, n_patients, replace = TRUE)
    
    # Generate outcomes based on effect modification
    outcome <- generate_outcomes(age, sex, treatment, em_params, scenario)
    
    study_data <- data.frame(
      Study = study,
      age = age,
      sex = sex,
      treatment = treatment,
      outcome = outcome
    )
    
    IPD <- rbind(IPD, study_data)
  }
  
  return(IPD)
}

#' Generate Outcomes Based on Effect Modification
generate_outcomes <- function(age, sex, treatment, em_params, scenario) {
  n_patients <- length(age)
  
  # Standardize age
  age_std <- (age - 65) / 10
  
  outcome <- numeric(n_patients)
  
  for (i in 1:n_patients) {
    trt_idx <- which(names(em_params$baseline_effects) == treatment[i])
    if (length(trt_idx) == 0) trt_idx <- 1
    
    # Linear predictor
    if (scenario$effect_modification_type == "linear") {
      linear_pred <- em_params$baseline_effects[trt_idx] + 
                    em_params$linear_slopes[trt_idx] * age_std[i] +
                    0.2 * sex[i]
    } else if (scenario$effect_modification_type == "nonlinear") {
      linear_pred <- em_params$baseline_effects[trt_idx] + 
                    em_params$linear_slopes[trt_idx] * age_std[i] +
                    0.1 * em_params$linear_slopes[trt_idx] * age_std[i]^2 +
                    0.2 * sex[i]
    } else if (scenario$effect_modification_type == "threshold") {
      threshold_effect <- ifelse(age_std[i] > 0, 0.3, 0)
      linear_pred <- em_params$baseline_effects[trt_idx] + 
                    threshold_effect * em_params$linear_slopes[trt_idx] +
                    0.2 * sex[i]
    } else { # mixed
      linear_pred <- em_params$baseline_effects[trt_idx] + 
                    em_params$linear_slopes[trt_idx] * age_std[i] +
                    0.3 * sex[i] +
                    0.1 * em_params$linear_slopes[trt_idx] * age_std[i] * sex[i]
    }
    
    # Generate binary outcome
    prob <- plogis(linear_pred)
    outcome[i] <- rbinom(1, 1, prob)
  }
  
  return(outcome)
}

#' Generate AgD Data
generate_agd_data <- function(network, em_params, scenario) {
  AgD <- data.frame()
  
  for (i in 1:nrow(network$comparisons)) {
    comparison <- network$comparisons[i, ]
    
    # Study characteristics
    n_total <- sample(50:300, 1)
    age_mean <- rnorm(1, 65, 5)
    sex_prop <- runif(1, 0.3, 0.7)
    
    # Generate treatment effects based on study characteristics
    age_std <- (age_mean - 65) / 10
    
    trt1_idx <- which(names(em_params$baseline_effects) == comparison$trt1)
    trt2_idx <- which(names(em_params$baseline_effects) == comparison$trt2)
    
    if (length(trt1_idx) == 0) trt1_idx <- 1
    if (length(trt2_idx) == 0) trt2_idx <- 1
    
    # Calculate true treatment effect
    if (scenario$effect_modification_type == "linear") {
      eff1 <- em_params$baseline_effects[trt1_idx] + em_params$linear_slopes[trt1_idx] * age_std
      eff2 <- em_params$baseline_effects[trt2_idx] + em_params$linear_slopes[trt2_idx] * age_std
    } else {
      # Simplified for other types
      eff1 <- em_params$baseline_effects[trt1_idx]
      eff2 <- em_params$baseline_effects[trt2_idx]
    }
    
    true_te <- eff1 - eff2
    observed_te <- rnorm(1, true_te, 0.1)
    se <- runif(1, 0.08, 0.15)
    
    study_data <- data.frame(
      Study = comparison$study,
      Trt1 = comparison$trt1,
      Trt2 = comparison$trt2,
      age_mean = age_mean,
      sex_prop = sex_prop,
      n = n_total,
      TE = observed_te,
      se = se,
      true_TE = true_te
    )
    
    AgD <- rbind(AgD, study_data)
  }
  
  return(AgD)
}

#' Introduce MCAR Missingness
introduce_mcar_missingness <- function(data, missing_pct) {
  n_obs <- nrow(data)
  n_missing <- round(n_obs * missing_pct / 100)
  
  # Randomly select observations to make missing
  missing_indices <- sample(1:n_obs, n_missing)
  
  # Make age missing for selected observations
  if ("age" %in% colnames(data)) {
    data$age[missing_indices] <- NA
  }
  if ("age_mean" %in% colnames(data)) {
    data$age_mean[missing_indices] <- NA
  }
  
  return(data)
}

#' Introduce MAR Missingness
introduce_mar_missingness <- function(data, missing_pct) {
  # Make missingness depend on other variables
  if ("sex" %in% colnames(data) && "age" %in% colnames(data)) {
    # Age more likely to be missing in older females
    prob_missing <- 0.1 + 0.3 * data$sex * (data$age > median(data$age, na.rm = TRUE))
    missing_indices <- which(runif(nrow(data)) < prob_missing)
    
    # Adjust to achieve target missing percentage
    target_n_missing <- round(nrow(data) * missing_pct / 100)
    if (length(missing_indices) > target_n_missing) {
      missing_indices <- sample(missing_indices, target_n_missing)
    }
    
    data$age[missing_indices] <- NA
  }
  
  return(data)
}

#' Run Single Simulation
run_single_simulation <- function(scenario_row) {
  tryCatch({
    # Generate data
    sim_data <- generate_simulation_data(scenario_row)
    
    # Run NMI analysis
    target_age <- 65
    x_vect <- c(age = target_age)
    
    if (scenario_row$missing_percentage > 0) {
      # Use ML imputation for missing data
      result <- nmi_with_ml_imputation(
        IPD = sim_data$IPD,
        AgD = sim_data$AgD,
        x_vect = x_vect,
        AgD_EM_cols = "age_mean",
        IPD_EM_cols = "age",
        imputation_method = "random_forest",
        n_imputations = 3
      )
      nmi_result <- result$pooled_result
    } else {
      # Standard NMI analysis
      if (scenario_row$effect_modification_type == "linear") {
        nmi_result <- NMI_interpolation_continuous(
          IPD = sim_data$IPD,
          AgD = sim_data$AgD,
          x_vect = x_vect,
          AgD_EM_cols = "age_mean",
          IPD_EM_cols = "age",
          interpolation_method = "linear"
        )
      } else {
        nmi_result <- NMI_interpolation_continuous(
          IPD = sim_data$IPD,
          AgD = sim_data$AgD,
          x_vect = x_vect,
          AgD_EM_cols = "age_mean",
          IPD_EM_cols = "age",
          interpolation_method = "spline"
        )
      }
    }
    
    # Extract results
    if (!is.null(nmi_result) && !is.null(nmi_result$Final)) {
      estimates <- nmi_result$Final
      
      # Calculate performance metrics vs truth
      if ("true_TE" %in% colnames(sim_data$AgD)) {
        true_effects <- sim_data$AgD[c("Study", "true_TE")]
        merged_results <- merge(estimates, true_effects, by = "Study", all.x = TRUE)
        
        if (nrow(merged_results) > 0) {
          bias <- mean(merged_results$TE - merged_results$true_TE, na.rm = TRUE)
          rmse <- sqrt(mean((merged_results$TE - merged_results$true_TE)^2, na.rm = TRUE))
          coverage <- mean(
            merged_results$true_TE >= merged_results$TE - 1.96 * merged_results$se &
            merged_results$true_TE <= merged_results$TE + 1.96 * merged_results$se,
            na.rm = TRUE
          )
        } else {
          bias <- rmse <- coverage <- NA
        }
      } else {
        bias <- rmse <- coverage <- NA
      }
      
      return(data.frame(
        scenario_id = paste(names(scenario_row), scenario_row, sep = "=", collapse = ";"),
        bias = bias,
        rmse = rmse,
        coverage = coverage,
        n_estimates = nrow(estimates),
        convergence = 1,
        error = NA
      ))
    } else {
      return(data.frame(
        scenario_id = paste(names(scenario_row), scenario_row, sep = "=", collapse = ";"),
        bias = NA,
        rmse = NA,
        coverage = NA,
        n_estimates = 0,
        convergence = 0,
        error = "No results returned"
      ))
    }
    
  }, error = function(e) {
    return(data.frame(
      scenario_id = paste(names(scenario_row), scenario_row, sep = "=", collapse = ";"),
      bias = NA,
      rmse = NA,
      coverage = NA,
      n_estimates = 0,
      convergence = 0,
      error = as.character(e$message)
    ))
  })
}

#' Main Simulation Study
run_simulation_study <- function() {
  cat("Starting simulation study with", nrow(scenarios), "scenarios\n")
  cat("Running", n_simulations, "replications per scenario\n")
  
  # Create list of all simulation runs
  all_runs <- list()
  run_id <- 1
  
  for (i in 1:nrow(scenarios)) {
    for (j in 1:n_simulations) {
      all_runs[[run_id]] <- list(
        scenario = scenarios[i, ],
        replication = j,
        run_id = run_id
      )
      run_id <- run_id + 1
    }
  }
  
  cat("Total simulation runs:", length(all_runs), "\n")
  
  # Run simulations in parallel
  cat("Running simulations in parallel using", n_cores, "cores\n")
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(nmi)
    library(dplyr)
  })
  
  results <- parLapply(cl, all_runs, function(run) {
    result <- run_single_simulation(run$scenario)
    result$replication <- run$replication
    result$run_id <- run$run_id
    return(result)
  })
  
  stopCluster(cl)
  
  # Combine results
  simulation_results <- do.call(rbind, results)
  
  # Save results
  save(simulation_results, scenarios, file = "simulation_results.RData")
  
  return(simulation_results)
}

# Run the simulation study
if (interactive()) {
  cat("To run the simulation study, execute: run_simulation_study()\n")
  cat("Warning: This will take several hours to complete.\n")
} else {
  # Run if called from command line
  results <- run_simulation_study()
  cat("Simulation study completed. Results saved to simulation_results.RData\n")
} 