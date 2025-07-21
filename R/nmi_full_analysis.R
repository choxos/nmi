#' Complete Network Meta-Interpolation Analysis
#'
#' Performs a complete NMI analysis including BLUP imputation, interpolation,
#' and network meta-analysis using cmdstanr.
#'
#' @param IPD Individual patient data.
#' @param AgD Aggregate data.
#' @param x_vect Target effect modifier values for interpolation.
#' @param AgD_EM_cols Column names for effect modifiers in AgD.
#' @param IPD_EM_cols Column names for effect modifiers in IPD.
#' @param IPD_treatment_col Treatment column name in IPD.
#' @param AgD_treatment_cols Treatment column names in AgD.
#' @param IPD_outcome_col Outcome column name in IPD.
#' @param AgD_TE_col Treatment effect column name in AgD.
#' @param AgD_SE_col Standard error column name in AgD.
#' @param AgD_study_col Study column name in AgD.
#' @param study_sample_sizes Sample sizes for AgD studies.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' @param nma_model_type NMA model type: "fixed" or "random".
#' @param mcmc_settings List of MCMC settings (n_iter, n_warmup, n_chains, etc.).
#' 
#' @return A list containing interpolation results and NMA results.
#'
#' @export
#' @importFrom dplyr rename mutate
nmi_full_analysis <- function(IPD, AgD, x_vect,
                             AgD_EM_cols, IPD_EM_cols,
                             IPD_treatment_col, AgD_treatment_cols,
                             IPD_outcome_col, AgD_TE_col, AgD_SE_col, AgD_study_col,
                             study_sample_sizes, outcome_type = "binary",
                             nma_model_type = "fixed",
                             mcmc_settings = list(n_iter = 2000, n_warmup = 1000, 
                                                 n_chains = 2, adapt_delta = 0.8, max_treedepth = 10)) {
  
  cat("Starting NMI Analysis...\n")
  
  # Step 1: Perform NMI interpolation
  cat("Step 1: Performing NMI interpolation...\n")
  
  nmi_results <- NMI_interpolation(
    IPD = IPD,
    AgD = AgD,
    x_vect = x_vect,
    AgD_EM_cols = AgD_EM_cols,
    IPD_EM_cols = IPD_EM_cols,
    Study_col = AgD_study_col,
    samp_sizes = study_sample_sizes,
    AgD_Trt_cols = AgD_treatment_cols,
    TE_col = AgD_TE_col,
    SE_col = AgD_SE_col,
    IPD_Trt_col = IPD_treatment_col,
    outcome_col = IPD_outcome_col,
    outcome_type = outcome_type
  )
  
  # Step 2: Run Network Meta-Analysis on interpolated data
  cat("Step 2: Running Network Meta-Analysis...\n")
  
  # Prepare NMA data from interpolated results
  nma_data <- nmi_results$Final
  
  # Ensure column names match expected format for NMA_run
  if (!"TE" %in% colnames(nma_data)) {
    if ("te" %in% colnames(nma_data)) {
      nma_data <- nma_data %>% rename(TE = te)
    }
  }
  if (!"se" %in% colnames(nma_data)) {
    if ("SE" %in% colnames(nma_data)) {
      nma_data <- nma_data %>% rename(se = SE)
    }
  }
  
  # Run NMA using cmdstanr
  nma_results <- NMA_run(
    dat = nma_data,
    N_chains = mcmc_settings$n_chains,
    N_iter = mcmc_settings$n_iter,
    burnin = mcmc_settings$n_warmup,
    outcome_type = outcome_type,
    adapt_delta = mcmc_settings$adapt_delta,
    max_treedepth = mcmc_settings$max_treedepth
  )
  
  cat("NMI Analysis completed successfully!\n")
  
  # Return comprehensive results
  result <- list(
    nmi_interpolation = nmi_results,
    nma_results = nma_results,
    settings = list(
      x_target = x_vect,
      outcome_type = outcome_type,
      nma_model_type = nma_model_type,
      mcmc_settings = mcmc_settings
    ),
    treatments = sort(unique(c(as.character(nma_data$Trt1), as.character(nma_data$Trt2)))),
    data_info = list(
      n_studies = length(unique(nma_data$Study)),
      n_comparisons = nrow(nma_data),
      effect_modifiers = AgD_EM_cols
    )
  )
  
  # Add class for method dispatch
  class(result) <- c("nmi_analysis", class(result))
  
  return(result)
}

#' Run Standard Network Meta-Analysis (for comparison)
#'
#' Runs a standard NMA without interpolation for comparison with NMI results.
#'
#' @param AgD Aggregate data in standard format.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' @param mcmc_settings List of MCMC settings.
#' @param study_col Study column name.
#' @param trt_cols Treatment column names.
#' @param te_col Treatment effect column name.
#' @param se_col Standard error column name.
#' 
#' @return NMA results from cmdstanr.
#'
#' @export
nmi_standard_nma <- function(AgD, outcome_type = "binary",
                            mcmc_settings = list(n_iter = 2000, n_warmup = 1000, 
                                                n_chains = 2, adapt_delta = 0.8, max_treedepth = 10),
                            study_col = "Study", trt_cols = c("Trt1", "Trt2"),
                            te_col = "TE", se_col = "SE") {
  
  cat("Running standard NMA for comparison...\n")
  
  # Prepare data for NMA
  nma_data <- AgD[, c(study_col, trt_cols, te_col, se_col)]
  colnames(nma_data) <- c("Study", "Trt1", "Trt2", "TE", "se")
  
  # Get overall treatment effects (first occurrence of each comparison)
  nma_data <- nma_data[!duplicated(paste(nma_data$Trt1, nma_data$Trt2)), ]
  
  # Run NMA
  nma_results <- NMA_run(
    dat = nma_data,
    N_chains = mcmc_settings$n_chains,
    N_iter = mcmc_settings$n_iter,
    burnin = mcmc_settings$n_warmup,
    outcome_type = outcome_type,
    adapt_delta = mcmc_settings$adapt_delta,
    max_treedepth = mcmc_settings$max_treedepth
  )
  
  cat("Standard NMA completed.\n")
  
  return(nma_results)
} 