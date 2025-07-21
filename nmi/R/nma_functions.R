#' Stan model for fixed effects NMA with different outcome types
#'
#' Stan model code for fixed effects network meta-analysis with different
#' outcome types in a treatment-difference format.
#'
#' @param outcome_type Type of outcome: "binary", "continuous", or "count"
#' @return Character string containing Stan model code
#'
#' @export
stan_nma_model <- function(outcome_type = "continuous") {
  if (outcome_type == "continuous") {
    return("
      data {
        int<lower=0> ns;  // number of studies
        int<lower=0> nt;  // number of treatments
        array[ns,2] int t;   // treatment indicators
        vector[ns] y;     // treatment effects
        vector[ns] se;    // standard errors
      }
      
      parameters {
        vector[nt-1] d_raw;  // treatment effects (relative to reference)
      }
      
      transformed parameters {
        vector[nt] d;
        vector[ns] theta;
        
        d[1] = 0;  // reference treatment effect
        d[2:nt] = d_raw;
        
        for(i in 1:ns) {
          theta[i] = d[t[i,2]] - d[t[i,1]];
        }
      }
      
      model {
        // Priors
        d_raw ~ normal(0, 10);
        
        // Likelihood
        y ~ normal(theta, se);
      }
      
      generated quantities {
        matrix[nt-1,nt] D;
        
        // Pairwise treatment effects
        for(c in 1:(nt-1)) {
          for(k in (c+1):nt) {
            D[c,k] = d[k] - d[c];
          }
        }
      }
    ")
  } else if (outcome_type == "binary") {
    return("
      data {
        int<lower=0> ns;  // number of studies
        int<lower=0> nt;  // number of treatments
        array[ns,2] int t;   // treatment indicators
        vector[ns] y;     // log odds ratios
        vector[ns] se;    // standard errors
      }
      
      parameters {
        vector[nt-1] d_raw;  // treatment effects (relative to reference)
      }
      
      transformed parameters {
        vector[nt] d;
        vector[ns] theta;
        
        d[1] = 0;  // reference treatment effect
        d[2:nt] = d_raw;
        
        for(i in 1:ns) {
          theta[i] = d[t[i,2]] - d[t[i,1]];
        }
      }
      
      model {
        // Priors
        d_raw ~ normal(0, 10);
        
        // Likelihood
        y ~ normal(theta, se);
      }
      
      generated quantities {
        matrix[nt-1,nt] D;
        
        // Pairwise treatment effects
        for(c in 1:(nt-1)) {
          for(k in (c+1):nt) {
            D[c,k] = d[k] - d[c];
          }
        }
      }
    ")
  } else if (outcome_type == "count") {
    return("
      data {
        int<lower=0> ns;  // number of studies
        int<lower=0> nt;  // number of treatments
        array[ns,2] int t;   // treatment indicators
        vector[ns] y;     // log rate ratios
        vector[ns] se;    // standard errors
      }
      
      parameters {
        vector[nt-1] d_raw;  // treatment effects (relative to reference)
      }
      
      transformed parameters {
        vector[nt] d;
        vector[ns] theta;
        
        d[1] = 0;  // reference treatment effect
        d[2:nt] = d_raw;
        
        for(i in 1:ns) {
          theta[i] = d[t[i,2]] - d[t[i,1]];
        }
      }
      
      model {
        // Priors
        d_raw ~ normal(0, 10);
        
        // Likelihood
        y ~ normal(theta, se);
      }
      
      generated quantities {
        matrix[nt-1,nt] D;
        
        // Pairwise treatment effects
        for(c in 1:(nt-1)) {
          for(k in (c+1):nt) {
            D[c,k] = d[k] - d[c];
          }
        }
      }
    ")
  } else {
    stop("outcome_type must be 'binary', 'continuous', or 'count'")
  }
}

#' Run Network Meta-Analysis
#'
#' Runs a fixed effects network meta-analysis using Stan via cmdstanr.
#'
#' @param dat Aggregate level data.
#' @param N_chains Number of Markov chains.
#' @param N_iter Number of total iterations per chain (including burn in).
#' @param burnin Length of burn in, i.e. number of iterations to discard at the 
#' beginning.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' @param adapt_delta Adapt delta parameter for Stan sampler.
#' @param max_treedepth Maximum tree depth for Stan sampler.
#' 
#' @return A list with cmdstanr fit object and summary in R2jags format for compatibility.
#'
#' @export
#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr as_tibble mutate recode
#' @importFrom posterior as_draws_df summarise_draws
NMA_run <- function(dat, N_chains = 2, N_iter = 2000, burnin = 1000, 
                    outcome_type = "continuous", adapt_delta = 0.8, max_treedepth = 10) {
  
  # Check CmdStan installation
  if (!require("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr package is required. Please install it first.")
  }
  
  cmdstan_ver <- tryCatch({
    cmdstanr::cmdstan_version(error_on_NA = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(cmdstan_ver) || is.na(cmdstan_ver)) {
    cat("CmdStan not found. Installing...\n")
    cmdstanr::install_cmdstan()
  }
  
  dat <- dat %>% 
    as_tibble() %>% 
    mutate(across(c(Trt1, Trt2), 
                  ~ dplyr::recode(., 'A' = '1', 'B' = '2', 
                                  'C' = '3', 'D' = '4')))
  
  ns <- nrow(dat)
  
  se <- as.numeric(dat$se)
  y <- as.numeric(dat$TE)
  nt <- length(unique(dat$Trt2)) + 1
  t <- apply(dat[, 2:3], 2, as.numeric)
  
  stan_data <- list(ns = ns, nt = nt, t = t, y = y, se = se)
  
  model_code <- stan_nma_model(outcome_type)
  
  # Write model to temporary file
  model_file <- tempfile(fileext = ".stan")
  writeLines(model_code, model_file)
  
  # Compile and run model
  cat("Compiling Stan model...\n")
  model <- cmdstanr::cmdstan_model(model_file)
  
  cat("Sampling from Stan model...\n")
  fit <- model$sample(
    data = stan_data,
    chains = N_chains,
    iter_warmup = burnin,
    iter_sampling = N_iter - burnin,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    show_messages = TRUE,
    refresh = max(1, floor((N_iter - burnin) / 10))
  )
  
  cat("Stan sampling completed successfully!\n")
  
  # Convert to posterior format and create R2jags-style output for compatibility
  draws <- fit$draws()
  summary_draws <- posterior::summarise_draws(draws)
  
  # Create R2jags-style summary matrix
  param_names <- summary_draws$variable
  
  # Check which quantile columns are available
  available_cols <- colnames(summary_draws)
  
  # Build column selection based on what's available
  base_cols <- c("mean", "sd", "rhat", "ess_bulk")
  quantile_cols <- c()
  
  if ("q5" %in% available_cols) quantile_cols <- c(quantile_cols, "q5")
  if ("median" %in% available_cols) {
    quantile_cols <- c(quantile_cols, "median")
  } else if ("q50" %in% available_cols) {
    quantile_cols <- c(quantile_cols, "q50")
  }
  if ("q95" %in% available_cols) quantile_cols <- c(quantile_cols, "q95")
  
  # Use all available columns
  selected_cols <- c(base_cols[base_cols %in% available_cols], 
                     quantile_cols)
  
  summary_matrix <- as.matrix(summary_draws[, selected_cols])
  rownames(summary_matrix) <- param_names
  
  # Create standard column names
  new_colnames <- selected_cols
  if ("q5" %in% selected_cols) {
    new_colnames[new_colnames == "q5"] <- "2.5%"
  }
  if ("median" %in% selected_cols) {
    new_colnames[new_colnames == "median"] <- "50%"
  } else if ("q50" %in% selected_cols) {
    new_colnames[new_colnames == "q50"] <- "50%"
  }
  if ("q95" %in% selected_cols) {
    new_colnames[new_colnames == "q95"] <- "97.5%"
  }
  if ("rhat" %in% selected_cols) {
    new_colnames[new_colnames == "rhat"] <- "Rhat"
  }
  if ("ess_bulk" %in% selected_cols) {
    new_colnames[new_colnames == "ess_bulk"] <- "n.eff"
  }
  
  colnames(summary_matrix) <- new_colnames
  
  # Create R2jags-style output structure
  bugs_output <- list(
    summary = summary_matrix,
    fit = fit,
    draws = draws
  )
  
  # Clean up temporary file
  unlink(model_file)
  
  return(list(
    cmdstan_fit = fit,
    BUGSoutput = bugs_output,
    summary = summary_matrix
  ))
}

#' Extract treatment effects from NMA run
#'
#' Summarizes posterior distributions of treatment effects from an NMA run.
#'
#' @param sim The output of \code{\link{NMA_run}} (cmdstanr format).
#' 
#' @return A data frame of posterior summaries for the treatment effects.
#'
#' @export
#' @importFrom posterior as_draws_df
#' @importFrom stats quantile
NMA_NMI_summary <- function(sim) {
  # Extract draws from cmdstanr output
  if ("cmdstan_fit" %in% names(sim)) {
    draws <- sim$cmdstan_fit$draws()
  } else if ("BUGSoutput" %in% names(sim)) {
    draws <- sim$BUGSoutput$draws
  } else {
    stop("Unsupported simulation output format")
  }
  
  # Convert to data frame
  draws_df <- posterior::as_draws_df(draws)
  
  # Extract D matrix variables
  d_vars <- grep("^D\\[", colnames(draws_df), value = TRUE)
  
  if (length(d_vars) == 0) {
    # If no D matrix, try d_raw variables
    d_vars <- grep("^d_raw", colnames(draws_df), value = TRUE)
    
    if (length(d_vars) > 0) {
      # Create summary for d_raw parameters
      out_list <- list()
      for (var in d_vars) {
        samp <- draws_df[[var]]
        out_list[[var]] <- c(
          mean(samp), 
          quantile(samp, c(0.5, 0.025, 0.975))
        )
      }
      
      out <- as.data.frame(do.call(rbind, out_list))
      colnames(out) <- c("Mean", "50%", "2.5%", "97.5%")
      out$Parameter <- row.names(out)
      row.names(out) <- c()
      
      return(out)
    }
    
    stop("No treatment effect parameters found in model output")
  }
  
  # Calculate summary statistics for D matrix
  out_list <- list()
  for (var in d_vars) {
    samp <- draws_df[[var]]
    out_list[[var]] <- c(
      mean(samp), 
      quantile(samp, c(0.5, 0.025, 0.975))
    )
  }
  
  out <- as.data.frame(do.call(rbind, out_list))
  colnames(out) <- c("Mean", "50%", "2.5%", "97.5%")
  out$Parameter <- row.names(out)
  row.names(out) <- c()
  
  return(out)
}

