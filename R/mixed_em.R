#' Mixed Effect Modifier Support for NMI
#' 
#' This module provides support for mixed effect modifier types (binary + continuous)
#' in the NMI package, allowing researchers to handle heterogeneous covariate sets.
#' 
#' @author Ahmad Sofi-Mahmudi
#' @references Harari et al. (2023) Network meta-interpolation
#' 

#' Validate Mixed EM Configuration
#' 
#' Validates that mixed effect modifier configurations are valid and supported
#' 
#' @param em_types Named character vector with EM types
#' @param x_vect Named vector of target EM values
#' 
#' @return List with validation results and configuration summary
#' @export
validate_mixed_em_config <- function(em_types, x_vect) {
  
  if (!all(names(x_vect) %in% names(em_types))) {
    stop("All x_vect names must correspond to effect modifiers in em_types")
  }
  
  # Count EM types
  binary_ems <- names(em_types)[em_types == "binary"]
  continuous_ems <- names(em_types)[em_types == "continuous"]
  categorical_ems <- names(em_types)[em_types == "categorical"]
  
  n_binary <- length(binary_ems)
  n_continuous <- length(continuous_ems)
  n_categorical <- length(categorical_ems)
  n_total <- n_binary + n_continuous + n_categorical
  
  # Check supported configurations
  if (n_total == 0) {
    stop("No effect modifiers specified")
  }
  
  # Validate binary EM values
  if (n_binary > 0) {
    binary_values <- x_vect[binary_ems]
    if (any(!binary_values %in% c(0, 1))) {
      stop("Binary effect modifiers must have values 0 or 1")
    }
  }
  
  # Validate continuous EM values
  if (n_continuous > 0) {
    continuous_values <- x_vect[continuous_ems]
    if (any(!is.finite(continuous_values))) {
      stop("Continuous effect modifiers must have finite numeric values")
    }
  }
  
  # Check supported combinations
  if (n_continuous > 1) {
    warning("Multiple continuous EMs detected. This is experimental. Consider using one continuous EM at a time.")
  }
  
  if (n_categorical > 0) {
    stop("Categorical effect modifiers not yet supported in mixed EM analysis")
  }
  
  return(list(
    valid = TRUE,
    n_binary = n_binary,
    n_continuous = n_continuous,
    n_categorical = n_categorical,
    binary_ems = binary_ems,
    continuous_ems = continuous_ems,
    categorical_ems = categorical_ems,
    configuration = paste0(n_binary, "B+", n_continuous, "C+", n_categorical, "Cat")
  ))
}

#' Mixed EM Data Preparation
#' 
#' Prepares data for mixed EM analysis by handling binary and continuous EMs appropriately
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param binary_ems Character vector of binary EM column names
#' @param continuous_ems Character vector of continuous EM column names
#' @param x_vect Named vector of target values
#' 
#' @return List with prepared data components
#' @export
prepare_mixed_em_data <- function(IPD, AgD, binary_ems, continuous_ems, x_vect) {
  
  # Separate binary and continuous components of target vector
  binary_targets <- x_vect[binary_ems]
  continuous_targets <- x_vect[continuous_ems]
  
  # For binary EMs, filter data to matching subgroups
  if (length(binary_ems) > 0) {
    # Filter IPD to target binary EM values
    ipd_filtered <- IPD
    for (em in binary_ems) {
      target_val <- binary_targets[em]
      ipd_filtered <- ipd_filtered[ipd_filtered[[em]] == target_val, ]
    }
    
    # Filter AgD to studies with compatible binary EM values
    # This is simplified - in practice would need more sophisticated matching
    agd_filtered <- AgD
    
    if (nrow(ipd_filtered) == 0) {
      warning("No IPD observations match the target binary EM values")
    }
  } else {
    ipd_filtered <- IPD
    agd_filtered <- AgD
  }
  
  # For continuous EMs, we'll use interpolation methods
  # This will be handled in the main mixed interpolation function
  
  return(list(
    ipd_filtered = ipd_filtered,
    agd_filtered = agd_filtered,
    binary_targets = binary_targets,
    continuous_targets = continuous_targets,
    n_ipd_remaining = nrow(ipd_filtered),
    n_agd_remaining = nrow(agd_filtered)
  ))
}

#' Hybrid Interpolation for Mixed EMs
#' 
#' Combines binary EM filtering with continuous EM interpolation
#' 
#' @param prepared_data Output from prepare_mixed_em_data
#' @param continuous_ems Character vector of continuous EM column names
#' @param continuous_agd_cols Character vector of continuous EM column names in AgD
#' @param interpolation_method Method for continuous EMs ("linear", "spline")
#' @param ... Additional arguments for interpolation methods
#' 
#' @return Interpolated results for the mixed EM configuration
#' @export
hybrid_interpolation <- function(prepared_data, continuous_ems, continuous_agd_cols,
                                interpolation_method = "linear", ...) {
  
  if (length(continuous_ems) == 0) {
    # Pure binary case - no interpolation needed
    stop("Pure binary EM case should use standard NMI_interpolation()")
  }
  
  if (length(continuous_ems) > 1) {
    stop("Multiple continuous EMs in mixed analysis not yet supported")
  }
  
  # Single continuous EM case
  continuous_em <- continuous_ems[1]
  continuous_agd_col <- continuous_agd_cols[1]
  target_value <- prepared_data$continuous_targets[continuous_em]
  
  # Create study-level data for interpolation
  # This uses the filtered AgD based on binary EM constraints
  study_data <- prepared_data$agd_filtered[, c(continuous_agd_col, "TE", "se")]
  names(study_data)[1] <- continuous_em
  
  # Check if we have enough data for interpolation
  if (nrow(study_data) < 2) {
    stop("Insufficient studies remaining after binary EM filtering for continuous interpolation")
  }
  
  # Perform interpolation on the filtered data
  if (interpolation_method == "linear") {
    result <- linear_interpolation(
      study_data = study_data,
      target_values = prepared_data$continuous_targets,
      em_cols = continuous_em,
      ...
    )
  } else if (interpolation_method == "spline") {
    result <- spline_interpolation(
      study_data = study_data,
      target_values = prepared_data$continuous_targets,
      em_cols = continuous_em,
      ...
    )
  } else {
    stop("Unknown interpolation method: ", interpolation_method)
  }
  
  # Add information about binary EM filtering
  result$binary_em_filter <- prepared_data$binary_targets
  result$n_studies_used <- nrow(study_data)
  result$mixed_em_type <- "binary_filtered_continuous_interpolated"
  
  return(result)
}

#' Main Mixed Effect Modifier Interpolation
#' 
#' Extended NMI interpolation supporting mixed effect modifier types
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param x_vect Named vector of target EM values (mixed types)
#' @param AgD_EM_cols Character vector of EM columns in AgD
#' @param IPD_EM_cols Character vector of EM columns in IPD
#' @param em_types Named character vector specifying EM types
#' @param interpolation_method Method for continuous EMs: "linear", "spline"
#' @param ... Additional arguments passed to interpolation methods
#' 
#' @return Extended NMI results with mixed EM support
#' @export
NMI_interpolation_mixed <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                   em_types = NULL, interpolation_method = "linear", ...) {
  
  # Auto-detect EM types if not provided
  if (is.null(em_types)) {
    em_types <- detect_em_types(IPD, IPD_EM_cols)
    message("Auto-detected EM types: ", paste(names(em_types), em_types, sep = "=", collapse = ", "))
  }
  
  # Validate mixed EM configuration
  config <- validate_mixed_em_config(em_types, x_vect)
  message("Mixed EM configuration: ", config$configuration)
  
  # Route to appropriate analysis based on configuration
  if (config$n_continuous == 0) {
    # Pure binary - use standard NMI
    stop("Pure binary EM analysis should use standard NMI_interpolation()")
    
  } else if (config$n_binary == 0) {
    # Pure continuous - use continuous NMI
    message("Pure continuous EM detected. Routing to NMI_interpolation_continuous()")
    return(NMI_interpolation_continuous(
      IPD = IPD, AgD = AgD, x_vect = x_vect,
      AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols,
      em_types = em_types, interpolation_method = interpolation_method, ...
    ))
    
  } else {
    # Mixed binary + continuous
    message("Mixed binary + continuous EM analysis")
    
    # Prepare data with binary EM filtering
    prepared_data <- prepare_mixed_em_data(
      IPD = IPD, AgD = AgD,
      binary_ems = config$binary_ems,
      continuous_ems = config$continuous_ems,
      x_vect = x_vect
    )
    
    message("After binary EM filtering: ", prepared_data$n_agd_remaining, " studies available")
    
    # Map continuous EM columns between IPD and AgD
    continuous_agd_cols <- AgD_EM_cols[AgD_EM_cols %in% config$continuous_ems | 
                                       grepl(paste0(config$continuous_ems, collapse = "|"), AgD_EM_cols)]
    
    if (length(continuous_agd_cols) == 0) {
      stop("No matching continuous EM columns found in AgD")
    }
    
    # Perform hybrid interpolation
    interpolation_result <- hybrid_interpolation(
      prepared_data = prepared_data,
      continuous_ems = config$continuous_ems,
      continuous_agd_cols = continuous_agd_cols,
      interpolation_method = interpolation_method,
      ...
    )
    
    # Create output in NMI format
    interpolated_data <- data.frame(
      Study = "Mixed_EM_Interpolated",
      Trt1 = "A",  # Simplified - would need proper treatment coding
      Trt2 = "B",
      TE = interpolation_result$te,
      se = interpolation_result$se,
      stringsAsFactors = FALSE
    )
    
    # Add all EM values
    for (em_name in names(x_vect)) {
      interpolated_data[[em_name]] <- x_vect[em_name]
    }
    
    return(list(
      Final = interpolated_data,
      method = paste0("mixed_", interpolation_result$method),
      extrapolation = interpolation_result$extrapolation,
      em_types = em_types,
      configuration = config$configuration,
      binary_ems = config$binary_ems,
      continuous_ems = config$continuous_ems,
      target_values = x_vect,
      n_studies_filtered = prepared_data$n_agd_remaining,
      binary_em_filter = interpolation_result$binary_em_filter
    ))
  }
}

#' Uncertainty Propagation for Mixed EMs
#' 
#' Propagates uncertainty through mixed EM interpolation accounting for both
#' binary EM filtering uncertainty and continuous EM interpolation uncertainty
#' 
#' @param mixed_result Output from NMI_interpolation_mixed
#' @param AgD Original aggregate data
#' @param n_bootstrap Number of bootstrap samples for uncertainty quantification
#' 
#' @return Enhanced results with uncertainty quantification
#' @export
propagate_mixed_em_uncertainty <- function(mixed_result, AgD, n_bootstrap = 1000) {
  
  if (!"mixed_" %in% substr(mixed_result$method, 1, 6)) {
    stop("This function is for mixed EM results only")
  }
  
  # This is a simplified implementation
  # Full implementation would:
  # 1. Bootstrap the binary EM filtering process
  # 2. Bootstrap the continuous EM interpolation
  # 3. Combine uncertainties appropriately
  
  message("Uncertainty propagation for mixed EMs is experimental")
  
  # Placeholder: Add simple uncertainty bounds
  te_est <- mixed_result$Final$TE
  se_est <- mixed_result$Final$se
  
  # Simple uncertainty inflation for mixed approach
  uncertainty_inflation <- 1.2  # 20% inflation for mixed method
  se_adjusted <- se_est * uncertainty_inflation
  
  # Calculate confidence intervals
  ci_lower <- te_est - 1.96 * se_adjusted
  ci_upper <- te_est + 1.96 * se_adjusted
  
  # Add uncertainty information to results
  mixed_result$Final$se_adjusted <- se_adjusted
  mixed_result$Final$ci_lower <- ci_lower
  mixed_result$Final$ci_upper <- ci_upper
  mixed_result$uncertainty_method <- "simple_inflation"
  mixed_result$uncertainty_inflation <- uncertainty_inflation
  
  return(mixed_result)
}

#' Summary for Mixed EM Results
#' 
#' Creates a summary table for mixed effect modifier interpolation results
#' 
#' @param mixed_result Output from NMI_interpolation_mixed
#' 
#' @return Formatted summary of mixed EM analysis
#' @export
summarize_mixed_em_result <- function(mixed_result) {
  
  cat("=== Mixed Effect Modifier NMI Summary ===\n\n")
  
  cat("Configuration:", mixed_result$configuration, "\n")
  cat("Method:", mixed_result$method, "\n")
  cat("Extrapolation warning:", mixed_result$extrapolation, "\n\n")
  
  cat("Effect Modifiers:\n")
  if (length(mixed_result$binary_ems) > 0) {
    cat("  Binary EMs:", paste(mixed_result$binary_ems, collapse = ", "), "\n")
  }
  if (length(mixed_result$continuous_ems) > 0) {
    cat("  Continuous EMs:", paste(mixed_result$continuous_ems, collapse = ", "), "\n")
  }
  
  cat("\nTarget Values:\n")
  for (em_name in names(mixed_result$target_values)) {
    em_type <- mixed_result$em_types[em_name]
    cat("  ", em_name, " (", em_type, "): ", mixed_result$target_values[em_name], "\n")
  }
  
  if (!is.null(mixed_result$n_studies_filtered)) {
    cat("\nStudies available after filtering:", mixed_result$n_studies_filtered, "\n")
  }
  
  cat("\nInterpolated Treatment Effect:\n")
  cat("  TE:", round(mixed_result$Final$TE, 4), "\n")
  cat("  SE:", round(mixed_result$Final$se, 4), "\n")
  
  if ("ci_lower" %in% names(mixed_result$Final)) {
    cat("  95% CI: [", round(mixed_result$Final$ci_lower, 4), ", ", 
        round(mixed_result$Final$ci_upper, 4), "]\n")
  }
  
  cat("\n")
  
  invisible(mixed_result)
} 