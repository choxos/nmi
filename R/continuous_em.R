#' Continuous Effect Modifier Support for NMI
#' 
#' This module provides support for continuous effect modifiers in the NMI package,
#' extending the original binary-only implementation to handle real-valued covariates.
#' 
#' @author Ahmad Sofi-Mahmudi
#' @references Harari et al. (2023) Network meta-interpolation
#' 

#' Detect Effect Modifier Types
#' 
#' Automatically detects whether effect modifiers are binary, categorical, or continuous
#' 
#' @param data A data frame containing effect modifier columns
#' @param em_cols Character vector of effect modifier column names
#' @param threshold Numeric threshold for determining if a variable is effectively binary
#' 
#' @return A named character vector with EM types ("binary", "categorical", "continuous")
#' @export
#' 
#' @examples
#' data <- data.frame(age = c(45, 50, 65), sex = c(0, 1, 1), bmi = c(22.5, 28.3, 30.1))
#' detect_em_types(data, c("age", "sex", "bmi"))
detect_em_types <- function(data, em_cols, threshold = 0.95) {
  
  if (!all(em_cols %in% names(data))) {
    stop("Some effect modifier columns not found in data")
  }
  
  types <- character(length(em_cols))
  names(types) <- em_cols
  
  for (col in em_cols) {
    values <- data[[col]]
    unique_vals <- unique(values[!is.na(values)])
    n_unique <- length(unique_vals)
    
    # Check if binary (only 0,1 or similar)
    if (n_unique == 2 && all(unique_vals %in% c(0, 1))) {
      types[col] <- "binary"
    } else if (n_unique <= 10 && all(unique_vals == floor(unique_vals))) {
      # Categorical: few integer values
      types[col] <- "categorical"  
    } else {
      # Continuous: many unique values or non-integer
      types[col] <- "continuous"
    }
  }
  
  return(types)
}

#' Linear Interpolation for Continuous Effect Modifiers
#' 
#' Performs linear interpolation to estimate treatment effects at target EM values
#' 
#' @param study_data Data frame with study-level EM values and treatment effects
#' @param target_values Named vector of target EM values for interpolation
#' @param em_cols Character vector of continuous EM column names
#' @param te_col Character name of treatment effect column
#' @param se_col Character name of standard error column
#' 
#' @return List with interpolated treatment effect and standard error
#' @export
#' 
#' @importFrom stats approx
linear_interpolation <- function(study_data, target_values, em_cols, 
                                te_col = "TE", se_col = "se") {
  
  if (length(em_cols) > 1) {
    stop("Linear interpolation currently supports single EM only. Use multivariate methods for multiple EMs.")
  }
  
  em_col <- em_cols[1]
  target_val <- target_values[em_col]
  
  if (is.na(target_val)) {
    stop("Target value for ", em_col, " is missing")
  }
  
  # Sort by EM values
  study_data <- study_data[order(study_data[[em_col]]), ]
  
  # Interpolate treatment effect
  te_interp <- stats::approx(
    x = study_data[[em_col]], 
    y = study_data[[te_col]], 
    xout = target_val,
    method = "linear"
  )$y
  
  # Interpolate standard error
  se_interp <- stats::approx(
    x = study_data[[em_col]], 
    y = study_data[[se_col]], 
    xout = target_val,
    method = "linear"
  )$y
  
  # Check for extrapolation
  em_range <- range(study_data[[em_col]])
  if (target_val < em_range[1] || target_val > em_range[2]) {
    warning("Target value ", target_val, " is outside observed range [", 
            em_range[1], ", ", em_range[2], "]. Extrapolation may be unreliable.")
  }
  
  return(list(
    te = te_interp,
    se = se_interp,
    method = "linear",
    extrapolation = target_val < em_range[1] || target_val > em_range[2]
  ))
}

#' Spline Interpolation for Continuous Effect Modifiers
#' 
#' Performs spline-based interpolation using natural splines or cubic splines
#' 
#' @param study_data Data frame with study-level EM values and treatment effects
#' @param target_values Named vector of target EM values for interpolation
#' @param em_cols Character vector of continuous EM column names (single EM for now)
#' @param te_col Character name of treatment effect column
#' @param se_col Character name of standard error column
#' @param spline_type Character: "natural", "cubic", or "smoothing"
#' @param df Degrees of freedom for spline (default: 3)
#' 
#' @return List with interpolated treatment effect and standard error
#' @export
#' 
#' @importFrom stats spline predict smooth.spline
spline_interpolation <- function(study_data, target_values, em_cols,
                                te_col = "TE", se_col = "se", 
                                spline_type = "natural", df = 3) {
  
  if (length(em_cols) > 1) {
    stop("Spline interpolation currently supports single EM only")
  }
  
  em_col <- em_cols[1]
  target_val <- target_values[em_col]
  
  if (is.na(target_val)) {
    stop("Target value for ", em_col, " is missing")
  }
  
  # Check minimum data points
  if (nrow(study_data) < 3) {
    warning("Too few data points for spline interpolation. Using linear interpolation.")
    return(linear_interpolation(study_data, target_values, em_cols, te_col, se_col))
  }
  
  # Sort by EM values
  study_data <- study_data[order(study_data[[em_col]]), ]
  
  # Fit spline for treatment effect
  if (spline_type == "smoothing") {
    te_spline <- stats::smooth.spline(study_data[[em_col]], study_data[[te_col]])
    te_interp <- predict(te_spline, target_val)$y
  } else {
    te_spline <- stats::spline(study_data[[em_col]], study_data[[te_col]], 
                              xout = target_val, method = "natural")
    te_interp <- te_spline$y
  }
  
  # Fit spline for standard error  
  if (spline_type == "smoothing") {
    se_spline <- stats::smooth.spline(study_data[[em_col]], study_data[[se_col]])
    se_interp <- predict(se_spline, target_val)$y
  } else {
    se_spline <- stats::spline(study_data[[em_col]], study_data[[se_col]], 
                              xout = target_val, method = "natural")
    se_interp <- se_spline$y
  }
  
  # Check for extrapolation
  em_range <- range(study_data[[em_col]])
  extrapolation <- target_val < em_range[1] || target_val > em_range[2]
  
  if (extrapolation) {
    warning("Target value ", target_val, " is outside observed range [", 
            em_range[1], ", ", em_range[2], "]. Extrapolation may be unreliable.")
  }
  
  return(list(
    te = te_interp,
    se = se_interp,
    method = paste0("spline_", spline_type),
    extrapolation = extrapolation
  ))
}

#' Adaptive Discretization for Continuous Effect Modifiers
#' 
#' Optimally discretizes continuous effect modifiers to minimize information loss
#' 
#' @param continuous_data Numeric vector of continuous EM values
#' @param outcome_data Associated outcome data for optimization
#' @param method Discretization optimization method
#' @param max_bins Maximum number of bins to consider
#' @param min_bin_size Minimum observations per bin
#' 
#' @return List with optimal discretization and performance metrics
#' @export
optimize_discretization <- function(continuous_data, outcome_data = NULL, 
                                  method = "equal_freq", max_bins = 10, 
                                  min_bin_size = 5) {
  
  n <- length(continuous_data)
  
  if (n < min_bin_size * 2) {
    stop("Insufficient data for discretization (need at least ", min_bin_size * 2, " observations)")
  }
  
  # Try different numbers of bins
  bin_candidates <- 2:min(max_bins, floor(n / min_bin_size))
  
  best_bins <- 2
  best_score <- -Inf
  
  for (n_bins in bin_candidates) {
    
    # Create discretization
    if (method == "equal_freq") {
      # Equal frequency binning
      quantiles <- quantile(continuous_data, probs = seq(0, 1, length.out = n_bins + 1))
      discretized <- cut(continuous_data, breaks = quantiles, include.lowest = TRUE)
    } else if (method == "equal_width") {
      # Equal width binning
      discretized <- cut(continuous_data, breaks = n_bins, include.lowest = TRUE)
    } else {
      stop("Unknown discretization method: ", method)
    }
    
    # Check bin sizes
    bin_counts <- table(discretized)
    if (any(bin_counts < min_bin_size)) {
      next  # Skip if any bin too small
    }
    
    # Score discretization (simple variance-based for now)
    # Higher variance between bins is better
    bin_means <- tapply(continuous_data, discretized, mean)
    score <- var(bin_means, na.rm = TRUE)
    
    if (score > best_score) {
      best_score <- score
      best_bins <- n_bins
    }
  }
  
  # Create final discretization
  if (method == "equal_freq") {
    quantiles <- quantile(continuous_data, probs = seq(0, 1, length.out = best_bins + 1))
    final_discretized <- cut(continuous_data, breaks = quantiles, include.lowest = TRUE)
    breaks <- quantiles
  } else {
    final_discretized <- cut(continuous_data, breaks = best_bins, include.lowest = TRUE)
    breaks <- NULL
  }
  
  return(list(
    discretized = final_discretized,
    n_bins = best_bins,
    breaks = breaks,
    method = method,
    score = best_score,
    bin_counts = table(final_discretized)
  ))
}

#' Main Continuous Effect Modifier Interpolation
#' 
#' Extended NMI interpolation supporting continuous effect modifiers
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data 
#' @param x_vect Named vector of target EM values (can include continuous values)
#' @param AgD_EM_cols Character vector of EM columns in AgD
#' @param IPD_EM_cols Character vector of EM columns in IPD  
#' @param em_types Named character vector specifying EM types ("binary", "continuous")
#' @param interpolation_method Method for continuous EMs: "linear", "spline", "discretize"
#' @param ... Additional arguments passed to interpolation methods
#' 
#' @return Extended NMI results with continuous EM support
#' @export
NMI_interpolation_continuous <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                       em_types = NULL, 
                                       interpolation_method = "linear", ...) {
  
  # Auto-detect EM types if not provided
  if (is.null(em_types)) {
    # Check IPD for EM types
    em_types <- detect_em_types(IPD, IPD_EM_cols)
    message("Auto-detected EM types: ", paste(names(em_types), em_types, sep = "=", collapse = ", "))
  }
  
  # Validate inputs
  if (!all(names(x_vect) %in% names(em_types))) {
    stop("All x_vect names must correspond to effect modifiers")
  }
  
  # Separate binary and continuous EMs
  binary_ems <- names(em_types)[em_types == "binary"]
  continuous_ems <- names(em_types)[em_types == "continuous"]
  
  if (length(continuous_ems) == 0) {
    warning("No continuous EMs detected. Consider using standard NMI_interpolation().")
    # Fall back to standard NMI - would need to call original function
    stop("Fallback to standard NMI not yet implemented. Use NMI_interpolation() for binary EMs only.")
  }
  
  # For now, implement simple case: single continuous EM
  if (length(continuous_ems) > 1) {
    stop("Multiple continuous EMs not yet supported. Please use one continuous EM at a time.")
  }
  
  if (length(binary_ems) > 0) {
    stop("Mixed binary + continuous EMs not yet supported. Coming in next update.")
  }
  
  # Continuous EM interpolation
  continuous_em <- continuous_ems[1]
  target_value <- x_vect[continuous_em]
  
  # Create study-level data for interpolation
  # This is a simplified implementation - in full version would integrate with existing NMI pipeline
  study_data <- AgD[, c(AgD_EM_cols[AgD_EM_cols %in% continuous_em], "TE", "se")]
  names(study_data)[1] <- continuous_em
  
  # Perform interpolation
  if (interpolation_method == "linear") {
    result <- linear_interpolation(study_data, x_vect, continuous_em)
  } else if (interpolation_method == "spline") {
    result <- spline_interpolation(study_data, x_vect, continuous_em, ...)
  } else {
    stop("Unknown interpolation method: ", interpolation_method)
  }
  
  # Create output in NMI format
  interpolated_data <- data.frame(
    Study = "Interpolated",
    Trt1 = "A",  # Simplified - would need proper treatment coding
    Trt2 = "B", 
    TE = result$te,
    se = result$se,
    stringsAsFactors = FALSE
  )
  
  # Add EM values
  interpolated_data[[continuous_em]] <- target_value
  
  return(list(
    Final = interpolated_data,
    method = result$method,
    extrapolation = result$extrapolation,
    em_types = em_types,
    continuous_ems = continuous_ems,
    target_values = x_vect
  ))
} 