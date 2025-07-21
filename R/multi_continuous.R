#' Multiple Continuous Effect Modifiers Support for NMI
#' 
#' This module provides support for multiple continuous effect modifiers
#' using multivariate interpolation methods.
#' 
#' @author Ahmad Sofi-Mahmudi
#' @references Harari et al. (2023) Network meta-interpolation
#' 

#' Multivariate Linear Interpolation
#' 
#' Performs multivariate linear interpolation for multiple continuous effect modifiers
#' 
#' @param study_data Data frame with study-level EM values and treatment effects
#' @param target_values Named vector of target EM values for interpolation
#' @param em_cols Character vector of continuous EM column names
#' @param te_col Character name of treatment effect column
#' @param se_col Character name of standard error column
#' @param method Multivariate interpolation method ("idw", "rbf", "linear_model")
#' 
#' @return List with interpolated treatment effect and standard error
#' @export
#' 
#' @importFrom stats lm predict
multivariate_interpolation <- function(study_data, target_values, em_cols,
                                     te_col = "TE", se_col = "se", 
                                     method = "linear_model") {
  
  n_ems <- length(em_cols)
  if (n_ems < 2) {
    stop("Use single EM interpolation functions for one effect modifier")
  }
  
  if (n_ems > 3) {
    warning("Interpolation with >3 continuous EMs is experimental and may be unreliable")
  }
  
  # Check target values are provided for all EMs
  if (!all(em_cols %in% names(target_values))) {
    stop("Target values must be provided for all continuous EMs")
  }
  
  # Validate data completeness
  if (any(is.na(study_data[, em_cols]))) {
    stop("Missing values in EM columns not allowed for multivariate interpolation")
  }
  
  # Check minimum data requirements
  min_studies_required <- 2^n_ems  # Exponential in dimensions
  if (nrow(study_data) < min_studies_required) {
    warning("Limited data for ", n_ems, "D interpolation. Need at least ", 
            min_studies_required, " studies, have ", nrow(study_data))
  }
  
  # Prepare EM matrix
  X <- as.matrix(study_data[, em_cols])
  y_te <- study_data[[te_col]]
  y_se <- study_data[[se_col]]
  
  # Target point
  x_target <- as.numeric(target_values[em_cols])
  
  # Check for extrapolation
  em_ranges <- apply(X, 2, range)
  extrapolation <- any(x_target < em_ranges[1, ] | x_target > em_ranges[2, ])
  
  if (extrapolation) {
    warning("Target values outside observed range. Extrapolation may be unreliable.")
  }
  
  # Perform interpolation based on method
  if (method == "linear_model") {
    # Fit linear model
    te_result <- linear_model_interpolation(X, y_te, x_target, em_cols)
    se_result <- linear_model_interpolation(X, y_se, x_target, em_cols)
    
  } else if (method == "idw") {
    # Inverse distance weighting
    te_result <- idw_interpolation(X, y_te, x_target)
    se_result <- idw_interpolation(X, y_se, x_target)
    
  } else if (method == "rbf") {
    # Radial basis function
    te_result <- rbf_interpolation(X, y_te, x_target)
    se_result <- rbf_interpolation(X, y_se, x_target)
    
  } else {
    stop("Unknown multivariate interpolation method: ", method)
  }
  
  return(list(
    te = te_result,
    se = se_result,
    method = paste0("multivariate_", method),
    extrapolation = extrapolation,
    n_dimensions = n_ems,
    n_studies_used = nrow(study_data)
  ))
}

#' Linear Model Interpolation
#' 
#' Uses linear regression model for multivariate interpolation
#' 
#' @param X Matrix of EM values (studies x EMs)
#' @param y Vector of outcome values (TE or SE)
#' @param x_target Target EM values vector
#' @param em_cols EM column names for formula construction
#' 
#' @return Interpolated value
#' @importFrom stats lm predict
linear_model_interpolation <- function(X, y, x_target, em_cols) {
  
  # Create data frame for modeling
  model_data <- data.frame(y = y, X)
  names(model_data) <- c("y", em_cols)
  
  # Create formula
  formula_str <- paste("y ~", paste(em_cols, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Fit linear model
  model <- lm(formula_obj, data = model_data)
  
  # Create target data frame
  target_data <- data.frame(t(x_target))
  names(target_data) <- em_cols
  
  # Predict
  prediction <- predict(model, newdata = target_data)
  
  return(as.numeric(prediction))
}

#' Inverse Distance Weighting Interpolation
#' 
#' Uses inverse distance weighting for multivariate interpolation
#' 
#' @param X Matrix of EM values (studies x EMs)
#' @param y Vector of outcome values
#' @param x_target Target EM values vector
#' @param power Power parameter for IDW (default: 2)
#' 
#' @return Interpolated value
idw_interpolation <- function(X, y, x_target, power = 2) {
  
  # Calculate distances from target to all study points
  distances <- sqrt(rowSums((X - matrix(x_target, nrow = nrow(X), 
                                       ncol = ncol(X), byrow = TRUE))^2))
  
  # Handle exact matches (distance = 0)
  if (any(distances == 0)) {
    exact_match_idx <- which(distances == 0)[1]
    return(y[exact_match_idx])
  }
  
  # Calculate weights (inverse distance)
  weights <- 1 / (distances^power)
  
  # Weighted average
  interpolated_value <- sum(weights * y) / sum(weights)
  
  return(interpolated_value)
}

#' Radial Basis Function Interpolation
#' 
#' Uses RBF for multivariate interpolation (simplified Gaussian RBF)
#' 
#' @param X Matrix of EM values (studies x EMs)
#' @param y Vector of outcome values
#' @param x_target Target EM values vector
#' @param bandwidth Bandwidth parameter for RBF
#' 
#' @return Interpolated value
rbf_interpolation <- function(X, y, x_target, bandwidth = NULL) {
  
  # Auto-select bandwidth if not provided
  if (is.null(bandwidth)) {
    # Use median distance as bandwidth
    distances_all <- as.matrix(dist(X))
    bandwidth <- median(distances_all[distances_all > 0])
  }
  
  # Calculate RBF weights (Gaussian)
  distances <- sqrt(rowSums((X - matrix(x_target, nrow = nrow(X), 
                                       ncol = ncol(X), byrow = TRUE))^2))
  weights <- exp(-(distances^2) / (2 * bandwidth^2))
  
  # Weighted average
  interpolated_value <- sum(weights * y) / sum(weights)
  
  return(interpolated_value)
}

#' Validate Multiple Continuous EM Configuration
#' 
#' Validates that multiple continuous EM setup is appropriate
#' 
#' @param study_data Data frame with study-level data
#' @param em_cols Character vector of EM column names
#' @param target_values Named vector of target values
#' 
#' @return List with validation results and recommendations
#' @export
validate_multi_continuous_config <- function(study_data, em_cols, target_values) {
  
  n_ems <- length(em_cols)
  n_studies <- nrow(study_data)
  
  # Check dimensionality
  if (n_ems == 1) {
    return(list(
      valid = TRUE,
      recommendation = "single_em",
      message = "Single EM detected. Use single EM interpolation functions."
    ))
  }
  
  if (n_ems > 4) {
    return(list(
      valid = FALSE,
      recommendation = "reduce_dimensions",
      message = paste("Too many EMs (", n_ems, "). Consider reducing to 2-3 most important EMs.")
    ))
  }
  
  # Check data sufficiency
  min_studies <- 2^n_ems
  recommended_studies <- min_studies * 2
  
  if (n_studies < min_studies) {
    return(list(
      valid = FALSE,
      recommendation = "insufficient_data",
      message = paste("Insufficient studies for", n_ems, "D interpolation. Need at least", 
                     min_studies, "studies, have", n_studies)
    ))
  }
  
  # Check for collinearity
  X <- as.matrix(study_data[, em_cols])
  correlation_matrix <- cor(X)
  max_correlation <- max(abs(correlation_matrix[upper.tri(correlation_matrix)]))
  
  if (max_correlation > 0.9) {
    return(list(
      valid = TRUE,
      recommendation = "collinearity_warning",
      message = paste("High correlation detected (max =", round(max_correlation, 3), 
                     "). Consider reducing EMs or using regularization.")
    ))
  }
  
  # Check coverage
  X_scaled <- scale(X)
  target_scaled <- scale(matrix(target_values[em_cols], nrow = 1), 
                        center = attr(X_scaled, "scaled:center"),
                        scale = attr(X_scaled, "scaled:scale"))
  
  # Simple convex hull check (for 2D-3D)
  if (n_ems <= 3) {
    # Check if target is within range on each dimension
    in_range <- all(target_values[em_cols] >= apply(X, 2, min) & 
                   target_values[em_cols] <= apply(X, 2, max))
    
    if (!in_range) {
      return(list(
        valid = TRUE,
        recommendation = "extrapolation_warning",
        message = "Target values outside observed range. Extrapolation required."
      ))
    }
  }
  
  # All checks passed
  if (n_studies < recommended_studies) {
    return(list(
      valid = TRUE,
      recommendation = "proceed_with_caution",
      message = paste("Valid configuration but limited data. Have", n_studies, 
                     "studies, recommend", recommended_studies, "for stable results.")
    ))
  }
  
  return(list(
    valid = TRUE,
    recommendation = "proceed",
    message = "Good configuration for multivariate interpolation."
  ))
}

#' Main Multiple Continuous EM Interpolation
#' 
#' Extended NMI interpolation supporting multiple continuous effect modifiers
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data  
#' @param x_vect Named vector of target EM values (all continuous)
#' @param AgD_EM_cols Character vector of EM columns in AgD
#' @param IPD_EM_cols Character vector of EM columns in IPD
#' @param interpolation_method Method: "linear_model", "idw", "rbf"
#' @param auto_validate Whether to run validation checks
#' @param ... Additional arguments passed to interpolation methods
#' 
#' @return Extended NMI results with multiple continuous EM support
#' @export
NMI_interpolation_multi_continuous <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                              interpolation_method = "linear_model",
                                              auto_validate = TRUE, ...) {
  
  # Auto-detect EM types
  em_types <- detect_em_types(IPD, IPD_EM_cols)
  continuous_ems <- names(em_types)[em_types == "continuous"]
  
  if (length(continuous_ems) < 2) {
    stop("Use single continuous EM functions for fewer than 2 continuous EMs")
  }
  
  # Check that all specified EMs are continuous
  non_continuous <- setdiff(IPD_EM_cols, continuous_ems)
  if (length(non_continuous) > 0) {
    stop("Non-continuous EMs detected: ", paste(non_continuous, collapse = ", "), 
         ". Use mixed EM functions for binary + continuous combinations.")
  }
  
  message("Multiple continuous EM analysis with ", length(continuous_ems), " EMs: ",
          paste(continuous_ems, collapse = ", "))
  
  # Map EM columns between IPD and AgD
  agd_em_mapping <- AgD_EM_cols[AgD_EM_cols %in% continuous_ems | 
                               sapply(continuous_ems, function(em) any(grepl(em, AgD_EM_cols)))]
  
  if (length(agd_em_mapping) != length(continuous_ems)) {
    stop("Could not map all continuous EMs to AgD columns")
  }
  
  # Validate configuration
  if (auto_validate) {
    study_data_for_validation <- AgD[, c(agd_em_mapping, "TE", "se")]
    names(study_data_for_validation)[1:length(agd_em_mapping)] <- continuous_ems
    
    validation <- validate_multi_continuous_config(
      study_data_for_validation, 
      continuous_ems, 
      x_vect
    )
    
    message("Validation: ", validation$message)
    
    if (!validation$valid) {
      stop("Configuration validation failed: ", validation$message)
    }
    
    if (validation$recommendation %in% c("collinearity_warning", "extrapolation_warning", 
                                        "proceed_with_caution")) {
      warning(validation$message)
    }
  }
  
  # Prepare study data for interpolation
  study_data <- AgD[, c(agd_em_mapping, "TE", "se")]
  names(study_data)[1:length(agd_em_mapping)] <- continuous_ems
  
  # Remove any studies with missing EM data
  complete_cases <- complete.cases(study_data[, continuous_ems])
  if (!all(complete_cases)) {
    study_data <- study_data[complete_cases, ]
    message("Removed ", sum(!complete_cases), " studies with missing EM data")
  }
  
  # Perform multivariate interpolation
  result <- multivariate_interpolation(
    study_data = study_data,
    target_values = x_vect,
    em_cols = continuous_ems,
    method = interpolation_method,
    ...
  )
  
  # Create output in NMI format
  interpolated_data <- data.frame(
    Study = "Multi_Continuous_Interpolated",
    Trt1 = "A",  # Simplified - would need proper treatment coding
    Trt2 = "B",
    TE = result$te,
    se = result$se,
    stringsAsFactors = FALSE
  )
  
  # Add all EM values
  for (em_name in continuous_ems) {
    interpolated_data[[em_name]] <- x_vect[em_name]
  }
  
  return(list(
    Final = interpolated_data,
    method = result$method,
    extrapolation = result$extrapolation,
    em_types = em_types,
    continuous_ems = continuous_ems,
    target_values = x_vect,
    n_dimensions = result$n_dimensions,
    n_studies_used = result$n_studies_used,
    interpolation_method = interpolation_method
  ))
}

#' Cross-Validation for Multiple Continuous EMs
#' 
#' Performs leave-one-out cross-validation to assess interpolation quality
#' 
#' @param AgD Aggregate data
#' @param continuous_ems Character vector of continuous EM column names
#' @param agd_em_cols Character vector of corresponding AgD EM columns
#' @param method Interpolation method to test
#' 
#' @return Cross-validation results with performance metrics
#' @export
cross_validate_multi_continuous <- function(AgD, continuous_ems, agd_em_cols, 
                                           method = "linear_model") {
  
  n_studies <- nrow(AgD)
  if (n_studies < 3) {
    stop("Need at least 3 studies for cross-validation")
  }
  
  # Prepare study data
  study_data <- AgD[, c(agd_em_cols, "TE", "se")]
  names(study_data)[1:length(agd_em_cols)] <- continuous_ems
  
  # Initialize results storage
  cv_results <- data.frame(
    study = 1:n_studies,
    observed_te = study_data$TE,
    predicted_te = NA,
    observed_se = study_data$se,
    predicted_se = NA,
    abs_error_te = NA,
    abs_error_se = NA
  )
  
  # Leave-one-out cross-validation
  for (i in 1:n_studies) {
    # Training data (all except study i)
    train_data <- study_data[-i, ]
    
    # Target values (study i EM values)
    target_values <- setNames(as.numeric(study_data[i, continuous_ems]), continuous_ems)
    
    # Skip if insufficient training data
    if (nrow(train_data) < 2^length(continuous_ems)) {
      next
    }
    
    # Perform interpolation
    tryCatch({
      result <- multivariate_interpolation(
        study_data = train_data,
        target_values = target_values,
        em_cols = continuous_ems,
        method = method
      )
      
      cv_results$predicted_te[i] <- result$te
      cv_results$predicted_se[i] <- result$se
      cv_results$abs_error_te[i] <- abs(result$te - study_data$TE[i])
      cv_results$abs_error_se[i] <- abs(result$se - study_data$se[i])
      
    }, error = function(e) {
      # Skip this iteration if interpolation fails
      message("Interpolation failed for study ", i, ": ", e$message)
    })
  }
  
  # Calculate performance metrics
  valid_predictions <- !is.na(cv_results$predicted_te)
  n_valid <- sum(valid_predictions)
  
  if (n_valid == 0) {
    stop("No valid cross-validation predictions obtained")
  }
  
  # Performance metrics for TE
  rmse_te <- sqrt(mean(cv_results$abs_error_te[valid_predictions]^2))
  mae_te <- mean(cv_results$abs_error_te[valid_predictions])
  
  # Performance metrics for SE  
  rmse_se <- sqrt(mean(cv_results$abs_error_se[valid_predictions]^2))
  mae_se <- mean(cv_results$abs_error_se[valid_predictions])
  
  # Correlation
  cor_te <- cor(cv_results$observed_te[valid_predictions], 
               cv_results$predicted_te[valid_predictions])
  cor_se <- cor(cv_results$observed_se[valid_predictions], 
               cv_results$predicted_se[valid_predictions])
  
  return(list(
    cv_results = cv_results,
    performance = list(
      n_valid_predictions = n_valid,
      te_rmse = rmse_te,
      te_mae = mae_te,
      te_correlation = cor_te,
      se_rmse = rmse_se,
      se_mae = mae_se,
      se_correlation = cor_se
    ),
    method = method,
    n_dimensions = length(continuous_ems)
  ))
} 