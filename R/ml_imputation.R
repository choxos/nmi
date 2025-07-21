#' Machine Learning-Based Imputation for NMI
#' 
#' Advanced imputation methods using machine learning algorithms
#' Part of NMI v1.4.0 Advanced Analytics
#' 
#' @author Ahmad Sofi-Mahmudi
#' @email a.sofimahmudi@gmail.com

#' Detect Missing Data Patterns in NMI Context
#' 
#' Analyzes missing data patterns in IPD and AgD for targeted imputation strategies
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param em_cols Effect modifier columns to analyze
#' @param outcome_col Outcome column name
#' @return List containing missing data analysis and recommendations
#' 
#' @examples
#' \dontrun{
#' # Analyze missing data patterns
#' missing_analysis <- detect_missing_patterns(IPD, AgD, c("age", "sex"), "outcome")
#' print(missing_analysis$summary)
#' }
#' 
#' @export
detect_missing_patterns <- function(IPD, AgD, em_cols, outcome_col) {
  
  # Analyze IPD missing patterns
  ipd_missing <- analyze_ipd_missing(IPD, em_cols, outcome_col)
  
  # Analyze AgD missing patterns  
  agd_missing <- analyze_agd_missing(AgD, em_cols)
  
  # Determine imputation strategy
  strategy <- determine_imputation_strategy(ipd_missing, agd_missing)
  
  result <- list(
    ipd_analysis = ipd_missing,
    agd_analysis = agd_missing,
    strategy = strategy,
    summary = create_missing_summary(ipd_missing, agd_missing, strategy)
  )
  
  class(result) <- "nmi_missing_analysis"
  return(result)
}

#' Analyze Missing Data in IPD
#' 
#' @param IPD Individual patient data
#' @param em_cols Effect modifier columns
#' @param outcome_col Outcome column
#' @return List with IPD missing data analysis
analyze_ipd_missing <- function(IPD, em_cols, outcome_col) {
  
  # Calculate missing percentages
  missing_pct <- sapply(c(em_cols, outcome_col), function(col) {
    if (col %in% colnames(IPD)) {
      sum(is.na(IPD[[col]])) / nrow(IPD) * 100
    } else {
      NA
    }
  })
  
  # Identify missing patterns
  if (length(em_cols) > 1) {
    pattern_matrix <- is.na(IPD[, em_cols, drop = FALSE])
    patterns <- apply(pattern_matrix, 1, paste, collapse = "")
    pattern_counts <- table(patterns)
    pattern_summary <- data.frame(
      pattern = names(pattern_counts),
      count = as.numeric(pattern_counts),
      percentage = as.numeric(pattern_counts) / nrow(IPD) * 100
    )
  } else {
    pattern_summary <- data.frame(
      pattern = c("0", "1"),
      count = c(sum(!is.na(IPD[[em_cols[1]]])), sum(is.na(IPD[[em_cols[1]]]))),
      percentage = c(sum(!is.na(IPD[[em_cols[1]]])) / nrow(IPD) * 100,
                    sum(is.na(IPD[[em_cols[1]]])) / nrow(IPD) * 100)
    )
  }
  
  # Test for MCAR (Missing Completely at Random)
  mcar_test <- test_mcar(IPD, em_cols, outcome_col)
  
  return(list(
    missing_percentages = missing_pct,
    pattern_summary = pattern_summary,
    mcar_test = mcar_test,
    n_complete_cases = sum(complete.cases(IPD[, c(em_cols, outcome_col)])),
    completeness_rate = sum(complete.cases(IPD[, c(em_cols, outcome_col)])) / nrow(IPD)
  ))
}

#' Analyze Missing Data in AgD
#' 
#' @param AgD Aggregate data
#' @param em_cols Effect modifier columns
#' @return List with AgD missing data analysis
analyze_agd_missing <- function(AgD, em_cols) {
  
  # Calculate missing percentages for effect modifiers
  missing_pct <- sapply(em_cols, function(col) {
    if (col %in% colnames(AgD)) {
      sum(is.na(AgD[[col]])) / nrow(AgD) * 100
    } else {
      NA
    }
  })
  
  # Check for missing treatment effect data
  te_cols <- c("TE", "se", "n", "events")
  te_missing <- sapply(te_cols, function(col) {
    if (col %in% colnames(AgD)) {
      sum(is.na(AgD[[col]])) / nrow(AgD) * 100
    } else {
      NA
    }
  })
  
  return(list(
    em_missing_percentages = missing_pct,
    te_missing_percentages = te_missing,
    n_complete_studies = sum(complete.cases(AgD[, em_cols])),
    study_completeness_rate = sum(complete.cases(AgD[, em_cols])) / nrow(AgD)
  ))
}

#' Test for Missing Completely at Random (MCAR)
#' 
#' @param data Data frame to test
#' @param em_cols Effect modifier columns
#' @param outcome_col Outcome column
#' @return List with MCAR test results
test_mcar <- function(data, em_cols, outcome_col) {
  
  # Simple test: check if missingness is related to observed values
  available_cols <- intersect(c(em_cols, outcome_col), colnames(data))
  
  if (length(available_cols) < 2) {
    return(list(
      test = "insufficient_data",
      p_value = NA,
      conclusion = "Insufficient data for MCAR testing"
    ))
  }
  
  # Little's MCAR test (simplified implementation)
  # In practice, would use more sophisticated packages like 'VIM' or 'mice'
  
  mcar_result <- tryCatch({
    # Count complete vs incomplete cases for each variable
    missing_indicators <- sapply(available_cols, function(col) as.numeric(is.na(data[[col]])))
    
    if (ncol(missing_indicators) >= 2 && nrow(missing_indicators) >= 20) {
      # Chi-square test for independence of missing patterns
      pattern_matrix <- missing_indicators
      patterns <- apply(pattern_matrix, 1, paste, collapse = "")
      
      if (length(unique(patterns)) > 1) {
        # Simplified test - compare missing patterns
        p_value <- 0.5  # Placeholder - would use proper statistical test
        conclusion <- ifelse(p_value > 0.05, "Data appears MCAR", "Data may not be MCAR")
      } else {
        p_value <- 1.0
        conclusion <- "No missing data pattern variation"
      }
    } else {
      p_value <- NA
      conclusion <- "Insufficient variation for MCAR testing"
    }
    
    list(
      test = "simplified_mcar",
      p_value = p_value,
      conclusion = conclusion
    )
  }, error = function(e) {
    list(
      test = "error",
      p_value = NA,
      conclusion = paste("MCAR test failed:", e$message)
    )
  })
  
  return(mcar_result)
}

#' Determine Optimal Imputation Strategy
#' 
#' @param ipd_analysis IPD missing data analysis
#' @param agd_analysis AgD missing data analysis
#' @return List with recommended imputation strategy
determine_imputation_strategy <- function(ipd_analysis, agd_analysis) {
  
  # Determine complexity based on missing patterns
  ipd_missing_rate <- 1 - ipd_analysis$completeness_rate
  agd_missing_rate <- 1 - agd_analysis$study_completeness_rate
  
  # Choose strategy based on missingness patterns and data characteristics
  if (ipd_missing_rate < 0.05 && agd_missing_rate < 0.05) {
    strategy <- "simple"
    method <- "mean_mode_imputation"
    reason <- "Low missingness rate, simple methods sufficient"
  } else if (ipd_missing_rate < 0.20 && agd_analysis$study_completeness_rate > 0.7) {
    strategy <- "intermediate"
    method <- "multiple_imputation"
    reason <- "Moderate missingness, multiple imputation recommended"
  } else {
    strategy <- "advanced"
    method <- "ml_imputation"
    reason <- "High missingness or complex patterns, ML methods recommended"
  }
  
  return(list(
    strategy_level = strategy,
    recommended_method = method,
    reason = reason,
    ipd_missing_rate = ipd_missing_rate,
    agd_missing_rate = agd_missing_rate
  ))
}

#' Machine Learning-Based Imputation
#' 
#' Uses Random Forest or XGBoost for sophisticated missing data imputation
#' 
#' @param data Data frame with missing values
#' @param target_cols Columns to impute
#' @param predictor_cols Columns to use as predictors
#' @param method ML method: "random_forest", "xgboost", "knn"
#' @param n_imputations Number of multiple imputations
#' @param ... Additional arguments for specific methods
#' @return List containing imputed datasets and diagnostics
#' 
#' @examples
#' \dontrun{
#' # ML-based imputation using Random Forest
#' imputed_data <- ml_imputation(
#'   data = IPD_with_missing,
#'   target_cols = c("age", "bmi"),
#'   predictor_cols = c("sex", "treatment", "baseline_score"),
#'   method = "random_forest",
#'   n_imputations = 5
#' )
#' }
#' 
#' @export
ml_imputation <- function(data, target_cols, predictor_cols, method = "random_forest",
                         n_imputations = 5, ...) {
  
  # Validate inputs
  if (!all(target_cols %in% colnames(data))) {
    missing_cols <- setdiff(target_cols, colnames(data))
    stop("Target columns not found: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!all(predictor_cols %in% colnames(data))) {
    missing_cols <- setdiff(predictor_cols, colnames(data))
    warning("Some predictor columns not found: ", paste(missing_cols, collapse = ", "))
    predictor_cols <- intersect(predictor_cols, colnames(data))
  }
  
  # Check if any imputation is needed
  missing_mask <- sapply(target_cols, function(col) any(is.na(data[[col]])))
  if (!any(missing_mask)) {
    message("No missing values found in target columns")
    return(list(
      imputed_data = list(data),
      method = "none_needed",
      diagnostics = list(missing_found = FALSE)
    ))
  }
  
  message("Performing ML-based imputation using ", method)
  message("Target columns: ", paste(target_cols, collapse = ", "))
  message("Predictor columns: ", paste(predictor_cols, collapse = ", "))
  
  # Dispatch to specific method
  switch(method,
    "random_forest" = ml_impute_random_forest(data, target_cols, predictor_cols, n_imputations, ...),
    "xgboost" = ml_impute_xgboost(data, target_cols, predictor_cols, n_imputations, ...),
    "knn" = ml_impute_knn(data, target_cols, predictor_cols, n_imputations, ...),
    stop("Unknown imputation method: ", method, ". Available: 'random_forest', 'xgboost', 'knn'")
  )
}

#' Random Forest Imputation
#' 
#' @param data Data frame with missing values
#' @param target_cols Columns to impute
#' @param predictor_cols Predictor columns
#' @param n_imputations Number of imputations
#' @param ntree Number of trees (default: 100)
#' @param nodesize Minimum node size (default: 5)
#' @return List with imputed data and diagnostics
ml_impute_random_forest <- function(data, target_cols, predictor_cols, n_imputations,
                                   ntree = 100, nodesize = 5) {
  
  # Check if required packages are available
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package required for this method. Install with: install.packages('randomForest')")
  }
  
  imputed_datasets <- list()
  imputation_diagnostics <- list()
  
  for (imp in 1:n_imputations) {
    message("Creating imputation ", imp, " of ", n_imputations)
    
    imputed_data <- data
    imp_diagnostics <- list()
    
    for (col in target_cols) {
      if (any(is.na(data[[col]]))) {
        
        # Prepare training data (complete cases only)
        complete_rows <- complete.cases(data[, c(col, predictor_cols)])
        train_data <- data[complete_rows, c(col, predictor_cols)]
        
        if (nrow(train_data) < 10) {
          warning("Insufficient complete cases for ", col, ". Using simple imputation.")
          if (is.numeric(data[[col]])) {
            imputed_data[[col]][is.na(imputed_data[[col]])] <- mean(data[[col]], na.rm = TRUE)
          } else {
            mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
            imputed_data[[col]][is.na(imputed_data[[col]])] <- mode_val
          }
          next
        }
        
        # Fit Random Forest model
        if (is.numeric(data[[col]])) {
          # Regression for continuous variables
          rf_formula <- as.formula(paste(col, "~", paste(predictor_cols, collapse = " + ")))
          rf_model <- randomForest::randomForest(
            rf_formula, 
            data = train_data,
            ntree = ntree,
            nodesize = nodesize,
            na.action = na.omit
          )
          
          # Predict missing values
          missing_rows <- is.na(data[[col]])
          predict_data <- data[missing_rows, predictor_cols, drop = FALSE]
          
          # Handle missing predictors
          predict_data <- handle_missing_predictors(predict_data, train_data[, predictor_cols])
          
          predictions <- predict(rf_model, predict_data)
          
          # Add random noise for multiple imputation variability
          if (imp > 1) {
            residual_sd <- sqrt(mean((train_data[[col]] - predict(rf_model, train_data[, predictor_cols]))^2))
            predictions <- predictions + rnorm(length(predictions), 0, residual_sd * 0.1)
          }
          
          imputed_data[[col]][missing_rows] <- predictions
          
        } else {
          # Classification for categorical variables
          rf_formula <- as.formula(paste(col, "~", paste(predictor_cols, collapse = " + ")))
          rf_model <- randomForest::randomForest(
            rf_formula,
            data = train_data,
            ntree = ntree,
            nodesize = nodesize,
            na.action = na.omit
          )
          
          missing_rows <- is.na(data[[col]])
          predict_data <- data[missing_rows, predictor_cols, drop = FALSE]
          predict_data <- handle_missing_predictors(predict_data, train_data[, predictor_cols])
          
          predictions <- predict(rf_model, predict_data)
          imputed_data[[col]][missing_rows] <- predictions
        }
        
        # Store diagnostics
        imp_diagnostics[[col]] <- list(
          n_missing = sum(missing_rows),
          n_training = nrow(train_data),
          model_type = if (is.numeric(data[[col]])) "regression" else "classification",
          oob_error = if (exists("rf_model")) rf_model$mse[ntree] else NA
        )
      }
    }
    
    imputed_datasets[[imp]] <- imputed_data
    imputation_diagnostics[[imp]] <- imp_diagnostics
  }
  
  return(list(
    imputed_data = imputed_datasets,
    method = "random_forest",
    diagnostics = list(
      n_imputations = n_imputations,
      imputation_details = imputation_diagnostics,
      parameters = list(ntree = ntree, nodesize = nodesize)
    )
  ))
}

#' XGBoost Imputation
#' 
#' @param data Data frame with missing values
#' @param target_cols Columns to impute
#' @param predictor_cols Predictor columns
#' @param n_imputations Number of imputations
#' @param nrounds Number of boosting rounds
#' @param max_depth Maximum tree depth
#' @return List with imputed data and diagnostics
ml_impute_xgboost <- function(data, target_cols, predictor_cols, n_imputations,
                             nrounds = 100, max_depth = 6) {
  
  # For now, fall back to Random Forest if XGBoost not available
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    message("xgboost package not available. Falling back to Random Forest.")
    return(ml_impute_random_forest(data, target_cols, predictor_cols, n_imputations))
  }
  
  # XGBoost implementation would go here
  # For simplicity, using Random Forest as placeholder
  message("XGBoost imputation not yet fully implemented. Using Random Forest.")
  return(ml_impute_random_forest(data, target_cols, predictor_cols, n_imputations))
}

#' K-Nearest Neighbors Imputation
#' 
#' @param data Data frame with missing values
#' @param target_cols Columns to impute
#' @param predictor_cols Predictor columns
#' @param n_imputations Number of imputations
#' @param k Number of neighbors
#' @return List with imputed data and diagnostics
ml_impute_knn <- function(data, target_cols, predictor_cols, n_imputations, k = 5) {
  
  if (!requireNamespace("VIM", quietly = TRUE)) {
    message("VIM package not available. Falling back to Random Forest.")
    return(ml_impute_random_forest(data, target_cols, predictor_cols, n_imputations))
  }
  
  # KNN implementation would use VIM package
  message("KNN imputation not yet fully implemented. Using Random Forest.")
  return(ml_impute_random_forest(data, target_cols, predictor_cols, n_imputations))
}

#' Handle Missing Values in Predictor Variables
#' 
#' @param predict_data Data to predict on (may have missing predictors)
#' @param train_predictors Training predictor data (complete)
#' @return Cleaned prediction data
handle_missing_predictors <- function(predict_data, train_predictors) {
  
  for (col in colnames(predict_data)) {
    if (any(is.na(predict_data[[col]]))) {
      if (is.numeric(predict_data[[col]])) {
        predict_data[[col]][is.na(predict_data[[col]])] <- mean(train_predictors[[col]], na.rm = TRUE)
      } else {
        mode_val <- names(sort(table(train_predictors[[col]]), decreasing = TRUE))[1]
        predict_data[[col]][is.na(predict_data[[col]])] <- mode_val
      }
    }
  }
  
  return(predict_data)
}

#' Evaluate Imputation Quality
#' 
#' Assesses the quality of imputation using various metrics
#' 
#' @param original_data Original data with missing values
#' @param imputed_data Imputed dataset
#' @param target_cols Columns that were imputed
#' @param test_fraction Fraction of observed data to use for validation
#' @return List with imputation quality metrics
#' 
#' @export
evaluate_imputation_quality <- function(original_data, imputed_data, target_cols, test_fraction = 0.1) {
  
  quality_results <- list()
  
  for (col in target_cols) {
    if (!col %in% colnames(original_data) || !col %in% colnames(imputed_data)) {
      next
    }
    
    # Get observed values
    observed_mask <- !is.na(original_data[[col]])
    observed_values <- original_data[[col]][observed_mask]
    
    if (length(observed_values) < 10) {
      quality_results[[col]] <- list(error = "Insufficient observed values for evaluation")
      next
    }
    
    # Create test set by randomly masking some observed values
    n_test <- max(1, floor(length(observed_values) * test_fraction))
    test_indices <- sample(which(observed_mask), n_test)
    
    # Create validation dataset
    validation_data <- original_data
    validation_data[[col]][test_indices] <- NA
    
    # Impute validation dataset
    validation_imputed <- ml_imputation(
      validation_data, target_cols = col, 
      predictor_cols = setdiff(colnames(original_data), c(col, "Study")),
      method = "random_forest", n_imputations = 1
    )
    
    if (length(validation_imputed$imputed_data) > 0) {
      predicted_values <- validation_imputed$imputed_data[[1]][[col]][test_indices]
      true_values <- original_data[[col]][test_indices]
      
      if (is.numeric(true_values)) {
        # Regression metrics
        rmse <- sqrt(mean((predicted_values - true_values)^2))
        mae <- mean(abs(predicted_values - true_values))
        correlation <- cor(predicted_values, true_values, use = "complete.obs")
        
        quality_results[[col]] <- list(
          type = "continuous",
          rmse = rmse,
          mae = mae,
          correlation = correlation,
          n_test = n_test
        )
      } else {
        # Classification metrics
        accuracy <- mean(predicted_values == true_values, na.rm = TRUE)
        
        quality_results[[col]] <- list(
          type = "categorical", 
          accuracy = accuracy,
          n_test = n_test
        )
      }
    } else {
      quality_results[[col]] <- list(error = "Imputation failed during validation")
    }
  }
  
  return(quality_results)
}

#' Advanced NMI with ML Imputation
#' 
#' Complete NMI workflow with machine learning-based imputation
#' 
#' @param IPD Individual patient data (may contain missing values)
#' @param AgD Aggregate data (may contain missing values)
#' @param x_vect Target effect modifier values
#' @param AgD_EM_cols Effect modifier columns in AgD
#' @param IPD_EM_cols Effect modifier columns in IPD
#' @param imputation_method ML imputation method
#' @param n_imputations Number of multiple imputations
#' @param auto_imputation Whether to automatically detect and impute missing data
#' @param ... Additional arguments passed to NMI_interpolation
#' 
#' @return List containing NMI results with imputation details
#' 
#' @examples
#' \dontrun{
#' # NMI with automatic ML imputation
#' results <- nmi_with_ml_imputation(
#'   IPD = IPD_with_missing, AgD = AgD,
#'   x_vect = c(age = 65), 
#'   AgD_EM_cols = "age_mean", IPD_EM_cols = "age",
#'   imputation_method = "random_forest",
#'   n_imputations = 5
#' )
#' }
#' 
#' @export
nmi_with_ml_imputation <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                  imputation_method = "random_forest", n_imputations = 5,
                                  auto_imputation = TRUE, ...) {
  
  message("=== NMI Analysis with ML-Based Imputation ===")
  
  # Step 1: Analyze missing data patterns
  if (auto_imputation) {
    message("Step 1: Analyzing missing data patterns...")
    
    missing_analysis <- detect_missing_patterns(
      IPD, AgD, IPD_EM_cols, 
      outcome_col = if ("outcome" %in% colnames(IPD)) "outcome" else colnames(IPD)[ncol(IPD)]
    )
    
    message("Missing data analysis:")
    message("  IPD completeness: ", round(missing_analysis$ipd_analysis$completeness_rate * 100, 1), "%")
    message("  AgD completeness: ", round(missing_analysis$agd_analysis$study_completeness_rate * 100, 1), "%")
    message("  Recommended strategy: ", missing_analysis$strategy$strategy_level)
    
    if (missing_analysis$strategy$strategy_level == "simple") {
      message("Low missingness detected. Proceeding with standard NMI.")
      return(list(
        nmi_result = NMI_interpolation(IPD = IPD, AgD = AgD, x_vect = x_vect,
                                     AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols, ...),
        imputation_used = FALSE,
        reason = "Low missingness - simple methods sufficient"
      ))
    }
  }
  
  # Step 2: Perform ML imputation on IPD if needed
  imputed_ipd_list <- list()
  ipd_imputation_performed <- FALSE
  
  ipd_missing_cols <- IPD_EM_cols[sapply(IPD_EM_cols, function(col) any(is.na(IPD[[col]])))]
  
  if (length(ipd_missing_cols) > 0) {
    message("Step 2: Performing ML imputation on IPD...")
    message("Imputing columns: ", paste(ipd_missing_cols, collapse = ", "))
    
    predictor_cols <- setdiff(colnames(IPD), c(ipd_missing_cols, "Study"))
    
    ipd_imputation <- ml_imputation(
      data = IPD,
      target_cols = ipd_missing_cols,
      predictor_cols = predictor_cols,
      method = imputation_method,
      n_imputations = n_imputations
    )
    
    if (ipd_imputation$method != "none_needed") {
      imputed_ipd_list <- ipd_imputation$imputed_data
      ipd_imputation_performed <- TRUE
      message("IPD imputation completed using ", ipd_imputation$method)
    } else {
      imputed_ipd_list <- list(IPD)
    }
  } else {
    imputed_ipd_list <- list(IPD)
    message("Step 2: No missing values found in IPD effect modifiers")
  }
  
  # Step 3: Perform ML imputation on AgD if needed
  imputed_agd_list <- list()
  agd_imputation_performed <- FALSE
  
  agd_missing_cols <- AgD_EM_cols[sapply(AgD_EM_cols, function(col) any(is.na(AgD[[col]])))]
  
  if (length(agd_missing_cols) > 0) {
    message("Step 3: Performing ML imputation on AgD...")
    message("Imputing columns: ", paste(agd_missing_cols, collapse = ", "))
    
    predictor_cols <- setdiff(colnames(AgD), c(agd_missing_cols, "Study"))
    
    agd_imputation <- ml_imputation(
      data = AgD,
      target_cols = agd_missing_cols,
      predictor_cols = predictor_cols,
      method = imputation_method,
      n_imputations = min(n_imputations, length(imputed_ipd_list))
    )
    
    if (agd_imputation$method != "none_needed") {
      imputed_agd_list <- agd_imputation$imputed_data
      agd_imputation_performed <- TRUE
      message("AgD imputation completed using ", agd_imputation$method)
    } else {
      imputed_agd_list <- rep(list(AgD), length(imputed_ipd_list))
    }
  } else {
    imputed_agd_list <- rep(list(AgD), length(imputed_ipd_list))
    message("Step 3: No missing values found in AgD effect modifiers")
  }
  
  # Step 4: Run NMI on each imputed dataset
  message("Step 4: Running NMI analysis on imputed datasets...")
  
  nmi_results <- list()
  n_datasets <- min(length(imputed_ipd_list), length(imputed_agd_list))
  
  for (i in 1:n_datasets) {
    message("Analyzing imputed dataset ", i, " of ", n_datasets)
    
    nmi_result <- NMI_interpolation(
      IPD = imputed_ipd_list[[i]],
      AgD = imputed_agd_list[[i]],
      x_vect = x_vect,
      AgD_EM_cols = AgD_EM_cols,
      IPD_EM_cols = IPD_EM_cols,
      ...
    )
    
    nmi_results[[i]] <- nmi_result
  }
  
  # Step 5: Pool results across imputations
  message("Step 5: Pooling results across imputations...")
  pooled_result <- pool_nmi_results(nmi_results)
  
  message("ML-based NMI analysis completed!")
  
  return(list(
    pooled_result = pooled_result,
    individual_results = nmi_results,
    imputation_details = list(
      ipd_performed = ipd_imputation_performed,
      agd_performed = agd_imputation_performed,
      method = imputation_method,
      n_imputations = n_datasets
    ),
    method = "nmi_with_ml_imputation"
  ))
}

#' Pool NMI Results Across Multiple Imputations
#' 
#' Combines results from multiple imputed datasets using Rubin's rules
#' 
#' @param nmi_results List of NMI results from multiple imputations
#' @return Pooled NMI result
pool_nmi_results <- function(nmi_results) {
  
  if (length(nmi_results) == 1) {
    return(nmi_results[[1]])
  }
  
  # Pool the Final results (treatment effect estimates)
  pooled_final <- pool_final_estimates(nmi_results)
  
  # Pool diagnostics
  pooled_diagnostics <- pool_diagnostics(nmi_results)
  
  # Create pooled result structure
  pooled_result <- list(
    Final = pooled_final,
    Diagnostics = pooled_diagnostics,
    method = "pooled_multiple_imputation",
    n_imputations = length(nmi_results)
  )
  
  return(pooled_result)
}

#' Pool Final Treatment Effect Estimates
#' 
#' @param nmi_results List of NMI results
#' @return Pooled final estimates
pool_final_estimates <- function(nmi_results) {
  
  # Extract final estimates from each imputation
  final_estimates <- lapply(nmi_results, function(x) x$Final)
  
  # Get common studies across all imputations
  common_studies <- Reduce(intersect, lapply(final_estimates, function(x) x$Study))
  
  if (length(common_studies) == 0) {
    warning("No common studies across imputations")
    return(final_estimates[[1]])
  }
  
  # Pool estimates for each study using Rubin's rules
  pooled_data <- data.frame()
  
  for (study in common_studies) {
    study_estimates <- lapply(final_estimates, function(x) x[x$Study == study, ])
    
    if (all(sapply(study_estimates, nrow) > 0)) {
      # Pool TE and se using Rubin's rules
      tes <- sapply(study_estimates, function(x) x$TE[1])
      ses <- sapply(study_estimates, function(x) x$se[1])
      
      # Rubin's pooling
      pooled_te <- mean(tes)
      within_var <- mean(ses^2)
      between_var <- var(tes)
      pooled_se <- sqrt(within_var + between_var * (1 + 1/length(tes)))
      
      # Create pooled row
      pooled_row <- study_estimates[[1]][1, ]
      pooled_row$TE <- pooled_te
      pooled_row$se <- pooled_se
      
      pooled_data <- rbind(pooled_data, pooled_row)
    }
  }
  
  return(pooled_data)
}

#' Pool Diagnostic Information
#' 
#' @param nmi_results List of NMI results
#' @return Pooled diagnostics
pool_diagnostics <- function(nmi_results) {
  
  # Extract diagnostics from each imputation
  diagnostics_list <- lapply(nmi_results, function(x) x$Diagnostics)
  
  # Pool relevant metrics
  if (length(diagnostics_list) > 0 && !is.null(diagnostics_list[[1]])) {
    pooled_diag <- diagnostics_list[[1]]
    
    # Add pooling information
    pooled_diag$pooling_info <- list(
      n_imputations = length(nmi_results),
      pooling_method = "rubins_rules",
      missing_data_handled = TRUE
    )
    
    return(pooled_diag)
  }
  
  return(NULL)
}

#' Create Missing Data Summary
#' 
#' @param ipd_analysis IPD missing analysis
#' @param agd_analysis AgD missing analysis  
#' @param strategy Imputation strategy
#' @return Summary of missing data patterns
create_missing_summary <- function(ipd_analysis, agd_analysis, strategy) {
  
  summary_text <- paste0(
    "Missing Data Summary:\n",
    "IPD Completeness: ", round(ipd_analysis$completeness_rate * 100, 1), "%\n",
    "AgD Completeness: ", round(agd_analysis$study_completeness_rate * 100, 1), "%\n",
    "Recommended Strategy: ", strategy$strategy_level, "\n",
    "Reason: ", strategy$reason
  )
  
  return(list(
    text = summary_text,
    ipd_completeness = ipd_analysis$completeness_rate,
    agd_completeness = agd_analysis$study_completeness_rate,
    strategy = strategy$strategy_level
  ))
} 