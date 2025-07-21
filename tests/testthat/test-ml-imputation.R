# Tests for ML-Based Imputation (Phase 4 - v1.4.0)
# Testing machine learning imputation methods

test_that("detect_missing_patterns analyzes missing data correctly", {
  # Create test data with missing values
  IPD_missing <- data.frame(
    Study = rep("IPD_Study", 100),
    age = c(rnorm(80, 60, 10), rep(NA, 20)),  # 20% missing
    sex = c(rep(c("M", "F"), 40), rep(NA, 20)),  # 20% missing
    treatment = sample(c("A", "B"), 100, replace = TRUE),
    outcome = rbinom(100, 1, 0.4)
  )
  
  AgD_missing <- data.frame(
    Study = paste0("Study_", 1:5),
    age_mean = c(55, 60, NA, 65, 70),  # 20% missing
    sex_prop = c(0.5, NA, 0.6, 0.4, 0.5),  # 20% missing
    TE = c(0.5, 0.3, 0.4, 0.6, 0.2),
    se = c(0.1, 0.12, 0.11, 0.13, 0.14)
  )
  
  missing_analysis <- detect_missing_patterns(
    IPD_missing, AgD_missing, c("age", "sex"), "outcome"
  )
  
  expect_true("ipd_analysis" %in% names(missing_analysis))
  expect_true("agd_analysis" %in% names(missing_analysis))
  expect_true("strategy" %in% names(missing_analysis))
  expect_true("summary" %in% names(missing_analysis))
  
  # Check IPD analysis
  expect_equal(missing_analysis$ipd_analysis$missing_percentages["age"], 20)
  expect_equal(missing_analysis$ipd_analysis$missing_percentages["sex"], 20)
  expect_true(missing_analysis$ipd_analysis$completeness_rate < 1)
  
  # Check AgD analysis
  expect_equal(missing_analysis$agd_analysis$em_missing_percentages["age_mean"], 20)
  expect_equal(missing_analysis$agd_analysis$em_missing_percentages["sex_prop"], 20)
})

test_that("analyze_ipd_missing calculates correct percentages", {
  IPD_test <- data.frame(
    age = c(1:8, NA, NA),  # 20% missing
    sex = c(rep("M", 5), rep("F", 3), NA, NA),  # 20% missing
    outcome = c(1:9, NA)  # 10% missing
  )
  
  result <- analyze_ipd_missing(IPD_test, c("age", "sex"), "outcome")
  
  expect_equal(result$missing_percentages["age"], 20)
  expect_equal(result$missing_percentages["sex"], 20)
  expect_equal(result$missing_percentages["outcome"], 10)
  expect_equal(result$completeness_rate, 0.7)  # 7 out of 10 complete cases
})

test_that("analyze_agd_missing handles different scenarios", {
  # Test with missing EM data
  AgD_test <- data.frame(
    Study = paste0("S", 1:5),
    age_mean = c(50, 60, NA, 70, 80),  # 20% missing
    TE = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.05, 0.06, 0.07, 0.08, 0.09)
  )
  
  result <- analyze_agd_missing(AgD_test, "age_mean")
  
  expect_equal(result$em_missing_percentages["age_mean"], 20)
  expect_equal(result$study_completeness_rate, 0.8)  # 4 out of 5 complete
})

test_that("determine_imputation_strategy chooses appropriate methods", {
  # High completeness - should choose simple
  ipd_analysis_good <- list(completeness_rate = 0.98)
  agd_analysis_good <- list(study_completeness_rate = 0.95)
  
  strategy_simple <- determine_imputation_strategy(ipd_analysis_good, agd_analysis_good)
  expect_equal(strategy_simple$strategy_level, "simple")
  expect_equal(strategy_simple$recommended_method, "mean_mode_imputation")
  
  # Moderate completeness - should choose intermediate
  ipd_analysis_moderate <- list(completeness_rate = 0.85)
  agd_analysis_moderate <- list(study_completeness_rate = 0.80)
  
  strategy_intermediate <- determine_imputation_strategy(ipd_analysis_moderate, agd_analysis_moderate)
  expect_equal(strategy_intermediate$strategy_level, "intermediate")
  expect_equal(strategy_intermediate$recommended_method, "multiple_imputation")
  
  # Low completeness - should choose advanced
  ipd_analysis_poor <- list(completeness_rate = 0.60)
  agd_analysis_poor <- list(study_completeness_rate = 0.50)
  
  strategy_advanced <- determine_imputation_strategy(ipd_analysis_poor, agd_analysis_poor)
  expect_equal(strategy_advanced$strategy_level, "advanced")
  expect_equal(strategy_advanced$recommended_method, "ml_imputation")
})

test_that("ml_imputation validates input parameters", {
  data_test <- data.frame(
    age = c(1:8, NA, NA),
    sex = c(rep("M", 5), rep("F", 3), NA, NA),
    treatment = rep(c("A", "B"), 5)
  )
  
  # Missing target columns
  expect_error(ml_imputation(data_test, c("missing_col"), c("treatment")))
  
  # Missing predictor columns (should warn but continue)
  expect_warning(ml_imputation(data_test, c("age"), c("missing_predictor", "treatment")))
})

test_that("ml_imputation handles no missing data", {
  data_complete <- data.frame(
    age = 1:10,
    sex = rep(c("M", "F"), 5),
    treatment = rep(c("A", "B"), 5)
  )
  
  result <- ml_imputation(data_complete, c("age", "sex"), c("treatment"))
  
  expect_equal(result$method, "none_needed")
  expect_false(result$diagnostics$missing_found)
  expect_equal(length(result$imputed_data), 1)
  expect_equal(nrow(result$imputed_data[[1]]), 10)
})

test_that("ml_impute_random_forest performs imputation", {
  skip_if_not_installed("randomForest")
  
  # Create data with missing values
  set.seed(123)
  data_missing <- data.frame(
    age = c(rnorm(80, 60, 10), rep(NA, 20)),
    bmi = c(rnorm(90, 25, 3), rep(NA, 10)),
    sex = c(rep(c("M", "F"), 45), rep(NA, 10)),
    treatment = rep(c("A", "B"), 50),
    baseline_score = rnorm(100, 50, 10)
  )
  
  result <- ml_impute_random_forest(
    data_missing, 
    target_cols = c("age", "bmi", "sex"),
    predictor_cols = c("treatment", "baseline_score"),
    n_imputations = 2,
    ntree = 10  # Small for testing
  )
  
  expect_equal(result$method, "random_forest")
  expect_equal(length(result$imputed_data), 2)
  expect_equal(result$diagnostics$n_imputations, 2)
  
  # Check that missing values were imputed
  for (i in 1:2) {
    expect_false(any(is.na(result$imputed_data[[i]]$age)))
    expect_false(any(is.na(result$imputed_data[[i]]$bmi)))
    expect_false(any(is.na(result$imputed_data[[i]]$sex)))
  }
})

test_that("handle_missing_predictors works correctly", {
  predict_data <- data.frame(
    age = c(50, NA, 70),
    sex = c("M", "F", NA)
  )
  
  train_predictors <- data.frame(
    age = c(45, 55, 65, 75),
    sex = c("M", "F", "M", "F")
  )
  
  cleaned_data <- handle_missing_predictors(predict_data, train_predictors)
  
  expect_false(any(is.na(cleaned_data$age)))
  expect_false(any(is.na(cleaned_data$sex)))
  expect_equal(cleaned_data$age[2], mean(train_predictors$age))
})

test_that("evaluate_imputation_quality provides meaningful metrics", {
  skip_if_not_installed("randomForest")
  
  # Create original data
  set.seed(123)
  original_data <- data.frame(
    age = rnorm(100, 60, 10),
    bmi = rnorm(100, 25, 3),
    sex = sample(c("M", "F"), 100, replace = TRUE),
    treatment = sample(c("A", "B"), 100, replace = TRUE)
  )
  
  # Create version with missing data
  data_with_missing <- original_data
  missing_indices <- sample(1:100, 20)
  data_with_missing$age[missing_indices] <- NA
  
  # Impute
  imputed_result <- ml_imputation(
    data_with_missing,
    target_cols = "age",
    predictor_cols = c("bmi", "sex", "treatment"),
    method = "random_forest",
    n_imputations = 1
  )
  
  if (imputed_result$method != "none_needed") {
    quality <- evaluate_imputation_quality(
      data_with_missing,
      imputed_result$imputed_data[[1]], 
      "age",
      test_fraction = 0.2
    )
    
    expect_true("age" %in% names(quality))
    if (!"error" %in% names(quality$age)) {
      expect_true("rmse" %in% names(quality$age))
      expect_true("mae" %in% names(quality$age))
      expect_true("correlation" %in% names(quality$age))
      expect_equal(quality$age$type, "continuous")
    }
  }
})

test_that("nmi_with_ml_imputation full workflow", {
  # Create test data with missing values
  IPD_missing <- data.frame(
    Study = rep("IPD_Study", 50),
    age = c(rnorm(40, 60, 10), rep(NA, 10)),
    treatment = sample(c("A", "B"), 50, replace = TRUE),
    outcome = rbinom(50, 1, 0.4)
  )
  
  AgD_complete <- data.frame(
    Study = paste0("Study_", 1:3),
    Trt1 = c("A", "B", "A"),
    Trt2 = c("B", "C", "C"),
    age_mean = c(55, 60, 65),
    TE = c(0.5, 0.3, 0.7),
    se = c(0.1, 0.12, 0.11)
  )
  
  # Mock NMI_interpolation for testing
  old_nmi <- NMI_interpolation
  NMI_interpolation <<- function(...) {
    list(
      Final = data.frame(Study = "Test", TE = 0.5, se = 0.1),
      Diagnostics = data.frame(metric = "test", value = 1)
    )
  }
  
  result <- nmi_with_ml_imputation(
    IPD = IPD_missing,
    AgD = AgD_complete,
    x_vect = c(age = 62),
    AgD_EM_cols = "age_mean",
    IPD_EM_cols = "age",
    imputation_method = "random_forest",
    n_imputations = 2
  )
  
  expect_true("pooled_result" %in% names(result))
  expect_true("individual_results" %in% names(result))
  expect_true("imputation_details" %in% names(result))
  expect_equal(result$method, "nmi_with_ml_imputation")
  
  # Restore original function
  NMI_interpolation <<- old_nmi
})

test_that("pool_nmi_results combines multiple results correctly", {
  # Create mock NMI results
  nmi_results <- list(
    list(
      Final = data.frame(Study = "S1", TE = 0.5, se = 0.1),
      Diagnostics = data.frame(metric = "test", value = 1)
    ),
    list(
      Final = data.frame(Study = "S1", TE = 0.6, se = 0.12),
      Diagnostics = data.frame(metric = "test", value = 1.1)
    )
  )
  
  pooled <- pool_nmi_results(nmi_results)
  
  expect_true("Final" %in% names(pooled))
  expect_true("Diagnostics" %in% names(pooled))
  expect_equal(pooled$method, "pooled_multiple_imputation")
  expect_equal(pooled$n_imputations, 2)
  
  # Check that TE is pooled (mean of 0.5 and 0.6 = 0.55)
  expect_equal(pooled$Final$TE[1], 0.55)
})

test_that("pool_final_estimates uses Rubin's rules correctly", {
  # Create test results with known values
  nmi_results <- list(
    list(Final = data.frame(Study = "S1", TE = 1.0, se = 0.1)),
    list(Final = data.frame(Study = "S1", TE = 1.2, se = 0.1)),
    list(Final = data.frame(Study = "S1", TE = 0.8, se = 0.1))
  )
  
  pooled_final <- pool_final_estimates(nmi_results)
  
  expect_equal(nrow(pooled_final), 1)
  expect_equal(pooled_final$Study[1], "S1")
  expect_equal(pooled_final$TE[1], 1.0)  # Mean of 1.0, 1.2, 0.8
  expect_true(pooled_final$se[1] > 0.1)  # Should be inflated due to between-imputation variance
})

test_that("ml_imputation handles edge cases gracefully", {
  # Very small dataset
  small_data <- data.frame(
    age = c(50, NA, 70),
    treatment = c("A", "B", "A")
  )
  
  expect_warning({
    result_small <- ml_imputation(
      small_data, 
      target_cols = "age", 
      predictor_cols = "treatment",
      n_imputations = 1
    )
  })
  
  # All missing data
  all_missing <- data.frame(
    age = rep(NA, 10),
    treatment = rep(c("A", "B"), 5)
  )
  
  expect_warning({
    result_all_missing <- ml_imputation(
      all_missing,
      target_cols = "age",
      predictor_cols = "treatment",
      n_imputations = 1
    )
  })
})

test_that("ML imputation methods handle fallbacks correctly", {
  data_test <- data.frame(
    age = c(rnorm(8), NA, NA),
    treatment = rep(c("A", "B"), 5)
  )
  
  # Test XGBoost fallback
  result_xgb <- ml_impute_xgboost(data_test, "age", "treatment", 1)
  expect_equal(result_xgb$method, "random_forest")  # Should fall back
  
  # Test KNN fallback
  result_knn <- ml_impute_knn(data_test, "age", "treatment", 1)
  expect_equal(result_knn$method, "random_forest")  # Should fall back
}) 