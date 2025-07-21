# Tests for Mixed Effect Modifier Functionality
# Author: Ahmad Sofi-Mahmudi

test_that("validate_mixed_em_config works correctly", {
  
  # Valid mixed configuration
  em_types <- c(age = "continuous", sex = "binary")
  x_vect <- c(age = 65, sex = 1)
  
  config <- validate_mixed_em_config(em_types, x_vect)
  
  expect_true(config$valid)
  expect_equal(config$n_binary, 1)
  expect_equal(config$n_continuous, 1)
  expect_equal(config$configuration, "1B+1C+0Cat")
  expect_equal(config$binary_ems, "sex")
  expect_equal(config$continuous_ems, "age")
  
  # Test error cases
  expect_error(
    validate_mixed_em_config(em_types, c(age = 65, sex = 0.5)),
    "Binary effect modifiers must have values 0 or 1"
  )
  
  expect_error(
    validate_mixed_em_config(em_types, c(age = Inf, sex = 1)),
    "Continuous effect modifiers must have finite numeric values"
  )
  
  expect_error(
    validate_mixed_em_config(c(category = "categorical"), c(category = 1)),
    "Categorical effect modifiers not yet supported"
  )
})

test_that("prepare_mixed_em_data handles filtering correctly", {
  
  # Create test data
  IPD <- data.frame(
    Study = rep("IPD_Study", 100),
    age = rnorm(100, 60, 10),
    sex = sample(c(0, 1), 100, replace = TRUE),
    treatment = sample(c("A", "B"), 100, replace = TRUE),
    outcome = rbinom(100, 1, 0.4)
  )
  
  AgD <- data.frame(
    Study = paste0("Study_", 1:5),
    age_mean = c(50, 60, 65, 70, 75),
    TE = c(0.1, 0.3, 0.5, 0.4, 0.2),
    se = c(0.12, 0.10, 0.11, 0.13, 0.15)
  )
  
  # Test binary EM filtering
  binary_ems <- "sex"
  continuous_ems <- "age"
  x_vect <- c(age = 65, sex = 1)
  
  prepared <- prepare_mixed_em_data(IPD, AgD, binary_ems, continuous_ems, x_vect)
  
  expect_type(prepared, "list")
  expect_named(prepared, c("ipd_filtered", "agd_filtered", "binary_targets", 
                          "continuous_targets", "n_ipd_remaining", "n_agd_remaining"))
  
  # Check that IPD was filtered to sex = 1
  expect_true(all(prepared$ipd_filtered$sex == 1))
  expect_equal(prepared$binary_targets, c(sex = 1))
  expect_equal(prepared$continuous_targets, c(age = 65))
  
  # Check counts
  expect_true(prepared$n_ipd_remaining <= nrow(IPD))
  expect_equal(prepared$n_agd_remaining, nrow(AgD))  # AgD not filtered in simple implementation
})

test_that("hybrid_interpolation works for mixed EMs", {
  
  # Create prepared data
  prepared_data <- list(
    agd_filtered = data.frame(
      age_mean = c(50, 60, 70, 80),
      TE = c(0.1, 0.3, 0.5, 0.4),
      se = c(0.12, 0.10, 0.13, 0.15)
    ),
    continuous_targets = c(age = 65),
    binary_targets = c(sex = 1)
  )
  
  # Test hybrid interpolation
  result <- hybrid_interpolation(
    prepared_data = prepared_data,
    continuous_ems = "age",
    continuous_agd_cols = "age_mean",
    interpolation_method = "linear"
  )
  
  expect_type(result, "list")
  expect_true("te" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("binary_em_filter" %in% names(result))
  expect_equal(result$binary_em_filter, c(sex = 1))
  expect_equal(result$mixed_em_type, "binary_filtered_continuous_interpolated")
  
  # Test insufficient data error
  small_prepared <- prepared_data
  small_prepared$agd_filtered <- small_prepared$agd_filtered[1, ]  # Only 1 study
  
  expect_error(
    hybrid_interpolation(small_prepared, "age", "age_mean"),
    "Insufficient studies remaining after binary EM filtering"
  )
})

test_that("NMI_interpolation_mixed handles different configurations", {
  
  # Create test data
  IPD <- data.frame(
    Study = rep("IPD_Study", 100),
    age = rnorm(100, 60, 10),
    sex = sample(c(0, 1), 100, replace = TRUE),
    treatment = sample(c("A", "B"), 100, replace = TRUE),
    outcome = rbinom(100, 1, 0.4)
  )
  
  AgD <- data.frame(
    Study = paste0("Study_", 1:5),
    age_mean = c(50, 60, 65, 70, 75),
    TE = c(0.1, 0.3, 0.5, 0.4, 0.2),
    se = c(0.12, 0.10, 0.11, 0.13, 0.15)
  )
  
  # Test pure continuous (should route to NMI_interpolation_continuous)
  expect_message(
    NMI_interpolation_mixed(
      IPD = IPD,
      AgD = AgD,
      x_vect = c(age = 65),
      AgD_EM_cols = "age_mean",
      IPD_EM_cols = "age",
      em_types = c(age = "continuous")
    ),
    "Pure continuous EM detected"
  )
  
  # Test pure binary (should error with informative message)
  expect_error(
    NMI_interpolation_mixed(
      IPD = IPD,
      AgD = AgD,
      x_vect = c(sex = 1),
      AgD_EM_cols = "sex",
      IPD_EM_cols = "sex",
      em_types = c(sex = "binary")
    ),
    "Pure binary EM analysis should use standard NMI_interpolation"
  )
  
  # Test mixed binary + continuous
  expect_message(
    result <- NMI_interpolation_mixed(
      IPD = IPD,
      AgD = AgD,
      x_vect = c(age = 65, sex = 1),
      AgD_EM_cols = c("age_mean", "sex"),
      IPD_EM_cols = c("age", "sex"),
      em_types = c(age = "continuous", sex = "binary")
    ),
    "Mixed binary \\+ continuous EM analysis"
  )
  
  expect_type(result, "list")
  expect_true("Final" %in% names(result))
  expect_true("configuration" %in% names(result))
  expect_equal(result$configuration, "1B+1C+0Cat")
  expect_true(grepl("mixed_", result$method))
})

test_that("propagate_mixed_em_uncertainty adds uncertainty information", {
  
  # Create mock mixed EM result
  mixed_result <- list(
    Final = data.frame(
      Study = "Mixed_EM_Interpolated",
      TE = 0.3,
      se = 0.1
    ),
    method = "mixed_linear"
  )
  
  # Test uncertainty propagation
  expect_message(
    result_with_uncertainty <- propagate_mixed_em_uncertainty(mixed_result, NULL),
    "Uncertainty propagation for mixed EMs is experimental"
  )
  
  expect_true("se_adjusted" %in% names(result_with_uncertainty$Final))
  expect_true("ci_lower" %in% names(result_with_uncertainty$Final))
  expect_true("ci_upper" %in% names(result_with_uncertainty$Final))
  expect_true("uncertainty_method" %in% names(result_with_uncertainty))
  
  # Check that SE was inflated
  expect_true(result_with_uncertainty$Final$se_adjusted > mixed_result$Final$se)
  
  # Test error for non-mixed results
  non_mixed_result <- list(method = "linear")
  expect_error(
    propagate_mixed_em_uncertainty(non_mixed_result, NULL),
    "This function is for mixed EM results only"
  )
})

test_that("summarize_mixed_em_result produces readable output", {
  
  # Create mock mixed EM result
  mixed_result <- list(
    Final = data.frame(
      Study = "Mixed_EM_Interpolated",
      TE = 0.35,
      se = 0.12,
      ci_lower = 0.11,
      ci_upper = 0.59
    ),
    method = "mixed_linear",
    configuration = "1B+1C+0Cat",
    extrapolation = FALSE,
    em_types = c(age = "continuous", sex = "binary"),
    binary_ems = "sex",
    continuous_ems = "age",
    target_values = c(age = 65, sex = 1),
    n_studies_filtered = 4
  )
  
  # Capture output
  output <- capture.output(summarize_mixed_em_result(mixed_result))
  
  expect_true(any(grepl("Mixed Effect Modifier NMI Summary", output)))
  expect_true(any(grepl("1B\\+1C\\+0Cat", output)))
  expect_true(any(grepl("mixed_linear", output)))
  expect_true(any(grepl("Binary EMs: sex", output)))
  expect_true(any(grepl("Continuous EMs: age", output)))
  expect_true(any(grepl("TE: 0.35", output)))
  expect_true(any(grepl("95% CI:", output)))
})

test_that("mixed EM functions handle edge cases appropriately", {
  
  # Test with no IPD matching binary filter
  IPD_no_match <- data.frame(
    age = c(50, 60, 70),
    sex = c(0, 0, 0),  # All sex = 0
    treatment = c("A", "B", "A"),
    outcome = c(0, 1, 1)
  )
  
  AgD <- data.frame(
    age_mean = c(55, 65),
    TE = c(0.2, 0.4),
    se = c(0.1, 0.12)
  )
  
  expect_warning(
    prepare_mixed_em_data(
      IPD_no_match, AgD, "sex", "age", c(age = 60, sex = 1)
    ),
    "No IPD observations match the target binary EM values"
  )
  
  # Test with multiple continuous EMs warning
  em_types_multi <- c(age = "continuous", bmi = "continuous")
  x_vect_multi <- c(age = 65, bmi = 25)
  
  expect_warning(
    validate_mixed_em_config(em_types_multi, x_vect_multi),
    "Multiple continuous EMs detected"
  )
})

test_that("mixed EM integration with existing functions works", {
  
  # Test that detect_em_types works with mixed data
  mixed_data <- data.frame(
    age = rnorm(50, 60, 10),
    sex = sample(c(0, 1), 50, replace = TRUE),
    smoking = sample(c(0, 1), 50, replace = TRUE),
    bmi = rnorm(50, 25, 5)
  )
  
  em_types <- detect_em_types(mixed_data, c("age", "sex", "smoking", "bmi"))
  
  expect_equal(em_types["age"], c(age = "continuous"))
  expect_equal(em_types["sex"], c(sex = "binary"))
  expect_equal(em_types["smoking"], c(smoking = "binary"))
  expect_equal(em_types["bmi"], c(bmi = "continuous"))
  
  # Test that we can create a valid mixed configuration
  x_vect <- c(age = 65, sex = 1, smoking = 0, bmi = 26)
  
  # This should work (though with warnings about multiple continuous EMs)
  expect_warning(
    config <- validate_mixed_em_config(em_types, x_vect),
    "Multiple continuous EMs detected"
  )
  
  expect_equal(config$n_binary, 2)
  expect_equal(config$n_continuous, 2)
  expect_equal(config$configuration, "2B+2C+0Cat")
}) 