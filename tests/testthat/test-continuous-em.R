# Tests for Continuous Effect Modifier Functionality
# Author: Ahmad Sofi-Mahmudi

test_that("detect_em_types correctly identifies EM types", {
  
  # Test data with different EM types
  test_data <- data.frame(
    age = c(45.5, 50.2, 65.8, 72.1, 55.9),
    sex = c(0, 1, 1, 0, 1),
    smoking = c(0, 0, 1, 1, 0),
    bmi = c(22.5, 28.3, 30.1, 25.7, 27.8),
    category = c(1, 2, 3, 1, 2)
  )
  
  em_cols <- c("age", "sex", "smoking", "bmi", "category")
  types <- detect_em_types(test_data, em_cols)
  
  # Check types
  expect_equal(types["age"], c(age = "continuous"))
  expect_equal(types["sex"], c(sex = "binary"))
  expect_equal(types["smoking"], c(smoking = "binary"))
  expect_equal(types["bmi"], c(bmi = "continuous"))
  expect_equal(types["category"], c(category = "categorical"))
  
  # Test error handling
  expect_error(detect_em_types(test_data, c("age", "nonexistent")))
})

test_that("linear_interpolation works correctly", {
  
  # Create test study data
  study_data <- data.frame(
    age = c(50, 60, 70, 80),
    TE = c(0.2, 0.4, 0.5, 0.3),
    se = c(0.1, 0.12, 0.15, 0.18)
  )
  
  target_values <- c(age = 65)
  
  # Test linear interpolation
  result <- linear_interpolation(study_data, target_values, "age")
  
  expect_type(result, "list")
  expect_named(result, c("te", "se", "method", "extrapolation"))
  expect_equal(result$method, "linear")
  expect_false(result$extrapolation)
  expect_true(is.numeric(result$te))
  expect_true(is.numeric(result$se))
  
  # Test interpolated values are reasonable
  expect_true(result$te >= min(study_data$TE) & result$te <= max(study_data$TE))
  expect_true(result$se >= min(study_data$se) & result$se <= max(study_data$se))
})

test_that("linear_interpolation handles extrapolation correctly", {
  
  study_data <- data.frame(
    age = c(50, 60, 70),
    TE = c(0.2, 0.4, 0.5),
    se = c(0.1, 0.12, 0.15)
  )
  
  # Test extrapolation below range
  target_values_low <- c(age = 40)
  expect_warning(
    result_low <- linear_interpolation(study_data, target_values_low, "age"),
    "outside observed range"
  )
  expect_true(result_low$extrapolation)
  
  # Test extrapolation above range
  target_values_high <- c(age = 80)
  expect_warning(
    result_high <- linear_interpolation(study_data, target_values_high, "age"),
    "outside observed range"
  )
  expect_true(result_high$extrapolation)
})

test_that("spline_interpolation works correctly", {
  
  # Create test study data with enough points for spline
  study_data <- data.frame(
    age = c(40, 50, 60, 70, 80),
    TE = c(0.1, 0.3, 0.5, 0.4, 0.2),
    se = c(0.08, 0.1, 0.12, 0.14, 0.16)
  )
  
  target_values <- c(age = 65)
  
  # Test natural spline
  result <- spline_interpolation(study_data, target_values, "age", spline_type = "natural")
  
  expect_type(result, "list")
  expect_named(result, c("te", "se", "method", "extrapolation"))
  expect_equal(result$method, "spline_natural")
  expect_false(result$extrapolation)
  
  # Test smoothing spline
  result_smooth <- spline_interpolation(study_data, target_values, "age", spline_type = "smoothing")
  expect_equal(result_smooth$method, "spline_smoothing")
})

test_that("spline_interpolation falls back to linear with insufficient data", {
  
  # Test with only 2 data points
  study_data <- data.frame(
    age = c(50, 70),
    TE = c(0.2, 0.5),
    se = c(0.1, 0.15)
  )
  
  target_values <- c(age = 60)
  
  expect_warning(
    result <- spline_interpolation(study_data, target_values, "age"),
    "Too few data points for spline interpolation"
  )
  expect_equal(result$method, "linear")
})

test_that("optimize_discretization works correctly", {
  
  # Generate test data
  set.seed(123)
  continuous_data <- rnorm(100, mean = 50, sd = 10)
  
  # Test equal frequency discretization
  result_freq <- optimize_discretization(continuous_data, method = "equal_freq", max_bins = 5)
  
  expect_type(result_freq, "list")
  expect_named(result_freq, c("discretized", "n_bins", "breaks", "method", "score", "bin_counts"))
  expect_equal(result_freq$method, "equal_freq")
  expect_true(result_freq$n_bins >= 2)
  expect_true(all(result_freq$bin_counts >= 5))  # min_bin_size default is 5
  
  # Test equal width discretization
  result_width <- optimize_discretization(continuous_data, method = "equal_width", max_bins = 4)
  expect_equal(result_width$method, "equal_width")
  
  # Test error with insufficient data
  small_data <- continuous_data[1:8]  # Less than min_bin_size * 2
  expect_error(
    optimize_discretization(small_data),
    "Insufficient data for discretization"
  )
})

test_that("NMI_interpolation_continuous handles single continuous EM", {
  
  # Create test IPD data
  set.seed(123)
  IPD <- data.frame(
    Study = rep(1, 100),
    age = rnorm(100, 60, 10),
    Tr = sample(c(0, 1), 100, replace = TRUE),
    Y = rbinom(100, 1, 0.4)
  )
  
  # Create test AgD data
  AgD <- data.frame(
    Study = 1:4,
    age_mean = c(50, 60, 70, 80),
    TE = c(0.2, 0.4, 0.5, 0.3),
    se = c(0.1, 0.12, 0.15, 0.18)
  )
  
  target_values <- c(age = 65)
  
  # Test the main function
  expect_message(
    result <- NMI_interpolation_continuous(
      IPD = IPD,
      AgD = AgD,
      x_vect = target_values,
      AgD_EM_cols = "age_mean",
      IPD_EM_cols = "age"
    ),
    "Auto-detected EM types"
  )
  
  expect_type(result, "list")
  expect_named(result, c("Final", "method", "extrapolation", "em_types", "continuous_ems", "target_values"))
  expect_equal(result$method, "linear")
  expect_equal(result$continuous_ems, "age")
  expect_equal(result$target_values, target_values)
  
  # Check Final data frame structure
  expect_s3_class(result$Final, "data.frame")
  expect_true("TE" %in% names(result$Final))
  expect_true("se" %in% names(result$Final))
  expect_true("age" %in% names(result$Final))
})

test_that("NMI_interpolation_continuous errors appropriately", {
  
  # Test data
  IPD <- data.frame(
    age = c(50, 60, 70),
    sex = c(0, 1, 1),
    Tr = c(0, 1, 0),
    Y = c(0, 1, 1)
  )
  
  AgD <- data.frame(
    age_mean = c(55, 65),
    TE = c(0.2, 0.4),
    se = c(0.1, 0.12)
  )
  
  # Test multiple continuous EMs error
  expect_error(
    NMI_interpolation_continuous(
      IPD = IPD,
      AgD = AgD,
      x_vect = c(age = 60, another_continuous = 30),
      AgD_EM_cols = c("age_mean", "another_continuous"),
      IPD_EM_cols = c("age", "another_continuous"),
      em_types = c(age = "continuous", another_continuous = "continuous")
    ),
    "Multiple continuous EMs not yet supported"
  )
  
  # Test mixed EM types error
  expect_error(
    NMI_interpolation_continuous(
      IPD = IPD,
      AgD = AgD,
      x_vect = c(age = 60, sex = 1),
      AgD_EM_cols = c("age_mean", "sex"),
      IPD_EM_cols = c("age", "sex"),
      em_types = c(age = "continuous", sex = "binary")
    ),
    "Mixed binary \\+ continuous EMs not yet supported"
  )
}) 