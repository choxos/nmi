test_that("cmdstanr NMA run works", {
  skip_if_not_installed("cmdstanr")
  
  # Create minimal test data
  test_data <- data.frame(
    Study = 1:3,
    Trt1 = c("A", "A", "A"),
    Trt2 = c("B", "C", "D"),
    TE = c(0.5, 0.3, 0.7),
    se = c(0.2, 0.15, 0.25)
  )
  
  # Test minimal MCMC settings for quick test
  result <- expect_no_error(
    NMA_run(
      dat = test_data,
      N_chains = 1,
      N_iter = 100,
      burnin = 50,
      outcome_type = "continuous"
    )
  )
  
  # Check that result has expected structure
  expect_true(is.list(result))
  expect_true("BUGSoutput" %in% names(result))
  expect_true("summary" %in% names(result$BUGSoutput))
})

test_that("nmi_full_analysis works with minimal data", {
  skip_if_not_installed("cmdstanr")
  
  # Create minimal test datasets
  ipd_test <- data.frame(
    treatment = c("A", "A", "D", "D"),
    X1 = c(1, 0, 1, 0),
    X2 = c(1, 1, 0, 0),
    outcome = c(1, 0, 1, 0)
  )
  
  agd_test <- data.frame(
    Study = c(1, 1, 1),
    Trt1 = c("A", "A", "A"),
    Trt2 = c("B", "B", "B"),
    X1 = c(0.5, 1, 0),
    X2 = c(0.5, 0.5, 0.5),
    TE = c(0.5, 0.3, 0.7),
    SE = c(0.2, 0.15, 0.25)
  )
  
  # Test with minimal MCMC settings
  result <- expect_no_error(
    nmi_full_analysis(
      IPD = ipd_test,
      AgD = agd_test,
      x_vect = c(0.5, 0.5),
      AgD_EM_cols = c("X1", "X2"),
      IPD_EM_cols = c("X1", "X2"),
      IPD_treatment_col = "treatment",
      AgD_treatment_cols = c("Trt1", "Trt2"),
      IPD_outcome_col = "outcome",
      AgD_TE_col = "TE",
      AgD_SE_col = "SE",
      AgD_study_col = "Study",
      study_sample_sizes = c(100),
      outcome_type = "binary",
      mcmc_settings = list(n_iter = 100, n_warmup = 50, n_chains = 1)
    )
  )
  
  # Check result structure
  expect_true(inherits(result, "nmi_analysis"))
  expect_true(is.list(result))
  expect_true("nmi_interpolation" %in% names(result))
  expect_true("nma_results" %in% names(result))
  expect_true("settings" %in% names(result))
})

test_that("HTML report generation works", {
  skip_if_not_installed("cmdstanr")
  
  # Create mock nmi_analysis object
  mock_result <- list(
    nmi_interpolation = list(Final = data.frame(Study = 1, Trt1 = "A", Trt2 = "B", TE = 0.5, se = 0.2)),
    nma_results = list(BUGSoutput = list(summary = matrix(c(0.5, 0.1, 0.3, 0.7, 1.0), nrow = 1, 
                                                          dimnames = list("d_raw[1]", c("mean", "sd", "2.5%", "97.5%", "Rhat"))))),
    settings = list(x_target = c(0.5, 0.5), outcome_type = "binary", mcmc_settings = list(n_chains = 1, n_iter = 100)),
    treatments = c("A", "B"),
    data_info = list(n_studies = 1, n_comparisons = 1, effect_modifiers = c("X1", "X2"))
  )
  class(mock_result) <- c("nmi_analysis", class(mock_result))
  
  # Create temporary directory for test
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  
  # Test HTML report generation
  report_file <- expect_no_error(
    generate_nmi_html_report(
      nmi_results = mock_result,
      study_name = "TestStudy",
      outcome = "TestOutcome",
      report_title = "Test Report"
    )
  )
  
  # Check that file was created
  expect_true(file.exists(report_file))
  expect_true(grepl("\\.html$", report_file))
  
  # Clean up
  setwd(old_wd)
  unlink(file.path(temp_dir, "reports"), recursive = TRUE)
}) 