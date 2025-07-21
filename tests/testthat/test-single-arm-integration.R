# Tests for Single-Arm Study Integration (Phase 3 - v1.3.0)
# Testing single-arm study detection and integration methods

test_that("detect_single_arm_studies identifies single-arm studies correctly", {
  # Create mixed data with both comparative and single-arm studies
  AgD_mixed <- data.frame(
    Study = paste0("Study_", 1:6),
    Trt1 = c("A", "B", "C", "C", "D", "E"),  # Study 4 has C vs C (single-arm)
    Trt2 = c("B", "C", "C", "C", "E", "E"),  # Study 6 has E vs E (single-arm)
    TE = c(0.5, 0.3, 0.4, 0.2, 0.6, 0.1),
    se = c(0.1, 0.12, 0.11, 0.08, 0.13, 0.09)
  )
  
  result <- detect_single_arm_studies(AgD_mixed, c("Trt1", "Trt2"), "Study")
  
  expect_equal(result$n_single_arm, 2)  # Studies 4 and 6
  expect_equal(result$n_comparative, 4)  # Studies 1, 2, 3, 5
  expect_true("C" %in% result$treatments_single_arm)
  expect_true("E" %in% result$treatments_single_arm)
  expect_true(result$integration_feasible)  # Should have overlapping treatments
})

test_that("detect_single_arm_studies handles missing treatment data", {
  # Create data with NA treatments (another form of single-arm)
  AgD_with_na <- data.frame(
    Study = paste0("Study_", 1:4),
    Trt1 = c("A", "B", "C", NA),
    Trt2 = c("B", "C", NA, "D"),
    TE = c(0.5, 0.3, 0.4, 0.2),
    se = c(0.1, 0.12, 0.11, 0.08)
  )
  
  result <- detect_single_arm_studies(AgD_with_na, c("Trt1", "Trt2"), "Study")
  
  expect_equal(result$n_single_arm, 2)  # Studies 3 and 4 have NA treatments
  expect_equal(result$n_comparative, 2)  # Studies 1 and 2
})

test_that("detect_single_arm_studies validates input parameters", {
  AgD <- data.frame(Study = "S1", Trt1 = "A", Trt2 = "B", TE = 0.5, se = 0.1)
  
  # Missing treatment columns
  expect_error(detect_single_arm_studies(AgD, c("Wrong1", "Wrong2"), "Study"))
  
  # Missing study column
  expect_error(detect_single_arm_studies(AgD, c("Trt1", "Trt2"), "WrongStudy"))
})

test_that("integrate_single_arm_studies works with reference_connect method", {
  # Create test data with single-arm studies
  AgD_mixed <- data.frame(
    Study = paste0("Study_", 1:5),
    Trt1 = c("A", "B", "Placebo", "C", "C"),  # Study 5 is single-arm
    Trt2 = c("Placebo", "Placebo", "Placebo", "Placebo", "C"),  # Study 5 is single-arm
    TE = c(0.5, 0.3, 0.0, 0.4, 0.2),
    se = c(0.1, 0.12, 0.05, 0.11, 0.08),
    age_mean = c(60, 65, 62, 68, 70)
  )
  
  # Detect single-arm studies
  single_arm_info <- detect_single_arm_studies(AgD_mixed, c("Trt1", "Trt2"), "Study")
  
  # Integrate using reference_connect method
  integration_result <- integrate_single_arm_studies(
    AgD = AgD_mixed,
    single_arm_info = single_arm_info,
    reference_treatment = "Placebo",
    integration_method = "reference_connect",
    outcome_type = "binary"
  )
  
  expect_true(integration_result$success)
  expect_true(nrow(integration_result$integrated_data) > nrow(single_arm_info$comparative_studies))
  expect_equal(integration_result$diagnostics$method, "reference_connect")
  expect_equal(integration_result$diagnostics$reference_treatment, "Placebo")
})

test_that("integrate_single_arm_studies fails gracefully when no overlap", {
  # Create data with no overlapping treatments
  AgD_no_overlap <- data.frame(
    Study = paste0("Study_", 1:3),
    Trt1 = c("A", "X", "X"),  # Study 2,3 are single-arm with treatment X
    Trt2 = c("B", "X", "X"),  # But X is not in comparative studies
    TE = c(0.5, 0.3, 0.4),
    se = c(0.1, 0.12, 0.11)
  )
  
  single_arm_info <- detect_single_arm_studies(AgD_no_overlap, c("Trt1", "Trt2"), "Study")
  
  expect_false(single_arm_info$integration_feasible)
  
  # Should fail integration
  expect_warning({
    integration_result <- integrate_single_arm_studies(
      AgD = AgD_no_overlap,
      single_arm_info = single_arm_info,
      reference_treatment = "A",  # A is not in single-arm studies
      integration_method = "reference_connect",
      outcome_type = "binary"
    )
  })
})

test_that("extract_reference_treatment_data works correctly", {
  comparative_data <- data.frame(
    Study = paste0("Study_", 1:3),
    Trt1 = c("A", "B", "Placebo"),
    Trt2 = c("Placebo", "Placebo", "C"),
    TE = c(0.5, 0.3, 0.4),
    se = c(0.1, 0.12, 0.11)
  )
  
  ref_data <- extract_reference_treatment_data(comparative_data, "Placebo", "binary")
  
  expect_true(nrow(ref_data) > 0)
  expect_equal(ref_data$Treatment[1], "Placebo")
  expect_true("n_studies" %in% colnames(ref_data))
  expect_equal(ref_data$n_studies[1], 3)  # Placebo appears in all 3 studies
})

test_that("create_pseudo_comparisons generates valid data", {
  single_arm_data <- data.frame(
    Study = "SingleArm_1",
    Treatment = "NewTreatment",
    TE = 0.6,
    se = 0.15,
    age_mean = 65
  )
  
  reference_data <- data.frame(
    Treatment = "Placebo",
    TE_vs_internal = 0,
    se_vs_internal = 0.05,
    n_studies = 3,
    source = "internal_comparative"
  )
  
  pseudo_comp <- create_pseudo_comparisons(
    single_arm_data, reference_data, "Placebo", "binary", uncertainty_factor = 1.5
  )
  
  expect_equal(nrow(pseudo_comp), 1)
  expect_equal(pseudo_comp$Trt1[1], "NewTreatment")
  expect_equal(pseudo_comp$Trt2[1], "Placebo")
  expect_true(pseudo_comp$se[1] > reference_data$se_vs_internal[1])  # Should be inflated
  expect_equal(pseudo_comp$type[1], "pseudo_comparison")
  expect_true("age_mean" %in% colnames(pseudo_comp))  # Should copy EM columns
})

test_that("nmi_with_single_arm_integration full workflow", {
  # Create comprehensive test data
  IPD <- data.frame(
    Study = rep("IPD_Study", 100),
    age = rnorm(100, 60, 10),
    treatment = sample(c("A", "Placebo"), 100, replace = TRUE),
    outcome = rbinom(100, 1, 0.4)
  )
  
  AgD_mixed <- data.frame(
    Study = paste0("Study_", 1:5),
    Trt1 = c("A", "B", "C", "C", "D"),
    Trt2 = c("Placebo", "Placebo", "Placebo", "C", "D"),  # Study 4,5 are single-arm
    age_mean = c(55, 60, 65, 70, 68),
    TE = c(0.5, 0.3, 0.4, 0.2, 0.1),
    se = c(0.1, 0.12, 0.11, 0.08, 0.09)
  )
  
  # Mock NMI_interpolation for testing
  old_nmi <- NMI_interpolation
  NMI_interpolation <<- function(...) {
    list(Final = data.frame(Study = "Test", TE = 0.5, se = 0.1))
  }
  
  result <- nmi_with_single_arm_integration(
    IPD = IPD,
    AgD = AgD_mixed,
    x_vect = c(age = 62),
    AgD_EM_cols = "age_mean",
    IPD_EM_cols = "age",
    trt_cols = c("Trt1", "Trt2"),
    study_col = "Study",
    reference_treatment = "Placebo",
    integration_method = "reference_connect",
    outcome_type = "binary"
  )
  
  expect_true("nmi_result" %in% names(result))
  expect_true("single_arm_integration" %in% names(result))
  expect_true("single_arm_detection" %in% names(result))
  expect_true(result$single_arm_detection$n_single_arm >= 1)
  
  # Restore original function
  NMI_interpolation <<- old_nmi
})

test_that("validate_single_arm_integration provides meaningful feedback", {
  # Create mock integration result
  integration_result <- list(
    success = TRUE,
    diagnostics = list(
      method = "reference_connect",
      reference_treatment = "Placebo",
      n_pseudo_comparisons = 3,
      uncertainty_factor = 1.5,
      treatments_integrated = c("NewTrt1", "NewTrt2")
    )
  )
  
  validation <- validate_single_arm_integration(integration_result)
  
  expect_true("valid" %in% names(validation))
  expect_true("issues" %in% names(validation))
  expect_true("recommendations" %in% names(validation))
  expect_true("summary" %in% names(validation))
  expect_equal(validation$summary$n_pseudo_comparisons, 3)
  expect_equal(validation$summary$uncertainty_factor, 1.5)
})

test_that("validate_single_arm_integration detects issues", {
  # Create problematic integration result
  integration_result <- list(
    success = TRUE,
    diagnostics = list(
      method = "reference_connect",
      reference_treatment = "Placebo",
      n_pseudo_comparisons = 0,  # Problem: no pseudo-comparisons
      uncertainty_factor = 1.1,  # Problem: low uncertainty factor
      treatments_integrated = character(0)
    )
  )
  
  validation <- validate_single_arm_integration(integration_result)
  
  expect_false(validation$valid)
  expect_true(any(grepl("No pseudo-comparisons", validation$issues)))
  expect_true(any(grepl("Low uncertainty factor", validation$issues)))
})

test_that("single-arm integration handles edge cases", {
  # Empty data
  AgD_empty <- data.frame(
    Study = character(0), Trt1 = character(0), Trt2 = character(0),
    TE = numeric(0), se = numeric(0)
  )
  
  result_empty <- detect_single_arm_studies(AgD_empty, c("Trt1", "Trt2"), "Study")
  expect_equal(result_empty$n_single_arm, 0)
  expect_equal(result_empty$n_comparative, 0)
  
  # All single-arm studies
  AgD_all_single <- data.frame(
    Study = paste0("Study_", 1:3),
    Trt1 = c("A", "B", "C"),
    Trt2 = c("A", "B", "C"),  # All same treatment = single-arm
    TE = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07)
  )
  
  result_all_single <- detect_single_arm_studies(AgD_all_single, c("Trt1", "Trt2"), "Study")
  expect_equal(result_all_single$n_single_arm, 3)
  expect_equal(result_all_single$n_comparative, 0)
  expect_false(result_all_single$integration_feasible)  # No comparative studies to integrate with
})

test_that("integration methods handle fallback correctly", {
  AgD_mixed <- data.frame(
    Study = paste0("Study_", 1:3),
    Trt1 = c("A", "B", "B"),
    Trt2 = c("Placebo", "Placebo", "B"),  # Study 3 is single-arm
    TE = c(0.5, 0.3, 0.2),
    se = c(0.1, 0.12, 0.08)
  )
  
  single_arm_info <- detect_single_arm_studies(AgD_mixed, c("Trt1", "Trt2"), "Study")
  
  # Test outcome_model method (should fall back to reference_connect)
  expect_message({
    result_outcome <- integrate_single_arm_studies(
      AgD = AgD_mixed,
      single_arm_info = single_arm_info,
      reference_treatment = "Placebo",
      integration_method = "outcome_model",
      outcome_type = "binary"
    )
  }, "Falling back to reference connection")
  
  expect_true(result_outcome$success)
  
  # Test network_bridge method (should fall back to reference_connect)
  expect_message({
    result_bridge <- integrate_single_arm_studies(
      AgD = AgD_mixed,
      single_arm_info = single_arm_info,
      reference_treatment = "Placebo",
      integration_method = "network_bridge",
      outcome_type = "binary"
    )
  }, "Falling back to reference connection")
  
  expect_true(result_bridge$success)
}) 