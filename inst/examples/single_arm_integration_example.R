# Example: Single-Arm Study Integration in NMI
# Author: Ahmad Sofi-Mahmudi
# Date: 2025-01-21
# Part of NMI v1.3.0 Network Extensions

# Installation (if not already installed):
# devtools::install_github("choxos/nmi", build_vignettes = TRUE)

library(nmi)

cat("=== Single-Arm Study Integration Example ===\n")

# Example: Mixed Evidence Network
# ===============================

# Scenario: We have a network of diabetes treatments with:
# - Comparative RCTs (head-to-head trials)
# - Single-arm studies (often from early development or rare settings)
# - Need to integrate both types for comprehensive NMA

set.seed(2025)

# 1. Create Example Data with Mixed Study Types
# ---------------------------------------------

# Individual Patient Data (from a large pragmatic trial)
IPD_diabetes <- data.frame(
  Study = rep("PRAGMATIC_TRIAL", 400),
  age = rnorm(400, mean = 62, sd = 15),
  hba1c_baseline = rnorm(400, mean = 8.2, sd = 1.5),
  treatment = sample(c("Metformin", "Placebo"), 400, replace = TRUE),
  outcome_hba1c_reduction = rnorm(400, mean = 1.2, sd = 0.8)
)

# Add realistic treatment effects
for (i in 1:nrow(IPD_diabetes)) {
  baseline_hba1c <- IPD_diabetes$hba1c_baseline[i]
  age_effect <- (IPD_diabetes$age[i] - 62) / 10 * 0.1
  
  if (IPD_diabetes$treatment[i] == "Metformin") {
    # Metformin more effective in younger patients with higher baseline HbA1c
    treatment_effect <- 0.8 + (baseline_hba1c - 8.2) * 0.2 - age_effect
  } else {
    treatment_effect <- 0.1  # Placebo effect
  }
  
  IPD_diabetes$outcome_hba1c_reduction[i] <- rnorm(1, treatment_effect, 0.6)
}

cat("IPD created with", nrow(IPD_diabetes), "patients\n")

# Aggregate Data: Mix of Comparative RCTs and Single-Arm Studies
AgD_mixed <- data.frame(
  Study = c(
    # Comparative RCTs
    "RCT_001", "RCT_002", "RCT_003", "RCT_004", "RCT_005",
    # Single-arm studies
    "SINGLE_001", "SINGLE_002", "SINGLE_003", "SINGLE_004"
  ),
  Trt1 = c(
    # Comparative studies
    "Metformin", "Insulin", "GLP1_agonist", "SGLT2_inhibitor", "Metformin",
    # Single-arm studies (new treatments)
    "Novel_Drug_A", "Novel_Drug_B", "Combination_X", "Novel_Drug_A"
  ),
  Trt2 = c(
    # Comparative studies
    "Placebo", "Placebo", "Placebo", "Placebo", "Insulin",
    # Single-arm studies (same treatment = single-arm)
    "Novel_Drug_A", "Novel_Drug_B", "Combination_X", "Novel_Drug_A"
  ),
  age_mean = c(58, 65, 60, 70, 68, 45, 52, 48, 50),
  age_sd = c(12, 15, 10, 14, 13, 8, 10, 9, 11),
  hba1c_baseline_mean = c(8.5, 8.8, 8.2, 9.1, 8.6, 8.0, 7.8, 8.3, 8.1),
  sample_size = c(300, 250, 400, 180, 220, 80, 95, 60, 75),
  # Treatment effects (HbA1c reduction)
  TE = c(0.9, 1.2, 1.4, 1.1, 0.3, 1.8, 2.1, 1.6, 1.9),
  se = c(0.15, 0.18, 0.12, 0.20, 0.16, 0.25, 0.28, 0.32, 0.30)
)

cat("AgD created with", nrow(AgD_mixed), "studies\n")
cat("Includes", sum(AgD_mixed$Trt1 == AgD_mixed$Trt2), "single-arm studies\n")

# 2. Detect and Analyze Single-Arm Studies
# -----------------------------------------

cat("\n=== SINGLE-ARM STUDY DETECTION ===\n")

single_arm_detection <- detect_single_arm_studies(
  AgD = AgD_mixed,
  trt_cols = c("Trt1", "Trt2"),
  study_col = "Study"
)

cat("Detection Results:\n")
cat("  Comparative studies:", single_arm_detection$n_comparative, "\n")
cat("  Single-arm studies:", single_arm_detection$n_single_arm, "\n")
cat("  Integration feasible:", single_arm_detection$integration_feasible, "\n")

cat("\nTreatments in comparative studies:\n")
cat(" ", paste(single_arm_detection$treatments_comparative, collapse = ", "), "\n")

cat("\nTreatments in single-arm studies:\n")
cat(" ", paste(single_arm_detection$treatments_single_arm, collapse = ", "), "\n")

cat("\nOverlapping treatments (enable integration):\n")
if (length(single_arm_detection$overlapping_treatments) > 0) {
  cat(" ", paste(single_arm_detection$overlapping_treatments, collapse = ", "), "\n")
} else {
  cat("  None - integration not possible\n")
}

cat("\nNovel treatments from single-arm studies:\n")
if (length(single_arm_detection$unique_single_arm_treatments) > 0) {
  cat(" ", paste(single_arm_detection$unique_single_arm_treatments, collapse = ", "), "\n")
} else {
  cat("  None\n")
}

# 3. Integration Strategy Analysis
# --------------------------------

cat("\n=== INTEGRATION STRATEGY SELECTION ===\n")

# Check if we have a good reference treatment
potential_references <- single_arm_detection$overlapping_treatments
cat("Potential reference treatments:", paste(potential_references, collapse = ", "), "\n")

# Choose the reference treatment (usually placebo/control)
if ("Placebo" %in% potential_references) {
  reference_treatment <- "Placebo"
  cat("Selected reference treatment: Placebo (ideal choice)\n")
} else if (length(potential_references) > 0) {
  reference_treatment <- potential_references[1]
  cat("Selected reference treatment:", reference_treatment, "(best available)\n")
} else {
  cat("⚠️  No suitable reference treatment found - integration not possible\n")
  reference_treatment <- NULL
}

# 4. Perform Single-Arm Integration
# ---------------------------------

if (!is.null(reference_treatment) && single_arm_detection$integration_feasible) {
  cat("\n=== SINGLE-ARM INTEGRATION ===\n")
  
  integration_result <- integrate_single_arm_studies(
    AgD = AgD_mixed,
    single_arm_info = single_arm_detection,
    reference_treatment = reference_treatment,
    integration_method = "reference_connect",
    outcome_type = "continuous",
    uncertainty_factor = 1.8  # Higher uncertainty for novel treatments
  )
  
  if (integration_result$success) {
    cat("✅ Integration successful!\n")
    cat("Integration Details:\n")
    cat("  Method:", integration_result$diagnostics$method, "\n")
    cat("  Reference treatment:", integration_result$diagnostics$reference_treatment, "\n")
    cat("  Pseudo-comparisons created:", integration_result$diagnostics$n_pseudo_comparisons, "\n")
    cat("  Uncertainty factor:", integration_result$diagnostics$uncertainty_factor, "\n")
    
    cat("\nTreatments integrated:\n")
    for (trt in integration_result$diagnostics$treatments_integrated) {
      cat("  •", trt, "\n")
    }
    
    cat("\nData summary:\n")
    cat("  Original comparative studies:", nrow(single_arm_detection$comparative_studies), "\n")
    cat("  Original single-arm studies:", nrow(single_arm_detection$single_arm_studies), "\n")
    cat("  Final integrated dataset:", nrow(integration_result$integrated_data), "\n")
    
  } else {
    cat("❌ Integration failed\n")
    integration_result <- NULL
  }
} else {
  cat("\n❌ Skipping integration - not feasible\n")
  integration_result <- NULL
}

# 5. Run Complete NMI Analysis with Integration
# ---------------------------------------------

cat("\n=== COMPLETE NMI ANALYSIS ===\n")

target_age <- 62
target_hba1c <- 8.5
x_vect <- c(age = target_age, hba1c_baseline = target_hba1c)

# Run NMI with automatic single-arm integration
nmi_results <- nmi_with_single_arm_integration(
  IPD = IPD_diabetes,
  AgD = AgD_mixed,
  x_vect = x_vect,
  AgD_EM_cols = c("age_mean", "hba1c_baseline_mean"),
  IPD_EM_cols = c("age", "hba1c_baseline"),
  trt_cols = c("Trt1", "Trt2"),
  study_col = "Study",
  reference_treatment = if (!is.null(reference_treatment)) reference_treatment else "Placebo",
  integration_method = "reference_connect",
  outcome_type = "continuous",
  auto_detect_single_arm = TRUE
)

cat("Analysis Method:", nmi_results$method, "\n")

if (nmi_results$single_arm_integration$used) {
  cat("✅ Single-arm integration was used\n")
  cat("Network now includes treatments from single-arm studies\n")
} else {
  cat("ℹ️  Single-arm integration was not used\n")
  cat("Reason:", nmi_results$single_arm_integration$reason, "\n")
}

cat("Analysis completed for target population:\n")
cat("  Age:", target_age, "years\n")
cat("  Baseline HbA1c:", target_hba1c, "%\n")

# 6. Validate Integration Quality
# -------------------------------

if (!is.null(integration_result) && integration_result$success) {
  cat("\n=== INTEGRATION VALIDATION ===\n")
  
  validation <- validate_single_arm_integration(integration_result)
  
  cat("Validation Status:", ifelse(validation$valid, "✅ PASSED", "⚠️  ISSUES DETECTED"), "\n")
  
  if (!validation$valid) {
    cat("\nIssues identified:\n")
    for (issue in validation$issues) {
      cat("  •", issue, "\n")
    }
    
    cat("\nRecommendations:\n")
    for (rec in validation$recommendations) {
      cat("  •", rec, "\n")
    }
  }
  
  cat("\nIntegration Summary:\n")
  cat("  Pseudo-comparisons:", validation$summary$n_pseudo_comparisons, "\n")
  cat("  New treatments added:", validation$summary$n_treatments_added, "\n")
  cat("  Uncertainty inflation:", validation$summary$uncertainty_factor, "x\n")
}

# 7. Network Connectivity Analysis
# --------------------------------

cat("\n=== NETWORK CONNECTIVITY ANALYSIS ===\n")

# Analyze connectivity of the integrated network
if (!is.null(integration_result) && integration_result$success) {
  final_data <- integration_result$integrated_data
  data_description <- "integrated network (with single-arm studies)"
} else {
  final_data <- single_arm_detection$comparative_studies
  data_description <- "comparative studies only"
}

connectivity <- detect_network_connectivity(
  final_data, 
  trt_cols = c("Trt1", "Trt2"), 
  study_col = "Study"
)

cat("Final network connectivity (", data_description, "):\n", sep = "")
cat("  Connected:", connectivity$is_connected, "\n")
cat("  Components:", connectivity$n_components, "\n")
cat("  Treatments:", length(connectivity$treatments), "\n")
cat("  Connectivity ratio:", round(connectivity$connectivity_ratio, 3), "\n")

if (!connectivity$is_connected) {
  cat("\nNetwork components:\n")
  for (i in seq_along(connectivity$components)) {
    cat("  Component", i, ":", paste(connectivity$components[[i]], collapse = ", "), "\n")
  }
}

# 8. Clinical Interpretation and Recommendations
# ----------------------------------------------

cat("\n=== CLINICAL INTERPRETATION ===\n")

generate_clinical_recommendations <- function(detection, integration, connectivity) {
  cat("Clinical Insights:\n\n")
  
  # Network composition
  if (detection$n_single_arm > 0) {
    cat("🔬 EVIDENCE SYNTHESIS:\n")
    cat("  • Network includes", detection$n_comparative, "comparative RCTs\n")
    cat("  • Plus", detection$n_single_arm, "single-arm studies\n")
    cat("  • Single-arm studies contribute", length(detection$unique_single_arm_treatments), "novel treatments\n\n")
  }
  
  # Integration success
  if (!is.null(integration) && integration$success) {
    cat("🔗 INTEGRATION SUCCESS:\n")
    cat("  • Successfully integrated single-arm evidence\n")
    cat("  • Created", integration$diagnostics$n_pseudo_comparisons, "pseudo-comparisons\n")
    cat("  • Expanded network to include novel treatments\n\n")
  } else if (detection$n_single_arm > 0) {
    cat("⚠️  INTEGRATION LIMITATIONS:\n")
    cat("  • Single-arm studies could not be integrated\n")
    cat("  • Analysis limited to comparative studies only\n")
    cat("  • Novel treatments not included in network estimates\n\n")
  }
  
  # Connectivity assessment
  if (connectivity$is_connected) {
    cat("🌐 NETWORK COMPLETENESS:\n")
    cat("  • Fully connected network enables all treatment comparisons\n")
    cat("  • Reliable indirect comparison estimates available\n\n")
  } else {
    cat("🔍 NETWORK GAPS:\n")
    cat("  • Disconnected network (", connectivity$n_components, " components)\n")
    cat("  • Some treatment comparisons not estimable\n")
    cat("  • Consider component-wise analysis\n\n")
  }
  
  # Treatment landscape
  all_treatments <- connectivity$treatments
  cat("💊 TREATMENT LANDSCAPE:\n")
  cat("  • Total treatments analyzed:", length(all_treatments), "\n")
  cat("  • Established treatments:", paste(intersect(all_treatments, c("Metformin", "Insulin", "Placebo")), collapse = ", "), "\n")
  
  novel_treatments <- intersect(all_treatments, c("Novel_Drug_A", "Novel_Drug_B", "Combination_X"))
  if (length(novel_treatments) > 0) {
    cat("  • Novel treatments included:", paste(novel_treatments, collapse = ", "), "\n")
  }
  cat("\n")
  
  # Recommendations
  cat("📋 CLINICAL RECOMMENDATIONS:\n")
  
  if (!is.null(integration) && integration$success) {
    cat("  • Interpret novel treatment estimates with caution\n")
    cat("  • Consider uncertainty around pseudo-comparisons\n")
    cat("  • Validate findings with future head-to-head trials\n")
  }
  
  if (!connectivity$is_connected) {
    cat("  • Focus analysis on largest connected component\n")
    cat("  • Consider bridging studies for disconnected treatments\n")
  }
  
  cat("  • Target population: age", x_vect["age"], "years, HbA1c", x_vect["hba1c_baseline"], "%\n")
  cat("  • Results applicable to similar patient populations\n")
}

generate_clinical_recommendations(single_arm_detection, integration_result, connectivity)

# 9. Export and Summary
# --------------------

cat("\n=== ANALYSIS SUMMARY ===\n")
cat("✅ Single-arm study detection completed\n")
cat("✅ Integration strategy evaluated\n")

if (!is.null(integration_result) && integration_result$success) {
  cat("✅ Single-arm studies successfully integrated\n")
} else {
  cat("ℹ️  Single-arm integration not performed\n")
}

cat("✅ Complete NMI analysis with single-arm integration finished\n")
cat("✅ Network connectivity assessed\n")
cat("✅ Clinical recommendations provided\n")

cat("\nKey Achievements:\n")
cat("• Expanded evidence base beyond traditional RCTs\n")
cat("• Included novel treatments from single-arm studies\n")
cat("• Maintained analytical rigor with uncertainty quantification\n")
cat("• Provided clinically relevant treatment comparisons\n")

cat("\nExample completed successfully!\n") 