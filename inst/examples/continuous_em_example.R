# Example: Continuous Effect Modifiers in NMI
# Author: Ahmad Sofi-Mahmudi
# Date: 2025-01-21

# Installation (if not already installed):
# devtools::install_github("choxos/nmi", build_vignettes = TRUE)

library(nmi)

# Example: Age as a Continuous Effect Modifier
# ============================================

# This example demonstrates how to use continuous effect modifiers (age)
# in Network Meta-Interpolation analysis

cat("=== Continuous Effect Modifiers Example ===\n")

# 1. Create Example Data with Age as Continuous EM
# ------------------------------------------------

set.seed(2025)

# Individual Patient Data (IPD) with continuous age
IPD_continuous <- data.frame(
  Study = rep("IPD_Study", 200),
  age = rnorm(200, mean = 60, sd = 12),  # Continuous age
  sex = rbinom(200, 1, 0.6),             # Binary covariate  
  treatment = sample(c("A", "B"), 200, replace = TRUE),
  outcome = rbinom(200, 1, 0.4)
)

# Add treatment effect modification by age
# Older patients respond better to treatment B
age_effect <- (IPD_continuous$age - 60) / 10 * 0.1
treatment_effect <- ifelse(IPD_continuous$treatment == "B", 0.2 + age_effect, 0)
outcome_prob <- plogis(-0.5 + treatment_effect)
IPD_continuous$outcome <- rbinom(200, 1, outcome_prob)

cat("IPD Data created with", nrow(IPD_continuous), "patients\n")
cat("Age range:", round(range(IPD_continuous$age), 1), "\n")

# Aggregate Data (AgD) with age summaries from different studies
AgD_continuous <- data.frame(
  Study = paste0("Study_", 1:6),
  age_mean = c(45, 55, 60, 65, 70, 75),     # Different age profiles
  age_sd = c(8, 10, 12, 10, 8, 6),
  sample_size = c(120, 150, 180, 160, 140, 100),
  TE = c(-0.1, 0.1, 0.3, 0.5, 0.6, 0.4),   # Treatment effects vary with age
  se = c(0.15, 0.12, 0.10, 0.11, 0.13, 0.16)
)

cat("AgD Data created with", nrow(AgD_continuous), "studies\n")
cat("Age range across studies:", range(AgD_continuous$age_mean), "\n")

# 2. Automatic EM Type Detection
# ------------------------------

cat("\n=== Automatic EM Type Detection ===\n")

# Detect types of effect modifiers
em_types <- detect_em_types(IPD_continuous, c("age", "sex"))
print(em_types)

# 3. Linear Interpolation Example
# -------------------------------

cat("\n=== Linear Interpolation Example ===\n")

# Target age for interpolation
target_age <- 62.5

# Perform linear interpolation
linear_result <- linear_interpolation(
  study_data = AgD_continuous,
  target_values = c(age_mean = target_age),
  em_cols = "age_mean",
  te_col = "TE",
  se_col = "se"
)

cat("Linear interpolation at age", target_age, ":\n")
cat("  Treatment Effect:", round(linear_result$te, 3), "\n")
cat("  Standard Error:", round(linear_result$se, 3), "\n")
cat("  Method:", linear_result$method, "\n")
cat("  Extrapolation:", linear_result$extrapolation, "\n")

# 4. Spline Interpolation Example
# ------------------------------

cat("\n=== Spline Interpolation Example ===\n")

# Perform spline interpolation
spline_result <- spline_interpolation(
  study_data = AgD_continuous,
  target_values = c(age_mean = target_age),
  em_cols = "age_mean",
  spline_type = "natural"
)

cat("Spline interpolation at age", target_age, ":\n")
cat("  Treatment Effect:", round(spline_result$te, 3), "\n")
cat("  Standard Error:", round(spline_result$se, 3), "\n")
cat("  Method:", spline_result$method, "\n")

# Compare linear vs spline
cat("\nComparison:\n")
cat("  Linear TE:", round(linear_result$te, 3), "\n")
cat("  Spline TE:", round(spline_result$te, 3), "\n")
cat("  Difference:", round(abs(linear_result$te - spline_result$te), 3), "\n")

# 5. Adaptive Discretization Example
# ----------------------------------

cat("\n=== Adaptive Discretization Example ===\n")

# Demonstrate optimal discretization of age
age_data <- IPD_continuous$age

discretization_result <- optimize_discretization(
  continuous_data = age_data,
  method = "equal_freq",
  max_bins = 5
)

cat("Optimal discretization results:\n")
cat("  Number of bins:", discretization_result$n_bins, "\n")
cat("  Method:", discretization_result$method, "\n")
cat("  Score:", round(discretization_result$score, 3), "\n")
cat("  Bin counts:\n")
print(discretization_result$bin_counts)

# 6. Full Continuous NMI Analysis
# -------------------------------

cat("\n=== Full Continuous NMI Analysis ===\n")

# Define target values for different ages
target_ages <- c(age = 65)

# Perform continuous NMI interpolation
nmi_continuous_result <- NMI_interpolation_continuous(
  IPD = IPD_continuous,
  AgD = AgD_continuous,
  x_vect = target_ages,
  AgD_EM_cols = "age_mean",
  IPD_EM_cols = "age",
  interpolation_method = "linear"
)

cat("Continuous NMI results:\n")
print(nmi_continuous_result$Final)

cat("\nAnalysis summary:\n")
cat("  Method:", nmi_continuous_result$method, "\n")
cat("  Continuous EMs:", nmi_continuous_result$continuous_ems, "\n")
cat("  Target age:", nmi_continuous_result$target_values, "\n")
cat("  Extrapolation warning:", nmi_continuous_result$extrapolation, "\n")

# 7. Visualization of Age-TreatmentEffect Relationship
# -----------------------------------------------------

cat("\n=== Age-Treatment Effect Visualization ===\n")

if (require(ggplot2, quietly = TRUE)) {
  
  # Create visualization data
  age_seq <- seq(40, 80, by = 5)
  interpolated_effects <- sapply(age_seq, function(age) {
    result <- linear_interpolation(
      AgD_continuous, 
      c(age_mean = age), 
      "age_mean"
    )
    result$te
  })
  
  plot_data <- data.frame(
    age = age_seq,
    treatment_effect = interpolated_effects
  )
  
  # Add observed data points
  observed_data <- data.frame(
    age = AgD_continuous$age_mean,
    treatment_effect = AgD_continuous$TE
  )
  
  p <- ggplot() +
    geom_line(data = plot_data, aes(x = age, y = treatment_effect), 
              color = "blue", size = 1) +
    geom_point(data = observed_data, aes(x = age, y = treatment_effect), 
               color = "red", size = 3) +
    geom_vline(xintercept = target_ages["age"], linetype = "dashed", color = "green") +
    labs(
      title = "Age-Treatment Effect Relationship",
      subtitle = "Blue line: Linear interpolation, Red points: Observed data, Green line: Target age",
      x = "Age (years)",
      y = "Treatment Effect"
    ) +
    theme_minimal()
  
  print(p)
  
  cat("Visualization created showing:\n")
  cat("  - Blue line: Interpolated treatment effects across age\n")
  cat("  - Red points: Observed study data\n") 
  cat("  - Green line: Target age for interpolation\n")
  
} else {
  cat("ggplot2 not available for visualization\n")
}

# 8. Performance Comparison
# ------------------------

cat("\n=== Performance Comparison ===\n")

# Compare interpolation methods across different target ages
comparison_ages <- c(50, 55, 60, 65, 70)

comparison_results <- data.frame(
  age = comparison_ages,
  linear_te = NA,
  spline_te = NA,
  linear_se = NA,
  spline_se = NA
)

for (i in seq_along(comparison_ages)) {
  age <- comparison_ages[i]
  
  linear_res <- linear_interpolation(AgD_continuous, c(age_mean = age), "age_mean")
  spline_res <- spline_interpolation(AgD_continuous, c(age_mean = age), "age_mean")
  
  comparison_results$linear_te[i] <- linear_res$te
  comparison_results$linear_se[i] <- linear_res$se
  comparison_results$spline_te[i] <- spline_res$te
  comparison_results$spline_se[i] <- spline_res$se
}

cat("Method comparison across ages:\n")
print(round(comparison_results, 3))

# Summary
cat("\n=== Summary ===\n")
cat("✓ Continuous effect modifier support implemented\n")
cat("✓ Linear and spline interpolation available\n") 
cat("✓ Automatic EM type detection working\n")
cat("✓ Adaptive discretization functional\n")
cat("✓ Integration with existing NMI framework\n")
cat("\nNext steps: Mixed EM types, multiple continuous EMs, advanced imputation\n") 