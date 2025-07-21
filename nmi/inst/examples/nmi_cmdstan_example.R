# Example: Network Meta-Interpolation with cmdstanr and HTML Reporting
# This example demonstrates the updated NMI package functionality

library(nmi)

# Load example data (assuming these functions exist)
# Replace with your actual data loading or create synthetic data

# Example synthetic data for demonstration
set.seed(123)

# Load the provided example datasets
data(Example_IPD)
data(Example_AgD_NMI)

# Assign to variables used in the analysis
ipd_data <- Example_IPD
agd_data <- Example_AgD_NMI

# Define analysis parameters
target_em_values <- c(0.6, 0.4)  # Target effect modifier values
study_sample_sizes <- rep(300, 6)  # Sample sizes for each study

# Set MCMC parameters (using smaller values for quick demo)
mcmc_settings <- list(
  n_iter = 1000,
  n_warmup = 500,
  n_chains = 2,
  adapt_delta = 0.8,
  max_treedepth = 10
)

cat("Starting NMI Analysis Example with cmdstanr...\n")

# Perform complete NMI analysis
nmi_results <- nmi_full_analysis(
  IPD = ipd_data,
  AgD = agd_data,
  x_vect = target_em_values,
  AgD_EM_cols = c("x1", "x2"),
  IPD_EM_cols = c("x1", "x2"),
  IPD_treatment_col = "Tr",
  AgD_treatment_cols = c("Trt1", "Trt2"),
  IPD_outcome_col = "Y",
  AgD_TE_col = "TE",
  AgD_SE_col = "se",
  AgD_study_col = "Study",
  study_sample_sizes = study_sample_sizes,
  outcome_type = "binary",
  nma_model_type = "fixed",
  mcmc_settings = mcmc_settings
)

cat("NMI Analysis completed!\n")

# Display results summary
if (!is.null(nmi_results$nma_results$BUGSoutput)) {
  cat("\nTreatment Effects Summary:\n")
  summary_data <- nmi_results$nma_results$BUGSoutput$summary
  print(summary_data[grep("^d_raw", rownames(summary_data)), c("mean", "sd", "2.5%", "97.5%", "Rhat")])
}

# Generate HTML report
cat("\nGenerating HTML report...\n")
report_file <- generate_nmi_html_report(
  nmi_results = nmi_results,
  report_title = "NMI Analysis Example with cmdstanr",
  author_name = "Example User",
  organization = "Example Organization",
  study_name = "Demo_Study",
  outcome = "Binary_Outcome",
  include_data_tables = TRUE,
  include_plots = TRUE
)

cat("HTML report generated:", report_file, "\n")

# Run standard NMA for comparison
cat("\nRunning standard NMA for comparison...\n")
standard_nma_results <- nmi_standard_nma(
  AgD = agd_data,
  outcome_type = "binary",
  mcmc_settings = mcmc_settings,
  study_col = "Study",
  trt_cols = c("Trt1", "Trt2"),
  te_col = "TE",
  se_col = "SE"
)

cat("Standard NMA completed!\n")

# Compare results
if (!is.null(standard_nma_results$BUGSoutput)) {
  cat("\nStandard NMA Results:\n")
  std_summary <- standard_nma_results$BUGSoutput$summary
  print(std_summary[grep("^d_raw", rownames(std_summary)), c("mean", "sd", "2.5%", "97.5%", "Rhat")])
}

cat("\nExample completed successfully!\n")
cat("Key features demonstrated:\n")
cat("1. cmdstanr integration for Bayesian NMA\n")
cat("2. Complete NMI workflow with BLUP imputation and interpolation\n")
cat("3. Professional HTML report generation with organized file structure\n")
cat("4. Comparison between NMI and standard NMA approaches\n")
cat("5. Convergence diagnostics and model validation\n") 