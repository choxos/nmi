# Basic NMI Example
# This script demonstrates the basic usage of the NMI package

# Load the package
library(nmi)

# Load example data
IPD <- load_example_ipd()
AgD <- load_example_agd()

# Examine the data
print("IPD structure:")
print(str(IPD))
print(head(IPD))

print("AgD structure:")
print(str(AgD))
print(head(AgD))

# Define analysis parameters
x_vect <- c(0.675, 0.475)  # Effect modifier levels for comparison
AgD_EM_cols <- c('x1', 'x2')
IPD_EM_cols <- c('x1', 'x2')
Study_col <- 'Study'
samp_sizes <- rep(600, 6)  # Sample sizes for AgD studies
AgD_Trt_cols <- c('Trt1', 'Trt2')
TE_col <- 'TE'
SE_col <- 'se'
IPD_Trt_col <- 'Tr'
outcome_col <- 'Y'

# Perform NMI interpolation
print("Performing NMI interpolation...")
NMI_result <- NMI_interpolation(
  IPD = IPD,
  AgD = AgD,
  x_vect = x_vect,
  AgD_EM_cols = AgD_EM_cols,
  IPD_EM_cols = IPD_EM_cols,
  Study_col = Study_col,
  samp_sizes = samp_sizes,
  AgD_Trt_cols = AgD_Trt_cols,
  TE_col = TE_col,
  SE_col = SE_col,
  IPD_Trt_col = IPD_Trt_col,
  outcome_col = outcome_col,
  outcome_type = "binary"
)

print("NMI Results:")
print(NMI_result$Final)

# Create diagnostic plot
print("Creating diagnostic plot...")
diagnostic_plot <- NMI_diagnostic_plot(NMI_result)
print(diagnostic_plot)

# Run Network Meta-Analysis
print("Running NMA...")
NMI_fit <- NMA_run(
  dat = NMI_result$Final,
  N_chains = 2,  # Reduced for faster example
  N_iter = 1000,  # Reduced for faster example
  burnin = 300,   # Reduced for faster example
  outcome_type = "binary"
)

# Summarize results
print("Summarizing NMA results...")
NMI_summary <- NMA_NMI_summary(NMI_fit)
print(NMI_summary)

# Create results table
results_table <- result_table(NMI_summary)
print("Results Table:")
print(results_table)

print("Example completed successfully!")