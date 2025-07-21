# Network Meta-Interpolation (NMI) Package

[![R-CMD-check](https://github.com/username/nmi/workflows/R-CMD-check/badge.svg)](https://github.com/username/nmi/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

An R package implementing Network Meta-Interpolation (NMI) methodology for addressing effect modification in network meta-analysis using subgroup analyses.

## Overview

Network Meta-Interpolation (NMI) is a method for indirect treatment comparison that addresses effect modification when combining individual patient data (IPD) and aggregate data (AgD). The methodology was developed by Harari et al. (2023) and allows researchers to:

- Perform network meta-analysis at specific covariate levels
- Handle effect modification in mixed IPD/AgD networks
- Support multiple outcome types (binary, continuous, count)
- Use Bayesian inference with Stan for robust statistical analysis

## Features

- **Flexible outcome types**: Binary, continuous, and count outcomes
- **Effect modification handling**: Addresses differences in treatment effects across patient subgroups
- **BLUP imputation**: Uses Best Linear Unbiased Predictor for missing covariate values
- **Multiple analysis methods**: NMA, NMR, ML-NMR, and NMI
- **Modern Bayesian framework**: cmdstanr integration for fast, efficient Stan sampling
- **Professional reporting**: Automated HTML report generation with organized file structure
- **Comprehensive visualization**: Diagnostic plots, forest plots, and interactive tables
- **Convergence diagnostics**: Detailed MCMC diagnostics with R-hat and effective sample size

## Installation

```r
# Install from source
devtools::install_local("path/to/nmi")

# Load the package
library(nmi)
```

### Prerequisites

The package uses **cmdstanr** for Bayesian inference. Make sure you have it installed:

```r
# Install cmdstanr (if not already installed)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Install CmdStan (if not already installed)
library(cmdstanr)
install_cmdstan()
```

## New Features (v1.0.0)

### Enhanced Bayesian Analysis with cmdstanr

- **Faster sampling**: cmdstanr provides improved performance over rstan
- **Better diagnostics**: Enhanced convergence diagnostics and warnings
- **Parallel processing**: Automatic parallel chain execution
- **Memory efficiency**: Reduced memory usage for large models

### Professional HTML Reporting

- **Automated report generation**: One-function HTML report creation
- **Organized file structure**: Reports saved with timestamps in structured directories
- **Interactive elements**: Dynamic tables and navigation
- **Professional styling**: Publication-ready reports with modern design

## Quick Start

### Interactive Shiny App (Recommended for Beginners)

```r
library(nmi)

# Launch the interactive web application
launch_nmi_app()
```

The Shiny app provides a user-friendly interface with:
- Example datasets for learning
- Step-by-step analysis workflow
- Interactive visualizations

### Complete NMI Analysis with HTML Reporting

For programmatic analysis with professional reporting:

```r
library(nmi)

# Load your data (IPD and AgD)
# ipd_data <- your_individual_patient_data
# agd_data <- your_aggregate_data

# Run complete NMI analysis with cmdstanr
nmi_results <- nmi_full_analysis(
  IPD = ipd_data,
  AgD = agd_data,
  x_vect = c(0.6, 0.4),  # Target effect modifier values
  AgD_EM_cols = c("X1", "X2"),
  IPD_EM_cols = c("X1", "X2"),
  IPD_treatment_col = "treatment",
  AgD_treatment_cols = c("Trt1", "Trt2"),
  IPD_outcome_col = "outcome",
  AgD_TE_col = "TE",
  AgD_SE_col = "SE",
  AgD_study_col = "Study",
  study_sample_sizes = c(300, 300, 300, 300, 300, 300),
  outcome_type = "binary",
  mcmc_settings = list(n_iter = 2000, n_warmup = 1000, n_chains = 2)
)

# Generate professional HTML report
report_file <- generate_nmi_html_report(
  nmi_results = nmi_results,
  study_name = "MyStudy",
  outcome = "Primary_Efficacy",
  report_title = "NMI Analysis Report",
  author_name = "Your Name",
  organization = "Your Institution"
)

# Report saved to: reports/MyStudy_Primary_Efficacy_YYYYMMDD/MyStudy_Primary_Efficacy_YYYYMMDD_HHMMSS.html
```

### Report Organization

Reports are automatically organized with timestamps:
```
reports/
└── MyStudy_Primary_Efficacy_20250120/
    └── MyStudy_Primary_Efficacy_20250120_143022.html
```

This structure:
- Prevents file overwrites
- Organizes by study and outcome
- Maintains analysis history
- Supports multiple runs per day

### Example with Synthetic Data

A complete working example is available:

```r
# Run the cmdstanr + HTML reporting example
source(system.file("examples", "nmi_cmdstan_example.R", package = "nmi"))
```

This example demonstrates:
- Synthetic data generation for testing
- Complete NMI workflow with cmdstanr
- HTML report generation with file organization
- Comparison between NMI and standard NMA
- Convergence diagnostics and result interpretation

### Programmatic Usage

```r
library(nmi)

# Load example data
IPD <- load_example_ipd()
AgD <- load_example_agd()

# Define analysis parameters
x_vect <- c(0.675, 0.475)  # Effect modifier levels for comparison
AgD_EM_cols <- c('x1', 'x2')
IPD_EM_cols <- c('x1', 'x2')

# Perform NMI interpolation
NMI_result <- NMI_interpolation(
  IPD = IPD,
  AgD = AgD,
  x_vect = x_vect,
  AgD_EM_cols = AgD_EM_cols,
  IPD_EM_cols = IPD_EM_cols,
  Study_col = 'Study',
  samp_sizes = rep(600, 6),
  AgD_Trt_cols = c('Trt1', 'Trt2'),
  TE_col = 'TE',
  SE_col = 'se',
  IPD_Trt_col = 'Tr',
  outcome_col = 'Y',
  outcome_type = "binary"
)

# Run network meta-analysis
NMI_fit <- NMA_run(
  dat = NMI_result$Final,
  N_chains = 3,
  N_iter = 1500,
  burnin = 500,
  outcome_type = "binary"
)

# Summarize results
NMI_summary <- NMA_NMI_summary(NMI_fit)
print(NMI_summary)

# Create diagnostic plot
diagnostic_plot <- NMI_diagnostic_plot(NMI_result)
print(diagnostic_plot)
```

## Main Functions

### Core NMI Functions

- `NMI_interpolation()`: Main function for NMI interpolation
- `BLUP_impute()`: Impute missing covariate values using BLUP
- `GLMLOGIT()`: Transform IPD into subgroup analyses

### Analysis Functions

- `NMA_run()`: Run network meta-analysis using Stan
- `NMA_Meta_Reg_run_2D()`: Run network meta-regression with 2 effect modifiers
- `NMA_Meta_Reg_run_3D()`: Run network meta-regression with 3 effect modifiers
- `ML_NMR_Run_2D()`: Run multilevel network meta-regression with 2 effect modifiers
- `ML_NMR_Run_3D()`: Run multilevel network meta-regression with 3 effect modifiers

### Summary Functions

- `NMA_NMI_summary()`: Summarize NMA results
- `NMA_Metareg_summary_2D()`: Summarize NMR results (2D)
- `NMA_Metareg_summary_3D()`: Summarize NMR results (3D)
- `ML_NMR_summary_2D()`: Summarize ML-NMR results (2D)
- `ML_NMR_summary_3D()`: Summarize ML-NMR results (3D)

### Visualization Functions

- `NMI_diagnostic_plot()`: Create diagnostic plot for interpolation quality
- `NMI_diagnostic_plotly()`: Create interactive diagnostic plot
- `result_forest_plot()`: Create forest plot comparing methods
- `result_table()`: Format results table
- `display_result_table()`: Display formatted results table

### Shiny Application

- `launch_nmi_app()`: Launch the interactive Shiny application

## Data Requirements

### Individual Patient Data (IPD)

The IPD should contain:
- Treatment assignment column
- Outcome column
- Effect modifier columns
- Study identifier

### Aggregate Data (AgD)

The AgD should contain:
- Treatment comparison columns
- Effect modifier summary statistics
- Treatment effect estimates
- Standard errors
- Study identifier

## Outcome Types

The package supports three outcome types:

1. **Binary outcomes** (`outcome_type = "binary"`)
   - Uses logistic regression
   - Requires binary outcome variable (0/1)

2. **Continuous outcomes** (`outcome_type = "continuous"`)
   - Uses linear regression
   - Requires continuous outcome variable

3. **Count outcomes** (`outcome_type = "count"`)
   - Uses Poisson regression
   - Requires count outcome variable

## Stan Models

The package uses Stan for Bayesian inference. All models are built-in and automatically compiled when needed. The Stan models support:

- Fixed effects network meta-analysis
- Network meta-regression with multiple covariates
- Flexible prior specifications
- Efficient sampling algorithms

## Examples

See the package vignette for detailed examples:

```r
vignette("nmi_introduction", package = "nmi")
```

## Dependencies

- **R (>= 3.5.0)**
- **Stan ecosystem**: rstan, multinma
- **Data manipulation**: dplyr, tibble, tidyr
- **Visualization**: ggplot2, plotly, lemon
- **Tables**: kableExtra, reshape2
- **Statistics**: broom, broom.mixed

## References

Harari, O., Abrams, K. R., Ades, A. E., Sutton, A. J., & Cooper, N. J. (2023). Network meta‐interpolation: Effect modification adjustment in network meta‐analysis using subgroup analyses. *Journal of the Royal Statistical Society: Series A*, 186(2), 643-670.

## License

This package is licensed under the MIT License. See the LICENSE file for details.

## Contributing

We welcome contributions! Please see our contributing guidelines and code of conduct.

## Getting Help

- Check the function documentation with `?function_name`
- Browse the package vignette for examples
- Report issues on the GitHub repository

## Citation

If you use this package in your research, please cite:

```
@Manual{nmi,
  title = {nmi: Network Meta-Interpolation for Indirect Treatment Comparison},
  author = {NMI Developer},
  year = {2025},
  note = {R package version 1.0.0},
}
```

And the original methodology paper:

```
@article{harari2023network,
  title={Network meta-interpolation: Effect modification adjustment in network meta-analysis using subgroup analyses},
  author={Harari, Orit and Abrams, Keith R and Ades, AE and Sutton, Alex J and Cooper, Nicola J},
  journal={Journal of the Royal Statistical Society: Series A},
  volume={186},
  number={2},
  pages={643--670},
  year={2023},
  publisher={Oxford University Press}
}
```