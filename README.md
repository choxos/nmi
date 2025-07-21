# NMI Package: Network Meta-Interpolation

[![R](https://img.shields.io/badge/R-%E2%89%A5%203.5.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/Version-1.1.0-green)](https://github.com/choxos/nmi)

A comprehensive R package implementing **Network Meta-Interpolation (NMI)** methodology for addressing effect modification in network meta-analysis using subgroup analyses.

## 🆕 What's New in v1.1.0

- 🎯 **Continuous Effect Modifier Support**: Handle age, BMI, continuous biomarkers
- 📈 **Multiple Interpolation Methods**: Linear, spline, and adaptive discretization  
- 🔍 **Automatic EM Type Detection**: Smart detection of binary vs continuous variables
- ⚡ **Flexible API**: Seamless integration with existing NMI workflow
- ✅ **Comprehensive Testing**: Full validation suite with real-world examples

## Overview

The NMI package enables researchers to perform network meta-analysis while accounting for effect modification by combining:

- **Individual Patient Data (IPD)**: Rich, patient-level information
- **Aggregate Data (AgD)**: Study-level summaries from literature
- **Advanced Interpolation**: Estimate treatment effects at target covariate values

### Key Features

| Feature | Binary EMs | Continuous EMs | Status |
|---------|------------|----------------|--------|
| **Basic Interpolation** | ✅ | ✅ | Available |
| **Linear Methods** | ✅ | ✅ | Available |
| **Spline Methods** | ➖ | ✅ | New in v1.1.0 |
| **Auto-Detection** | ✅ | ✅ | New in v1.1.0 |
| **Mixed EM Types** | ✅ | 🔄 | Coming in v1.2.0 |
| **Multiple Continuous** | ➖ | 🔄 | Coming in v1.2.0 |

## Installation

### GitHub Installation (Recommended)

Install the latest version directly from GitHub:

```r
# Standard installation
devtools::install_github("choxos/nmi")

# With vignettes (recommended for full documentation)
devtools::install_github("choxos/nmi", build_vignettes = TRUE)

# Development version (latest features)
devtools::install_github("choxos/nmi", ref = "develop", build_vignettes = TRUE)
```

### Local Installation

If you have downloaded or cloned the repository locally:

```r
# Install from local directory
devtools::install_local("path/to/nmi")

# With vignettes
devtools::install_local("path/to/nmi", build_vignettes = TRUE)
```

### With Bayesian Features

For full Bayesian functionality including Stan integration:

```r
# First install cmdstanr (if not already installed)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()

# Check and fix C++ toolchain
cmdstanr::check_cmdstan_toolchain(fix = TRUE)

# Then install NMI from GitHub
devtools::install_github("choxos/nmi", build_vignettes = TRUE)
```

### Verify Installation

```r
# Load and test the package
library(nmi)

# Check version
packageVersion("nmi")

# Quick functionality test
nmi_help()

# Check package info
packageVersion("nmi")
```

### Common Compatibility Note

If you encounter a `promises >= 1.3.2` requirement error:
- The **core NMI functionality** works without promises (use `nmi_full_analysis()`, `NMI_interpolation()`)  
- The **Shiny app** (`launch_nmi_app()`) requires updated Shiny dependencies
- **Solution**: `install.packages("promises")` or use core functions only

## Quick Start

### Binary Effect Modifiers (Traditional)

```r
library(nmi)

# Load example data
IPD <- load_example_ipd()
AgD <- load_example_agd()

# Traditional binary EM analysis
result <- nmi_full_analysis(IPD, AgD)

# View results
result_table(result)
result_forest_plot(result)
```

### Continuous Effect Modifiers (New!)

```r
library(nmi)

# Create data with age as continuous EM
IPD <- data.frame(
  Study = rep("IPD_Study", 200),
  age = rnorm(200, mean = 60, sd = 10),  # Continuous age
  treatment = sample(c("A", "B"), 200, replace = TRUE),
  outcome = rbinom(200, 1, 0.4)
)

AgD <- data.frame(
  Study = paste0("Study_", 1:5),
  age_mean = c(45, 55, 65, 70, 75),
  TE = c(0.1, 0.3, 0.5, 0.4, 0.2),
  se = c(0.12, 0.10, 0.11, 0.13, 0.15)
)

# Continuous EM analysis
target_age <- 62.5
result <- NMI_interpolation_continuous(
  IPD = IPD,
  AgD = AgD,
  x_vect = c(age = target_age),
  AgD_EM_cols = "age_mean",
  IPD_EM_cols = "age",
  interpolation_method = "linear"  # or "spline"
)

print(result$Final)
```

## Core Functions

### Data Loading & Preparation
- `load_example_ipd()` - Load example individual patient data
- `load_example_agd()` - Load example aggregate data
- `GLMLOGIT()` - Convert IPD to subgroup analyses
- `BLUP_impute()` - Impute missing effect modifier data

### Analysis Functions
- `nmi_full_analysis()` - Complete NMI analysis workflow
- `NMI_interpolation()` - Core binary EM interpolation
- `NMI_interpolation_continuous()` - **New!** Continuous EM interpolation
- `NMA_run()` - Network meta-analysis
- `nmi_standard_nma()` - Standard NMA without interpolation

### Continuous EM Methods (New!)
- `detect_em_types()` - Automatic EM type detection
- `linear_interpolation()` - Linear interpolation for continuous EMs  
- `spline_interpolation()` - Spline-based interpolation
- `optimize_discretization()` - Adaptive discretization

### Visualization & Reporting
- `result_table()` - Formatted results table
- `result_forest_plot()` - Forest plot visualization
- `NMI_diagnostic_plot()` - Model diagnostics
- `generate_nmi_html_report()` - Comprehensive HTML reports

### Interactive Analysis
- `launch_nmi_app()` - Shiny web application
- `nmi_help()` - Quick help and examples

## Method Comparison

| Method | Best For | Advantages | Limitations |
|--------|----------|------------|-------------|
| **Linear** | Linear relationships, few studies | Simple, robust, fast | Limited flexibility |
| **Spline** | Non-linear relationships | Flexible, smooth curves | Needs ≥3 studies |
| **Discretization** | Interpretability | Compatible with binary methods | Information loss |

## Example Workflows

### 1. Automatic Method Selection

```r
# Let NMI automatically detect EM types and choose methods
em_types <- detect_em_types(IPD, c("age", "sex", "biomarker"))
print(em_types)
# age: continuous, sex: binary, biomarker: continuous
```

### 2. Method Comparison

```r
# Compare interpolation methods
linear_result <- linear_interpolation(AgD, c(age_mean = 65), "age_mean")
spline_result <- spline_interpolation(AgD, c(age_mean = 65), "age_mean")

comparison <- data.frame(
  Method = c("Linear", "Spline"),
  TE = c(linear_result$te, spline_result$te),
  SE = c(linear_result$se, spline_result$se)
)
```

### 3. Adaptive Discretization

```r
# Optimize continuous variable binning
discretization <- optimize_discretization(
  continuous_data = IPD$age,
  method = "equal_freq",
  max_bins = 5
)
print(discretization$bin_counts)
```

## Documentation

### Vignettes

- **Getting Started**: `vignette("getting_started", package = "nmi")`
- **Continuous EMs**: `vignette("continuous_effect_modifiers", package = "nmi")` 
- **Advanced Workflows**: `vignette("advanced_workflows", package = "nmi")`
- **HTML Reporting**: `vignette("html_reporting", package = "nmi")`
- **Stan Integration**: `vignette("cmdstanr_integration", package = "nmi")`

### Quick Access

```r
# Open vignettes
open_nmi_vignette()

# Package help
help(package = "nmi")

# Quick examples
nmi_help()
```

## Methodology

The NMI methodology addresses effect modification in network meta-analysis by:

1. **Data Integration**: Combining IPD and AgD sources
2. **Effect Modifier Modeling**: Handling binary, categorical, and continuous covariates
3. **Interpolation**: Estimating effects at target covariate values
4. **Uncertainty Quantification**: Proper propagation of estimation uncertainty

### Supported Effect Modifiers

| Type | Examples | Methods Available |
|------|----------|-------------------|
| **Binary** | Sex (0/1), Treatment history | Standard NMI |
| **Categorical** | Disease stage (1,2,3), Risk groups | Standard NMI |
| **Continuous** | Age, BMI, biomarker levels | Linear, Spline, Discretization |

## Performance & Scalability

| Dataset Size | Recommended Method | Complexity |
|--------------|-------------------|------------|
| Small (≤5 studies) | Linear interpolation | O(n log n) |
| Medium (5-20 studies) | Spline interpolation | O(n²) |
| Large (>20 studies) | Linear or pre-filtering | O(n log n) |

## Development Roadmap

### Phase 2 (v1.2.0) - Mixed EM Types
- Multiple continuous EMs support
- Binary + continuous EM combinations
- Advanced multivariate splines

### Phase 3 (v1.3.0) - Network Extensions  
- Disconnected network handling
- Real-world data integration
- Single-arm study inclusion

### Phase 4 (v1.4.0) - Advanced Analytics
- Machine learning imputation
- Uncertainty quantification
- Performance optimization

## Contributing

We welcome contributions! Please see our [development roadmap](DEVELOPMENT_ROADMAP.md) and [feature proposals](FEATURE_PROPOSAL_CONTINUOUS_EM.md).

### Development Workflow

```r
# 1. Clone repository
git clone https://github.com/choxos/nmi.git

# 2. Create feature branch
git checkout -b feature/new-functionality

# 3. Install development dependencies
devtools::install_dev_deps()

# 4. Run tests
devtools::test()

# 5. Submit pull request
```

## Citation

### Package Citation

```r
citation("nmi")
```

**For the package:**
> Sofi-Mahmudi, A. (2025). Network Meta-Interpolation (NMI) Package. R package version 1.1.0.

**For the methodology:**
> Harari et al. (2023). Network meta-interpolation: Effect modification adjustment in network meta-analysis using subgroup analyses.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact & Support

- **Author**: Ahmad Sofi-Mahmudi
- **Email**: a.sofimahmudi@gmail.com  
- **GitHub**: [https://github.com/choxos/nmi](https://github.com/choxos/nmi)
- **Issues**: [Report bugs and request features](https://github.com/choxos/nmi/issues)

---

**🚀 Ready to get started?** Install the package and run `nmi_help()` for immediate guidance!