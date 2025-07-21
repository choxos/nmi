# NMI Package: Network Meta-Interpolation

[![R](https://img.shields.io/badge/R-%E2%89%A5%203.5.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/Version-1.4.0-green)](https://github.com/choxos/nmi)

A comprehensive R package implementing **Network Meta-Interpolation (NMI)** methodology for addressing effect modification in network meta-analysis using subgroup analyses.

## 🆕 What's New in v1.4.0

- 🤖 **Machine Learning Imputation**: Random Forest, XGBoost, and KNN-based missing data imputation
- 📊 **Advanced Missing Data Analysis**: Sophisticated pattern detection and strategy selection
- 🔄 **Multiple Imputation Integration**: Rubin's rules for pooling results across imputations
- ⚡ **Performance Optimizations**: Enhanced computational efficiency for large datasets
- 🛠️ **Quality Assessment**: Comprehensive imputation quality evaluation metrics

## Overview

The NMI package enables researchers to perform network meta-analysis while accounting for effect modification by combining:

- **Individual Patient Data (IPD)**: Rich, patient-level information
- **Aggregate Data (AgD)**: Study-level summaries from literature
- **Advanced Interpolation**: Estimate treatment effects at target covariate values

### Key Features Evolution

| Feature | Binary EMs | Continuous EMs | Mixed EMs | Network Extensions | ML Imputation |
|---------|------------|----------------|-----------|-------------------|---------------|
| **Basic Interpolation** | ✅ v1.0.0 | ✅ v1.1.0 | ✅ v1.2.0 | ✅ v1.3.0 | ✅ v1.4.0 |
| **Linear Methods** | ✅ v1.0.0 | ✅ v1.1.0 | ✅ v1.2.0 | ✅ v1.3.0 | ✅ v1.4.0 |
| **Spline Methods** | ➖ | ✅ v1.1.0 | ✅ v1.2.0 | ✅ v1.3.0 | ✅ v1.4.0 |
| **Auto-Detection** | ✅ v1.0.0 | ✅ v1.1.0 | ✅ v1.2.0 | ✅ v1.3.0 | ✅ v1.4.0 |
| **Disconnected Networks** | ➖ | ➖ | ➖ | ✅ v1.3.0 | ✅ v1.4.0 |
| **Single-arm Integration** | ➖ | ➖ | ➖ | ✅ v1.3.0 | ✅ v1.4.0 |
| **Missing Data ML** | ➖ | ➖ | ➖ | ➖ | ✅ v1.4.0 |

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

### With Machine Learning Features

For advanced ML imputation capabilities:

```r
# Install ML dependencies
install.packages(c("randomForest", "xgboost", "VIM"))

# Then install NMI with full capabilities
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

### Continuous Effect Modifiers

```r
library(nmi)

# Create data with age as continuous EM
IPD <- data.frame(
  Study = rep("IPD_Study", 200),
  age = rnorm(200, mean = 60, sd = 10),  # Continuous age
  treatment = sample(c("A", "B"), 200, replace = TRUE),
  outcome = rbinom(200, 1, 0.4)
)

# Continuous EM analysis
target_age <- 62.5
result <- NMI_interpolation_continuous(
  IPD = IPD, AgD = AgD, x_vect = c(age = target_age),
  AgD_EM_cols = "age_mean", IPD_EM_cols = "age",
  interpolation_method = "spline"  # Linear, spline, or discretization
)
```

### Mixed Effect Modifiers

```r
# Mixed binary + continuous EMs
result <- NMI_interpolation_mixed(
  IPD = IPD, AgD = AgD, 
  x_vect = c(age = 65, sex = 1),  # Continuous age + binary sex
  AgD_EM_cols = c("age_mean", "sex_prop"),
  IPD_EM_cols = c("age", "sex")
)
```

### Network Extensions (v1.3.0+)

```r
# Handle disconnected networks
connectivity <- detect_network_connectivity(AgD, c("Trt1", "Trt2"), "Study")

if (!connectivity$is_connected) {
  result <- handle_disconnected_network(
    IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
    trt_cols = c("Trt1", "Trt2"), study_col = "Study",
    strategy = "component_wise"
  )
}

# Integrate single-arm studies
result <- nmi_with_single_arm_integration(
  IPD, AgD_mixed, x_vect, AgD_EM_cols, IPD_EM_cols,
  trt_cols = c("Trt1", "Trt2"), study_col = "Study",
  reference_treatment = "Placebo"
)
```

### Machine Learning Imputation (v1.4.0+)

```r
# Automatic ML-based missing data imputation
result <- nmi_with_ml_imputation(
  IPD = IPD_with_missing, AgD = AgD,
  x_vect = c(age = 65), 
  AgD_EM_cols = "age_mean", IPD_EM_cols = "age",
  imputation_method = "random_forest",
  n_imputations = 5
)

# Advanced missing data analysis
missing_analysis <- detect_missing_patterns(
  IPD_with_missing, AgD, c("age", "sex"), "outcome"
)
print(missing_analysis$strategy)  # Recommended imputation strategy
```

## Core Functions

### Data Loading & Preparation
- `load_example_ipd()` - Load example individual patient data
- `load_example_agd()` - Load example aggregate data
- `GLMLOGIT()` - Convert IPD to subgroup analyses
- `BLUP_impute()` - Basic imputation for missing effect modifier data

### Analysis Functions
- `nmi_full_analysis()` - Complete NMI analysis workflow
- `NMI_interpolation()` - Core binary EM interpolation
- `NMI_interpolation_continuous()` - **v1.1.0+** Continuous EM interpolation
- `NMI_interpolation_mixed()` - **v1.2.0+** Mixed EM interpolation
- `NMA_run()` - Network meta-analysis
- `nmi_standard_nma()` - Standard NMA without interpolation

### Network Extensions (v1.3.0+)
- `detect_network_connectivity()` - Analyze network structure
- `handle_disconnected_network()` - Manage disconnected networks
- `detect_single_arm_studies()` - Identify single-arm studies
- `nmi_with_single_arm_integration()` - Integrate single-arm evidence

### Machine Learning Imputation (v1.4.0+)
- `detect_missing_patterns()` - **New!** Analyze missing data patterns
- `ml_imputation()` - **New!** ML-based imputation (Random Forest, XGBoost, KNN)
- `evaluate_imputation_quality()` - **New!** Assess imputation performance
- `nmi_with_ml_imputation()` - **New!** Complete NMI with ML imputation

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
| **Mixed** | Multiple EM types | Handles complex modification | Higher complexity |
| **ML Imputation** | Missing data | Advanced pattern recognition | Requires larger samples |

## Advanced Features

### Effect Modifier Types

| EM Type | Description | NMI Support | Examples |
|---------|-------------|-------------|----------|
| **Binary** | Two categories (0/1) | ✅ Full support | Sex (M/F), Treatment history (Y/N) |
| **Categorical** | Multiple categories | ✅ Standard NMI | Disease stage (1,2,3), Risk groups |
| **Continuous** | Real-valued variables | ✅ v1.1.0+ | Age, BMI, biomarker levels |
| **Mixed** | Binary + continuous | ✅ v1.2.0+ | Age + sex, BMI + treatment history |
| **Multiple Continuous** | 2+ continuous EMs | ✅ v1.2.0+ | Age + BMI + baseline score |

### Missing Data Handling

| Scenario | Recommended Method | NMI Capability |
|----------|-------------------|----------------|
| **< 5% missing** | Simple imputation | Built-in mean/mode |
| **5-20% missing** | Multiple imputation | MICE-compatible |
| **> 20% missing** | ML imputation | ✅ v1.4.0+ Random Forest/XGBoost |
| **Complex patterns** | Advanced ML | ✅ v1.4.0+ Pattern detection |

### Network Types

| Network Type | Analysis Approach | NMI Support |
|--------------|-------------------|-------------|
| **Connected** | Standard NMI | ✅ All versions |
| **Disconnected** | Component-wise | ✅ v1.3.0+ |
| **Star network** | Hub-based analysis | ✅ Reference connection |
| **Mixed evidence** | Single-arm integration | ✅ v1.3.0+ |

## Performance & Scalability

| Dataset Size | Recommended Configuration | Complexity |
|--------------|---------------------------|------------|
| **Small (≤5 studies)** | Linear interpolation | O(n log n) |
| **Medium (5-20 studies)** | Spline interpolation | O(n²) |
| **Large (>20 studies)** | ML imputation + filtering | O(n log n) |
| **Missing data >20%** | Advanced ML methods | O(n² log n) |

## Development Roadmap

### ✅ Completed Features (v1.0.0 - v1.4.0)
- ✅ **Phase 1 (v1.1.0)**: Continuous Effect Modifiers
- ✅ **Phase 2 (v1.2.0)**: Mixed EM Types & Multi-Continuous EMs  
- ✅ **Phase 3 (v1.3.0)**: Network Extensions (Disconnected networks, Single-arm integration)
- ✅ **Phase 4 (v1.4.0)**: Advanced Analytics (ML imputation, Missing data analysis)

### 🔄 Future Development (v1.5.0+)
- 🔄 **Phase 5 (v1.5.0)**: Performance & Scalability
  - Parallel processing optimization
  - Memory-efficient algorithms
  - Large dataset handling
- 🔄 **Phase 6 (v1.6.0)**: API & Integration
  - REST API development
  - Cloud deployment tools
  - Integration with meta-analysis platforms

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
> Sofi-Mahmudi, A. (2025). Network Meta-Interpolation (NMI) Package. R package version 1.4.0. GitHub: https://github.com/choxos/nmi

**For the methodology:**
> Harari et al. (2023). Network meta-interpolation: Effect modification adjustment in network meta-analysis using subgroup analyses.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact & Support

- **Author**: Ahmad Sofi-Mahmudi
- **Email**: a.sofimahmudi@gmail.com  
- **GitHub**: https://github.com/choxos/nmi
- **Issues**: https://github.com/choxos/nmi/issues

### Getting Help

```r
# Quick help
nmi_help()

# Package documentation
help(package = "nmi")

# Open vignettes
open_nmi_vignette()
```

**🌟 The NMI package now provides state-of-the-art capabilities for network meta-analysis with advanced effect modification handling, network extensions, and machine learning-powered missing data imputation!** 🚀