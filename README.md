# Network Meta-Interpolation (NMI) Package

[![R Package](https://img.shields.io/badge/R%20Package-1.0.0-blue.svg)](https://github.com/sofimahmudi/nmi)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

The **nmi** package implements Network Meta-Interpolation (NMI) methodology for addressing effect modification in network meta-analysis using subgroup analyses. This method allows researchers to relax shared effect modification assumptions and interpolate treatment effects at specific covariate levels using both individual patient data (IPD) and aggregate data (AgD).

## Key Features

- 🔗 **Network Meta-Interpolation**: Advanced methodology for handling effect modification
- 📊 **Mixed Data Types**: Seamlessly combines IPD and AgD in a single analysis
- 🎯 **Flexible Outcomes**: Supports binary, continuous, and count outcomes
- 📈 **Bayesian Framework**: Uses Stan for robust statistical inference
- 🌐 **Interactive Interface**: Includes Shiny web application for user-friendly analysis
- 📋 **Professional Reports**: Generates comprehensive HTML reports
- 🔧 **Comprehensive Toolkit**: Complete suite of visualization and diagnostic tools

## Methodology

Network Meta-Interpolation addresses a critical limitation in traditional network meta-analysis by allowing treatment effects to vary across patient subgroups. The method:

1. **Relaxes assumptions** about shared effect modification across studies
2. **Interpolates treatment effects** at desired covariate levels
3. **Combines different data sources** efficiently
4. **Provides uncertainty quantification** through Bayesian inference

Based on the methodology described in: *Harari et al. (2023) "Network meta-interpolation: Effect modification adjustment in network meta-analysis using subgroup analyses"*

## Installation

### Prerequisites

The package requires cmdstanr for Bayesian inference:

```r
# Install cmdstanr
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Install CmdStan backend
library(cmdstanr)
install_cmdstan()
```

### Install NMI Package

```r
# Install from GitHub
devtools::install_github("sofimahmudi/nmi")

# Or install from local source
devtools::install_local("path/to/nmi")
```

## Quick Start

```r
library(nmi)

# Load example data
IPD <- load_example_ipd()
AgD <- load_example_agd()

# Define analysis parameters
x_vect <- c(0.675, 0.475)  # Target covariate levels

# Perform NMI analysis
result <- nmi_full_analysis(
  IPD = IPD,
  AgD = AgD,
  x_vect = x_vect
)

# View results
result_table(result)
result_forest_plot(result)

# Launch interactive app
launch_nmi_app()
```

## Core Functions

| Function | Description |
|----------|-------------|
| `NMI_interpolation()` | Main interpolation function |
| `nmi_full_analysis()` | Complete analysis pipeline |
| `NMA_run()` | Network meta-analysis |
| `result_table()` | Results formatting |
| `result_forest_plot()` | Forest plot visualization |
| `launch_nmi_app()` | Interactive Shiny application |
| `generate_nmi_html_report()` | HTML report generation |

## Documentation

- **Getting Started**: `vignette("getting_started")`
- **Advanced Workflows**: `vignette("advanced_workflows")`
- **HTML Reporting**: `vignette("html_reporting")`
- **Methodology Details**: `vignette("complete_methodology_comparison")`
- **Help Functions**: `nmi_help()` or `?nmi`

## Interactive Analysis

Launch the Shiny web application for point-and-click analysis:

```r
launch_nmi_app()
```

The app provides:
- 📁 Data upload interface
- ⚙️ Parameter configuration
- 🔄 Real-time analysis
- 📊 Interactive visualizations
- 📄 Report generation

## Citation

To cite the NMI package:

```r
# Package citation
nmi_citation()
```

**Package Citation:**
> Sofi-Mahmudi, A. (2025). Network Meta-Interpolation (NMI) Package. R package version 1.0.0. Email: a.sofimahmudi@gmail.com

**Methodology Citation:**
> Harari et al. (2023). Network meta-interpolation: Effect modification adjustment in network meta-analysis using subgroup analyses.

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

**Ahmad Sofi-Mahmudi**
- Email: a.sofimahmudi@gmail.com
- Year: 2025

## Acknowledgments

- Based on the Network Meta-Interpolation methodology by Harari et al. (2023)
- Built with R, Stan, and the Stan ecosystem
- Utilizes the multinma package for network meta-analysis foundations