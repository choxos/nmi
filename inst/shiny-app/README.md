# NMI Shiny Application

This directory contains the interactive Shiny application for the Network Meta-Interpolation (NMI) package.

## Features

The NMI Shiny app provides a comprehensive web-based interface for conducting Network Meta-Interpolation analyses with the following features:

### 🏠 Home
- Welcome interface with methodology overview
- Quick start guide
- Feature highlights

### 📊 Data Upload
- Upload your own CSV files (IPD and AgD)
- Use built-in example datasets for learning
- Data validation and preview
- Format requirements and guidance

### ⚙️ Analysis Setup
- Configure effect modifier values
- Select outcome type (binary, continuous, count)
- Set MCMC parameters
- Advanced analysis options

### 🔬 NMI Analysis
- Step-by-step analysis execution
- Real-time progress tracking
- Interpolation and NMA analysis
- Status monitoring and error handling

### 📈 Results
- Treatment effect estimates with credible intervals
- Interactive forest plots
- Downloadable results
- Comprehensive statistical summaries

### 🔍 Diagnostics
- Interpolation quality assessment
- Goodness of fit statistics
- Model convergence diagnostics
- Interactive diagnostic plots

### ❓ Help
- Comprehensive documentation
- Method overview and key concepts
- Data format requirements
- Troubleshooting guide
- References and further reading

## Running the App

### From R Package
```r
library(nmi)
launch_nmi_app()
```

### From App Directory
```r
# Navigate to the app directory
setwd("/path/to/nmi/inst/shiny-app")

# Run the app
shiny::runApp()
```

### Using run_app.R
```r
source("run_app.R")
```

## App Structure

```
shiny-app/
├── app.R              # Main application file
├── ui.R               # User interface definition
├── server.R           # Server logic
├── nmi_functions.R    # Simplified NMI functions
├── run_app.R          # App launcher script
├── www/
│   └── style.css      # Custom CSS styling
└── README.md          # This file
```

## Educational Features

The app includes several educational components:

1. **Example Datasets**: Built-in datasets based on the original NMI methodology paper
2. **Interactive Tutorials**: Step-by-step guidance through the analysis process
3. **Method Documentation**: Comprehensive help section explaining the methodology
4. **Diagnostic Tools**: Interactive plots and statistics to understand model performance
5. **Troubleshooting Guide**: Common issues and solutions

## Data Requirements

### Individual Patient Data (IPD)
- CSV format with columns: Study, Treatment, Outcome, Effect Modifiers
- Binary effect modifiers (0/1)
- Supported outcome types: binary, continuous, count

### Aggregate Data (AgD)
- CSV format with columns: Study, Treatments, Effect Modifiers, Treatment Effects, Standard Errors
- Treatment effect estimates with corresponding standard errors
- Effect modifier summary statistics (proportions for binary modifiers)

## Technical Details

### Dependencies
- shiny: Web application framework
- shinydashboard: Dashboard layout
- shinyWidgets: Enhanced UI components
- DT: Interactive data tables
- plotly: Interactive plots
- ggplot2: Static plots
- dplyr: Data manipulation
- shinycssloaders: Loading animations

### Performance Considerations
- Simplified functions for demonstration purposes
- MCMC settings can be adjusted for faster execution
- Progress tracking for long-running analyses
- Error handling and user feedback

## Customization

### Styling
- Custom CSS in `www/style.css`
- Bootstrap-based responsive design
- Consistent color scheme and typography

### Functionality
- Modular server logic for easy extension
- Reactive programming for real-time updates
- Input validation and error handling

## Deployment

The app can be deployed to various platforms:

1. **Local**: Run on local machine for personal use
2. **Shiny Server**: Deploy to institutional Shiny Server
3. **shinyapps.io**: Deploy to RStudio's cloud platform
4. **Docker**: Containerized deployment

## Educational Use

This app is designed for:
- Teaching NMI methodology in courses
- Demonstrating the analysis workflow
- Exploring example datasets
- Understanding diagnostic procedures
- Learning about effect modification in network meta-analysis

## Support

For issues or questions:
1. Check the Help section within the app
2. Review the troubleshooting guide
3. Consult the main package documentation
4. Report issues to the package maintainers

## License

This Shiny application is part of the NMI package and follows the same MIT license terms.