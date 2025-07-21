# Figures for NMI Package Manuscript

This folder should contain the following figures for the Research Synthesis Methods manuscript:

## Figure 1: Package Architecture and Workflow
- Overview diagram showing the modular structure of the NMI package
- Flow chart illustrating the main analysis pathways
- Integration points between different modules

## Figure 2: Effect Modification Patterns
- Panel A: Linear effect modification example
- Panel B: Non-linear (spline-based) effect modification
- Panel C: Threshold effect modification
- Panel D: Mixed effect modification (binary + continuous)

## Figure 3: Network Connectivity Scenarios
- Panel A: Connected network diagram
- Panel B: Disconnected network with component-wise analysis
- Panel C: Single-arm study integration example
- Panel D: Bridge augmentation visualization

## Figure 4: Missing Data Patterns and Imputation
- Panel A: Missing data pattern visualization
- Panel B: Imputation quality assessment metrics
- Panel C: Comparison of imputation methods performance
- Panel D: Impact of missing data on treatment effect estimates

## Figure 5: Simulation Study Results
- Panel A: Bias assessment across scenarios
- Panel B: Coverage probability by sample size
- Panel C: Performance comparison with alternative methods
- Panel D: Computational performance metrics

## Figure 6: Diabetes Example Results
- Panel A: Network diagram for diabetes treatments
- Panel B: Effect modification by baseline HbA1c
- Panel C: Treatment rankings across HbA1c levels
- Panel D: Comparison with traditional NMA results

## Figure 7: Shiny Application Interface
- Panel A: Data upload and validation screen
- Panel B: Analysis configuration interface
- Panel C: Results visualization dashboard
- Panel D: Export and reporting features

## Generation Instructions

Figures should be generated using the following R code structure:

```r
library(nmi)
library(ggplot2)
library(patchwork)

# Set consistent theme for all figures
theme_manuscript <- theme_bw() + 
  theme(
    text = element_text(size = 10),
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Generate each figure with appropriate dimensions
# Figure 1: 7" x 5"
# Figures 2-6: 7" x 7" (multi-panel)
# Figure 7: 7" x 9" (application screenshots)

# Save in high resolution
ggsave("figureX.tiff", width = 7, height = 5, dpi = 300, compression = "lzw")
```

## File Naming Convention

- `figure1_architecture.tiff`
- `figure2_effect_modification.tiff`
- `figure3_network_connectivity.tiff`
- `figure4_missing_data.tiff`
- `figure5_simulation_results.tiff`
- `figure6_diabetes_example.tiff`
- `figure7_shiny_interface.tiff` 