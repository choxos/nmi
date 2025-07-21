# Network Meta-Interpolation (NMI) Shiny App
# 
# This app provides an interactive interface for conducting Network Meta-Interpolation
# analysis using the nmi R package.

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

# Try to load the nmi package
tryCatch({
  library(nmi)
}, error = function(e) {
  # If nmi package is not installed, source the functions
  source("nmi_functions.R", local = TRUE)
})

# Source additional modules
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run the app
shinyApp(ui = ui, server = server)