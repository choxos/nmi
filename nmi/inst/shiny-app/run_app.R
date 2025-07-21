# Script to run the NMI Shiny App
# 
# This script provides a convenient way to launch the NMI Shiny application
# from the R console or RStudio.

#' Run the NMI Shiny Application
#'
#' This function launches the interactive Shiny application for Network Meta-Interpolation
#' analysis. The app provides a user-friendly interface for conducting NMI analyses
#' with data upload, parameter configuration, and results visualization.
#'
#' @param port The port number for the Shiny app (default: NULL, uses available port)
#' @param launch.browser Whether to launch the app in a browser (default: TRUE)
#' @param host The host IP address (default: '127.0.0.1' for localhost)
#' 
#' @return None. Launches the Shiny application.
#' 
#' @examples
#' \dontrun{
#' # Run the app with default settings
#' run_nmi_app()
#' 
#' # Run the app on a specific port
#' run_nmi_app(port = 3838)
#' 
#' # Run without launching browser
#' run_nmi_app(launch.browser = FALSE)
#' }
#' 
#' @export
run_nmi_app <- function(port = NULL, launch.browser = TRUE, host = '127.0.0.1') {
  
  # Check if required packages are installed
  required_packages <- c("shiny", "shinydashboard", "shinyWidgets", "DT", 
                        "plotly", "ggplot2", "dplyr", "shinycssloaders")
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("The following required packages are missing:", 
               paste(missing_packages, collapse = ", "), 
               "\nPlease install them using: install.packages(c(", 
               paste(paste0("'", missing_packages, "'"), collapse = ", "), "))"))
  }
  
  # Get the app directory
  app_dir <- system.file("shiny-app", package = "nmi")
  
  if (app_dir == "") {
    # If package is not installed, look for app in current directory
    app_dir <- "."
    if (!file.exists(file.path(app_dir, "app.R"))) {
      stop("Cannot find the Shiny app. Please ensure the nmi package is properly installed.")
    }
  }
  
  # Launch the app
  cat("Starting NMI Shiny Application...\n")
  cat("App directory:", app_dir, "\n")
  
  if (is.null(port)) {
    shiny::runApp(app_dir, launch.browser = launch.browser, host = host)
  } else {
    shiny::runApp(app_dir, port = port, launch.browser = launch.browser, host = host)
  }
}

# If this script is run directly, launch the app
if (interactive()) {
  run_nmi_app()
}