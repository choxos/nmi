#' Launch the NMI Shiny Application
#'
#' This function launches the interactive Shiny application for Network Meta-Interpolation
#' analysis. The app provides a comprehensive interface for conducting NMI analyses with
#' data upload, parameter configuration, analysis execution, and results visualization.
#'
#' @param port The port number for the Shiny app (default: NULL, uses available port)
#' @param launch.browser Whether to launch the app in a browser (default: TRUE)
#' @param host The host IP address (default: '127.0.0.1' for localhost)
#' 
#' @details The Shiny app provides the following features:
#' \itemize{
#'   \item Interactive data upload or example data loading
#'   \item Parameter configuration for NMI analysis
#'   \item Real-time analysis execution with progress tracking
#'   \item Comprehensive results visualization
#'   \item Diagnostic plots for model assessment
#'   \item Educational help documentation
#' }
#' 
#' The app includes example datasets based on the original NMI methodology paper
#' for educational purposes and testing.
#' 
#' @return None. Launches the Shiny application.
#' 
#' @examples
#' \dontrun{
#' # Launch the app with default settings
#' launch_nmi_app()
#' 
#' # Launch on a specific port
#' launch_nmi_app(port = 3838)
#' 
#' # Launch without opening browser
#' launch_nmi_app(launch.browser = FALSE)
#' }
#' 
#' @export
launch_nmi_app <- function(port = NULL, launch.browser = TRUE, host = '127.0.0.1') {
  
  # Check if required packages are installed
  required_packages <- c("shiny", "shinydashboard", "shinyWidgets", "DT", 
                        "plotly", "ggplot2", "dplyr", "shinycssloaders")
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("The following required packages are missing:", 
               paste(missing_packages, collapse = ", "), 
               "\nPlease install them using: install.packages(c(", 
               paste0("'", missing_packages, "'", collapse = ", "), "))")
    )
  }
  
  # Check for promises compatibility if shiny is available
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny_version <- packageVersion("shiny")
    
    # Check if promises might be an issue
    tryCatch({
      if (requireNamespace("promises", quietly = TRUE)) {
        promises_version <- packageVersion("promises")
        if (promises_version < "1.3.0") {
          warning("Promises version ", promises_version, " may cause compatibility issues. Consider updating: install.packages('promises')")
        }
      }
    }, error = function(e) {
      message("Note: If you encounter promises-related errors, try: install.packages('promises')")
    })
  }
  
  # Get the app directory
  app_dir <- system.file("shiny-app", package = "nmi")
  
  if (app_dir == "") {
    stop("Cannot find the Shiny app. Please ensure the nmi package is properly installed.")
  }
  
  # Launch the app
  message("Starting NMI Shiny Application...")
  message("App directory: ", app_dir)
  
  if (is.null(port)) {
    shiny::runApp(app_dir, launch.browser = launch.browser, host = host)
  } else {
    shiny::runApp(app_dir, port = port, launch.browser = launch.browser, host = host)
  }
}