#' Open NMI Package Vignettes
#'
#' Opens NMI package vignettes in your default browser when the standard 
#' vignette() function has display issues.
#'
#' @param topic Character string specifying which vignette to open. 
#'   Options: "getting_started", "complete_methodology_comparison", "cmdstanr_integration", 
#'   "html_reporting", "advanced_workflows", "nmi_introduction", or "all" to see all available.
#'
#' @return Invisibly returns the file path(s) to the vignette(s).
#'
#' @examples
#' \dontrun{
#' # Open the getting started guide
#' open_nmi_vignette("getting_started")
#' 
#' # See all available vignettes
#' open_nmi_vignette("all")
#' }
#'
#' @export
open_nmi_vignette <- function(topic = "all") {
  pkg_path <- find.package("nmi")
  doc_path <- file.path(pkg_path, "doc")
  
  vignettes <- list(
    getting_started = "getting_started.html",
    complete_methodology_comparison = "complete_methodology_comparison.html",
    cmdstanr_integration = "cmdstanr_integration.html", 
    html_reporting = "html_reporting.html",
    advanced_workflows = "advanced_workflows.html",
    nmi_introduction = "nmi_introduction.html"
  )
  
  if (topic == "all") {
    cat("Available NMI Package Vignettes:\n")
    cat("===============================\n\n")
    
    cat("1. Getting Started Guide:\n")
    cat("   open_nmi_vignette('getting_started')\n")
    cat("   Quick start guide and basic usage\n\n")
    
    cat("2. Complete Methodology Comparison: ⭐ COMPREHENSIVE GUIDE\n")
    cat("   open_nmi_vignette('complete_methodology_comparison')\n")
    cat("   Compare NMI vs ML-NMR vs NMA with visualizations and basic explanations\n\n")
    
    cat("3. cmdstanr Integration:\n") 
    cat("   open_nmi_vignette('cmdstanr_integration')\n")
    cat("   Modern Bayesian inference setup and optimization\n\n")
    
    cat("4. HTML Reporting:\n")
    cat("   open_nmi_vignette('html_reporting')\n") 
    cat("   Professional report generation and customization\n\n")
    
    cat("5. Advanced Workflows:\n")
    cat("   open_nmi_vignette('advanced_workflows')\n")
    cat("   Complex analysis patterns and real-world use cases\n\n")
    
    cat("6. NMI Introduction:\n")
    cat("   open_nmi_vignette('nmi_introduction')\n")
    cat("   Statistical methodology and theoretical background\n\n")
    
    cat("Direct file paths (can be opened in any browser):\n")
    for (name in names(vignettes)) {
      file_path <- file.path(doc_path, vignettes[[name]])
      cat(paste0("  ", name, ": ", file_path, "\n"))
    }
    
    return(invisible(file.path(doc_path, unlist(vignettes))))
  }
  
  if (!topic %in% names(vignettes)) {
    stop("Unknown vignette topic. Use open_nmi_vignette('all') to see available options.")
  }
  
  file_path <- file.path(doc_path, vignettes[[topic]])
  
  if (!file.exists(file_path)) {
    stop("Vignette file not found: ", file_path)
  }
  
  cat("Opening vignette:", topic, "\n")
  cat("File path:", file_path, "\n")
  
  # Try to open in browser
  if (.Platform$OS.type == "windows") {
    shell.exec(file_path)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", shQuote(file_path)))
  } else {
    system(paste("xdg-open", shQuote(file_path)))
  }
  
  return(invisible(file_path))
}

#' List Available NMI Documentation
#'
#' Shows all available documentation for the NMI package including
#' vignettes, function help, and example data.
#'
#' @return Invisibly returns a list of documentation resources.
#'
#' @examples
#' \dontrun{
#' nmi_help()
#' }
#'
#' @export
nmi_help <- function() {
  cat("NMI Package Documentation\n")
  cat("========================\n\n")
  
  cat("📚 VIGNETTES (Comprehensive Guides):\n")
  cat("  open_nmi_vignette('getting_started')                   # Quick start guide\n")
  cat("  open_nmi_vignette('complete_methodology_comparison') ⭐ # Method comparisons & visualizations\n")
  cat("  open_nmi_vignette('cmdstanr_integration')               # Bayesian setup\n") 
  cat("  open_nmi_vignette('html_reporting')                    # Professional reports\n")
  cat("  open_nmi_vignette('advanced_workflows')                # Complex workflows\n")
  cat("  open_nmi_vignette('nmi_introduction')                  # Statistical background\n\n")
  
  cat("📖 FUNCTION HELP:\n")
  cat("  ?nmi_full_analysis        # Main analysis function\n")
  cat("  ?generate_nmi_html_report # HTML report generation\n") 
  cat("  ?BLUP_impute             # Imputation method\n")
  cat("  ?NMI_interpolation       # Core interpolation\n")
  cat("  help(package = 'nmi')    # All functions\n\n")
  
  cat("🗃️ EXAMPLE DATA:\n")
  cat("  data(Example_IPD)        # Individual patient data\n")
  cat("  data(Example_AgD_NMI)    # Aggregate data\n\n")
  
  cat("🚀 QUICK START:\n")
  cat("  1. Start with: open_nmi_vignette('getting_started')\n")
  cat("  2. Compare methods: open_nmi_vignette('complete_methodology_comparison') ⭐\n")
  cat("  3. Load data:  data(Example_IPD); data(Example_AgD_NMI)\n")
  cat("  4. Get help:   ?nmi_full_analysis\n\n")
  
  invisible(list(
    vignettes = c("getting_started", "complete_methodology_comparison", "cmdstanr_integration", 
                  "html_reporting", "advanced_workflows", "nmi_introduction"),
    main_functions = c("nmi_full_analysis", "generate_nmi_html_report", 
                       "BLUP_impute", "NMI_interpolation"),
    example_data = c("Example_IPD", "Example_AgD_NMI")
  ))
} 