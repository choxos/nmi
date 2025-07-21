#' Package Startup Message
#' 
#' This function is called when the package is attached and displays
#' information about the NMI package.
#' 
#' @param libname Library name (not used)
#' @param pkgname Package name (not used)
.onAttach <- function(libname, pkgname) {
  
  # Get package version
  version <- utils::packageVersion("nmi")
  
  # Create startup message
  msg <- paste0(
    "\n",
    "╔══════════════════════════════════════════════════════════════════════════════╗\n",
    "║                    Network Meta-Interpolation (NMI) Package                 ║\n",
    "║                                Version ", version, "                                  ║\n",
    "╠══════════════════════════════════════════════════════════════════════════════╣\n",
    "║  Author: Ahmad Sofi-Mahmudi                                                  ║\n",
    "║  Email:  a.sofimahmudi@gmail.com                                             ║\n",
    "║  Year:   2025                                                                ║\n",
    "║  GitHub: https://github.com/choxos/nmi                                       ║\n",
    "╠══════════════════════════════════════════════════════════════════════════════╣\n",
    "║  📚 Getting Started:                                                         ║\n",
    "║    • Basic example:    ?nmi_help                                             ║\n",
    "║    • Load example data: IPD <- load_example_ipd()                            ║\n",
    "║                        AgD <- load_example_agd()                             ║\n",
    "║    • Run analysis:     result <- nmi_full_analysis(IPD, AgD)                 ║\n",
    "║    • Launch Shiny app: launch_nmi_app()                                      ║\n",
    "║                                                                              ║\n",
    "║  📦 Installation:                                                            ║\n",
    "║    • From GitHub:      devtools::install_github('choxos/nmi')               ║\n",
    "║    • With vignettes:   devtools::install_github('choxos/nmi',               ║\n",
    "║                                        build_vignettes = TRUE)              ║\n",
    "║                                                                              ║\n",
    "║  📖 Documentation:                                                           ║\n",
    "║    • Package help:     help(package = 'nmi')                                ║\n",
    "║    • Open vignettes:   open_nmi_vignette()                                   ║\n",
    "║    • Key functions:    ?NMI_interpolation, ?NMA_run                          ║\n",
    "║                                                                              ║\n",
    "║  🎯 Methodology: Network meta-interpolation for addressing effect           ║\n",
    "║     modification in network meta-analysis using subgroup analyses           ║\n",
    "║                                                                              ║\n",
    "║  📄 Citation: Based on Harari et al. (2023) Network meta-interpolation      ║\n",
    "╚══════════════════════════════════════════════════════════════════════════════╝\n"
  )
  
  # Display the message
  packageStartupMessage(msg)
}

#' Display package citation information
#' 
#' @export
nmi_citation <- function() {
  cat("To cite the NMI package in publications, please use:\n\n")
  cat("Sofi-Mahmudi, A. (2025). Network Meta-Interpolation (NMI) Package.\n")
  cat("R package version", as.character(utils::packageVersion("nmi")), "\n")
  cat("GitHub: https://github.com/choxos/nmi\n")
  cat("Email: a.sofimahmudi@gmail.com\n\n")
  cat("For the methodology, cite:\n")
  cat("Harari et al. (2023). Network meta-interpolation: Effect modification\n")
  cat("adjustment in network meta-analysis using subgroup analyses.\n")
}

#' Quick help for new users
#' 
#' @export
nmi_help <- function() {
  cat("╔═══════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                          NMI Package Quick Help                          ║\n")
  cat("╠═══════════════════════════════════════════════════════════════════════════╣\n")
  cat("║  📦 Installation:                                                         ║\n")
  cat("║     devtools::install_github('choxos/nmi', build_vignettes = TRUE)       ║\n")
  cat("║                                                                           ║\n")
  cat("║  1. Load example data:                                                    ║\n")
  cat("║     IPD <- load_example_ipd()    # Individual patient data               ║\n")
  cat("║     AgD <- load_example_agd()    # Aggregate data                        ║\n")
  cat("║                                                                           ║\n")
  cat("║  2. Run complete analysis:                                                ║\n")
  cat("║     result <- nmi_full_analysis(IPD, AgD)                                 ║\n")
  cat("║                                                                           ║\n")
  cat("║  3. View results:                                                         ║\n")
  cat("║     result_table(result)                                                  ║\n")
  cat("║     result_forest_plot(result)                                            ║\n")
  cat("║                                                                           ║\n")
  cat("║  4. Interactive analysis:                                                 ║\n")
  cat("║     launch_nmi_app()                                                      ║\n")
  cat("║                                                                           ║\n")
  cat("║  5. Documentation:                                                        ║\n")
  cat("║     open_nmi_vignette()                                                   ║\n")
  cat("║     help(package = 'nmi')                                                 ║\n")
  cat("║                                                                           ║\n")
  cat("║  6. Core functions:                                                       ║\n")
  cat("║     ?NMI_interpolation   # Main interpolation function                   ║\n")
  cat("║     ?NMA_run            # Network meta-analysis                          ║\n")
  cat("║     ?BLUP_impute        # Data imputation                                ║\n")
  cat("║                                                                           ║\n")
  cat("║  🌟 New in v1.2.0: Continuous & Mixed Effect Modifiers                   ║\n")
  cat("║     ?NMI_interpolation_continuous   # Continuous EMs                     ║\n")
  cat("║     ?NMI_interpolation_mixed        # Mixed binary + continuous          ║\n")
  cat("║     ?detect_em_types                # Automatic EM detection             ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════════════╝\n")
} 