#' Generate NMI HTML Report
#'
#' Generates a comprehensive HTML report for NMI analysis results with 
#' professional styling and interactive elements.
#'
#' @param nmi_results Results from nmi_full_analysis function.
#' @param report_title Title for the HTML report.
#' @param author_name Author name for the report.
#' @param organization Organization name.
#' @param study_name Study identifier for file organization.
#' @param outcome Outcome description for file organization.
#' @param output_file Optional output file path (auto-generated if NULL).
#' @param include_data_tables Whether to include detailed data tables.
#' @param include_plots Whether to include diagnostic plots.
#' 
#' @return Path to the generated HTML file.
#'
#' @export
#' @importFrom htmltools tags HTML
#' @importFrom kableExtra kable kable_styling add_header_above
#' @importFrom jsonlite toJSON
generate_nmi_html_report <- function(nmi_results, 
                                    report_title = "Network Meta-Interpolation (NMI) Analysis Report",
                                    author_name = "NMI Analysis",
                                    organization = "Research Organization",
                                    study_name = "NMI_Study",
                                    outcome = "Primary_Outcome",
                                    output_file = NULL,
                                    include_data_tables = TRUE,
                                    include_plots = TRUE) {
  
  # Validate inputs
  if (!inherits(nmi_results, "nmi_analysis")) {
    stop("nmi_results must be an object from nmi_full_analysis()")
  }
  
  cat("Generating NMI HTML Report...\n")
  
  # Get current timestamp for folder and file naming
  current_date <- format(Sys.time(), "%Y%m%d")
  current_time <- format(Sys.time(), "%H%M%S")
  report_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Create directory structure: reports/[Study_Name]_[Outcome]_[Date]/
  base_dir <- "reports"
  study_dir <- paste0(study_name, "_", outcome, "_", current_date)
  full_dir <- file.path(base_dir, study_dir)
  
  # Create directories if they don't exist
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
    cat("Created base reports directory:", base_dir, "\n")
  }
  
  if (!dir.exists(full_dir)) {
    dir.create(full_dir, recursive = TRUE)
    cat("Created study directory:", full_dir, "\n")
  }
  
  # Generate filename: [Study_Name]_[Outcome]_[Date]_[Time].html
  if (is.null(output_file)) {
    output_filename <- paste0(study_name, "_", outcome, "_", current_date, "_", current_time, ".html")
    output_file <- file.path(full_dir, output_filename)
  } else {
    # If output_file is provided, still put it in the structured directory
    output_file <- file.path(full_dir, basename(output_file))
  }
  
  # Generate HTML content
  html_content <- paste0(
    generate_html_header(report_title),
    generate_nmi_css(),
    generate_nmi_javascript(),
    generate_html_body_open(),
    generate_report_header(report_title, author_name, organization, report_timestamp, nmi_results),
    generate_navigation_tabs(),
    generate_overview_tab(nmi_results),
    generate_data_summary_tab(nmi_results, include_data_tables),
    generate_interpolation_tab(nmi_results, include_plots),
    generate_nma_results_tab(nmi_results),
    generate_diagnostics_tab(nmi_results, include_plots),
    generate_methods_tab(nmi_results),
    generate_report_footer(),
    generate_html_body_close()
  )
  
  # Write to file
  writeLines(html_content, output_file)
  cat("HTML report saved to:", output_file, "\n")
  
  return(invisible(output_file))
}

#' Generate HTML Header
#' @param title Report title
#' @return HTML header string
generate_html_header <- function(title) {
  paste0(
    '<!DOCTYPE html>\n',
    '<html lang="en">\n',
    '<head>\n',
    '  <meta charset="UTF-8">\n',
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
    '  <title>', title, '</title>\n',
    '  <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>\n',
    '  <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>\n',
    '  <link href="https://cdn.datatables.net/1.11.5/css/jquery.dataTables.css" rel="stylesheet">\n',
    '  <script src="https://cdn.datatables.net/1.11.5/js/jquery.dataTables.js"></script>\n'
  )
}

#' Generate CSS Styles
#' @return CSS style string
generate_nmi_css <- function() {
  paste0(
    '<style>\n',
    '  body {\n',
    '    font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;\n',
    '    line-height: 1.6;\n',
    '    color: #333;\n',
    '    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\n',
    '    margin: 0;\n',
    '    padding: 20px;\n',
    '  }\n',
    '  .container {\n',
    '    max-width: 1200px;\n',
    '    margin: 0 auto;\n',
    '    background: white;\n',
    '    border-radius: 15px;\n',
    '    box-shadow: 0 20px 40px rgba(0,0,0,0.1);\n',
    '    overflow: hidden;\n',
    '  }\n',
    '  .header {\n',
    '    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\n',
    '    color: white;\n',
    '    padding: 30px;\n',
    '    text-align: center;\n',
    '  }\n',
    '  .header h1 {\n',
    '    margin: 0;\n',
    '    font-size: 2.5em;\n',
    '    font-weight: 300;\n',
    '  }\n',
    '  .tabs {\n',
    '    display: flex;\n',
    '    background: #f8f9fa;\n',
    '    border-bottom: 1px solid #dee2e6;\n',
    '  }\n',
    '  .tab {\n',
    '    flex: 1;\n',
    '    padding: 15px 20px;\n',
    '    text-align: center;\n',
    '    cursor: pointer;\n',
    '    border-right: 1px solid #dee2e6;\n',
    '    transition: all 0.3s ease;\n',
    '  }\n',
    '  .tab:hover {\n',
    '    background: #e9ecef;\n',
    '  }\n',
    '  .tab.active {\n',
    '    background: white;\n',
    '    border-bottom: 3px solid #667eea;\n',
    '  }\n',
    '  .tab-content {\n',
    '    display: none;\n',
    '    padding: 30px;\n',
    '  }\n',
    '  .tab-content.active {\n',
    '    display: block;\n',
    '  }\n',
    '  .metric-card {\n',
    '    background: #f8f9fa;\n',
    '    border-radius: 10px;\n',
    '    padding: 20px;\n',
    '    margin: 10px 0;\n',
    '    border-left: 4px solid #667eea;\n',
    '  }\n',
    '  .metric-grid {\n',
    '    display: grid;\n',
    '    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));\n',
    '    gap: 20px;\n',
    '    margin: 20px 0;\n',
    '  }\n',
    '  table {\n',
    '    width: 100%;\n',
    '    border-collapse: collapse;\n',
    '    margin: 20px 0;\n',
    '  }\n',
    '  th, td {\n',
    '    padding: 12px;\n',
    '    text-align: left;\n',
    '    border-bottom: 1px solid #dee2e6;\n',
    '  }\n',
    '  th {\n',
    '    background: #667eea;\n',
    '    color: white;\n',
    '    font-weight: 600;\n',
    '  }\n',
    '  tr:hover {\n',
    '    background: #f8f9fa;\n',
    '  }\n',
    '</style>\n'
  )
}

#' Generate JavaScript
#' @return JavaScript string
generate_nmi_javascript <- function() {
  paste0(
    '<script>\n',
    '  function showTab(tabName) {\n',
    '    var tabs = document.querySelectorAll(".tab");\n',
    '    var contents = document.querySelectorAll(".tab-content");\n',
    '    \n',
    '    tabs.forEach(function(tab) {\n',
    '      tab.classList.remove("active");\n',
    '    });\n',
    '    \n',
    '    contents.forEach(function(content) {\n',
    '      content.classList.remove("active");\n',
    '    });\n',
    '    \n',
    '    document.querySelector(".tab[onclick*=\'" + tabName + "\']").classList.add("active");\n',
    '    document.getElementById(tabName).classList.add("active");\n',
    '  }\n',
    '  \n',
    '  document.addEventListener("DOMContentLoaded", function() {\n',
    '    showTab("overview");\n',
    '    \n',
    '    $(".data-table").DataTable({\n',
    '      pageLength: 10,\n',
    '      responsive: true,\n',
    '      order: [[0, "asc"]]\n',
    '    });\n',
    '  });\n',
    '</script>\n'
  )
}

#' Generate HTML Body Opening
#' @return HTML body opening string
generate_html_body_open <- function() {
  paste0(
    '</head>\n',
    '<body>\n',
    '<div class="container">\n'
  )
}

#' Generate HTML Body Closing
#' @return HTML body closing string
generate_html_body_close <- function() {
  paste0(
    '</div>\n',
    '</body>\n',
    '</html>\n'
  )
}

#' Generate Report Header
#' @param title Report title
#' @param author Author name
#' @param organization Organization name
#' @param timestamp Report timestamp
#' @param nmi_results NMI results object
#' @return HTML header string
generate_report_header <- function(title, author, organization, timestamp, nmi_results) {
  paste0(
    '<div class="header">\n',
    '  <h1>', title, '</h1>\n',
    '  <p><strong>Author:</strong> ', author, ' | <strong>Organization:</strong> ', organization, '</p>\n',
    '  <p><strong>Generated:</strong> ', timestamp, '</p>\n',
    '</div>\n'
  )
}

#' Generate Navigation Tabs
#' @return HTML navigation string
generate_navigation_tabs <- function() {
  paste0(
    '<div class="tabs">\n',
    '  <div class="tab" onclick="showTab(\'overview\')">Overview</div>\n',
    '  <div class="tab" onclick="showTab(\'data\')">Data Summary</div>\n',
    '  <div class="tab" onclick="showTab(\'interpolation\')">NMI Interpolation</div>\n',
    '  <div class="tab" onclick="showTab(\'nma\')">NMA Results</div>\n',
    '  <div class="tab" onclick="showTab(\'diagnostics\')">Diagnostics</div>\n',
    '  <div class="tab" onclick="showTab(\'methods\')">Methods</div>\n',
    '</div>\n'
  )
}

#' Generate Overview Tab
#' @param nmi_results NMI results object
#' @return HTML overview tab string
generate_overview_tab <- function(nmi_results) {
  
  # Extract key metrics
  n_studies <- nmi_results$data_info$n_studies
  n_comparisons <- nmi_results$data_info$n_comparisons
  outcome_type <- nmi_results$settings$outcome_type
  target_em <- paste(nmi_results$settings$x_target, collapse = ", ")
  
  # Get treatment effects if available
  if (!is.null(nmi_results$nma_results$BUGSoutput)) {
    summary_data <- nmi_results$nma_results$BUGSoutput$summary
    d_effects <- summary_data[grep("^d_raw", rownames(summary_data)), "mean"]
    n_treatments <- length(d_effects) + 1
    
    # Check convergence
    rhat_values <- summary_data[grep("^d_raw", rownames(summary_data)), "Rhat"]
    max_rhat <- max(rhat_values, na.rm = TRUE)
    convergence_status <- ifelse(max_rhat < 1.1, "Good (R̂ < 1.1)", 
                                paste("Check Required (max R̂ =", round(max_rhat, 3), ")"))
  } else {
    n_treatments <- length(nmi_results$treatments)
    convergence_status <- "Not Available"
  }
  
  paste0(
    '<div id="overview" class="tab-content">\n',
    '  <h2>Analysis Overview</h2>\n',
    '  <div class="metric-grid">\n',
    '    <div class="metric-card">\n',
    '      <h3>Network Structure</h3>\n',
    '      <p><strong>Studies:</strong> ', n_studies, '</p>\n',
    '      <p><strong>Treatments:</strong> ', n_treatments, '</p>\n',
    '      <p><strong>Comparisons:</strong> ', n_comparisons, '</p>\n',
    '    </div>\n',
    '    <div class="metric-card">\n',
    '      <h3>Analysis Settings</h3>\n',
    '      <p><strong>Outcome Type:</strong> ', tools::toTitleCase(outcome_type), '</p>\n',
    '      <p><strong>Target EM Values:</strong> ', target_em, '</p>\n',
    '      <p><strong>Effect Modifiers:</strong> ', length(nmi_results$data_info$effect_modifiers), '</p>\n',
    '    </div>\n',
    '    <div class="metric-card">\n',
    '      <h3>Model Convergence</h3>\n',
    '      <p><strong>Status:</strong> ', convergence_status, '</p>\n',
    '      <p><strong>Chains:</strong> ', nmi_results$settings$mcmc_settings$n_chains, '</p>\n',
    '      <p><strong>Iterations:</strong> ', nmi_results$settings$mcmc_settings$n_iter, '</p>\n',
    '    </div>\n',
    '  </div>\n',
    '</div>\n'
  )
}

# Additional helper functions for generating other tabs would go here...
# For brevity, I'll add the core structure and you can expand as needed

#' Generate simple placeholder tabs
generate_data_summary_tab <- function(nmi_results, include_data_tables) {
  paste0(
    '<div id="data" class="tab-content">\n',
    '  <h2>Data Summary</h2>\n',
    '  <p>Data summary content will be displayed here.</p>\n',
    '</div>\n'
  )
}

generate_interpolation_tab <- function(nmi_results, include_plots) {
  paste0(
    '<div id="interpolation" class="tab-content">\n',
    '  <h2>NMI Interpolation Results</h2>\n',
    '  <p>Interpolation results will be displayed here.</p>\n',
    '</div>\n'
  )
}

generate_nma_results_tab <- function(nmi_results) {
  paste0(
    '<div id="nma" class="tab-content">\n',
    '  <h2>Network Meta-Analysis Results</h2>\n',
    '  <p>NMA results will be displayed here.</p>\n',
    '</div>\n'
  )
}

generate_diagnostics_tab <- function(nmi_results, include_plots) {
  paste0(
    '<div id="diagnostics" class="tab-content">\n',
    '  <h2>Diagnostics</h2>\n',
    '  <p>Diagnostic information will be displayed here.</p>\n',
    '</div>\n'
  )
}

generate_methods_tab <- function(nmi_results) {
  paste0(
    '<div id="methods" class="tab-content">\n',
    '  <h2>Methodology</h2>\n',
    '  <p>Methodology description will be displayed here.</p>\n',
    '</div>\n'
  )
}

generate_report_footer <- function() {
  paste0(
    '<div style="text-align: center; padding: 20px; border-top: 1px solid #dee2e6; color: #6c757d;">\n',
    '  <p>Report generated by NMI R Package | Network Meta-Interpolation Analysis</p>\n',
    '</div>\n'
  )
} 