# UI for NMI Shiny App

# Define UI
ui <- dashboardPage(
  
  # Dashboard Header
  dashboardHeader(
    title = "Network Meta-Interpolation (NMI) Tool",
    titleWidth = 350
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Upload", tabName = "data", icon = icon("upload")),
      menuItem("Analysis Setup", tabName = "setup", icon = icon("cogs")),
      menuItem("NMI Analysis", tabName = "nmi", icon = icon("calculator")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("search")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        .form-control {
          border-radius: 4px;
        }
        .btn {
          border-radius: 4px;
        }
        .alert {
          border-radius: 4px;
        }
        .progress-bar {
          background-color: #3c8dbc;
        }
      "))
    ),
    
    # Tab Items
    tabItems(
      
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = "Welcome to the NMI Analysis Tool", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = "auto",
            
            h3("Network Meta-Interpolation (NMI)"),
            
            p(style = "font-size: 16px; text-align: justify;",
              "Network Meta-Interpolation (NMI) is a statistical method for addressing effect modification 
              in network meta-analysis when combining Individual Patient Data (IPD) and Aggregate Data (AgD). 
              This interactive tool allows you to perform NMI analysis with an intuitive interface."
            ),
            
            br(),
            
            fluidRow(
              column(6,
                h4("Key Features:"),
                tags$ul(
                  tags$li("Interactive data upload and validation"),
                  tags$li("Multiple outcome types (binary, continuous, count)"),
                  tags$li("Bayesian analysis with Stan"),
                  tags$li("Real-time diagnostic plots"),
                  tags$li("Comprehensive results visualization"),
                  tags$li("Educational example datasets")
                )
              ),
              column(6,
                h4("Analysis Workflow:"),
                tags$ol(
                  tags$li("Upload or select example data"),
                  tags$li("Configure analysis parameters"),
                  tags$li("Run NMI interpolation"),
                  tags$li("Perform network meta-analysis"),
                  tags$li("Review results and diagnostics")
                )
              )
            ),
            
            br(),
            
            div(style = "text-align: center;",
              actionButton("start_analysis", "Start Analysis", 
                         class = "btn-primary btn-lg",
                         icon = icon("play"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Quick Start Guide", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            h4("Getting Started:"),
            
            tags$div(
              tags$p("1. ", tags$strong("Data Upload:"), " Navigate to the 'Data Upload' tab to upload your IPD and AgD files, or use the provided example datasets."),
              tags$p("2. ", tags$strong("Analysis Setup:"), " Configure your analysis parameters including effect modifiers, outcome type, and MCMC settings."),
              tags$p("3. ", tags$strong("NMI Analysis:"), " Run the interpolation and network meta-analysis with your specified parameters."),
              tags$p("4. ", tags$strong("Results:"), " Review treatment effect estimates, credible intervals, and comparative results."),
              tags$p("5. ", tags$strong("Diagnostics:"), " Examine diagnostic plots to assess the quality of interpolation and model fit.")
            ),
            
            br(),
            
            h4("Example Data:"),
            p("The app includes example datasets based on the original NMI paper by Harari et al. (2023). 
              These datasets demonstrate the methodology with simulated data containing binary effect modifiers and outcomes.")
          )
        )
      ),
      
      # Data Upload Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Data Upload and Selection", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                h4("Data Source Selection"),
                radioButtons("data_source", "Choose data source:",
                           choices = list(
                             "Use example datasets" = "example",
                             "Upload your own data" = "upload"
                           ),
                           selected = "example"
                ),
                
                conditionalPanel(
                  condition = "input.data_source == 'example'",
                  
                  h5("Example Datasets"),
                  p("These datasets are based on the original NMI methodology paper and include:"),
                  tags$ul(
                    tags$li("IPD: Individual patient data with binary effect modifiers"),
                    tags$li("AgD: Aggregate data with treatment effect estimates"),
                    tags$li("Binary outcome (suitable for logistic regression)")
                  ),
                  
                  actionButton("load_example", "Load Example Data", 
                             class = "btn-info",
                             icon = icon("download"))
                ),
                
                conditionalPanel(
                  condition = "input.data_source == 'upload'",
                  
                  h5("Upload IPD (Individual Patient Data)"),
                  fileInput("ipd_file", "Choose IPD CSV File",
                           accept = ".csv"),
                  
                  h5("Upload AgD (Aggregate Data)"),
                  fileInput("agd_file", "Choose AgD CSV File",
                           accept = ".csv"),
                  
                  h5("Data Format Requirements"),
                  p("IPD should contain: Study, Treatment, Outcome, Effect Modifiers"),
                  p("AgD should contain: Study, Treatments, Effect Modifiers, Treatment Effects, Standard Errors")
                )
              ),
              
              column(6,
                h4("Data Validation"),
                
                verbatimTextOutput("data_status"),
                
                conditionalPanel(
                  condition = "output.data_loaded",
                  
                  h5("Data Summary"),
                  verbatimTextOutput("data_summary")
                )
              )
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.data_loaded",
            
            box(
              title = "IPD Preview", 
              status = "success", 
              solidHeader = TRUE,
              width = 6,
              
              DT::dataTableOutput("ipd_preview")
            ),
            
            box(
              title = "AgD Preview", 
              status = "success", 
              solidHeader = TRUE,
              width = 6,
              
              DT::dataTableOutput("agd_preview")
            )
          )
        )
      ),
      
      # Analysis Setup Tab
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Analysis Configuration", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            conditionalPanel(
              condition = "!output.data_loaded",
              
              div(style = "text-align: center; padding: 50px;",
                h4("Please upload or load data first"),
                p("Navigate to the 'Data Upload' tab to load your data before configuring the analysis.")
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded",
              
              fluidRow(
                column(6,
                  h4("Effect Modifier Configuration"),
                  
                  h5("Effect Modifier Values for Comparison"),
                  p("Specify the effect modifier levels at which treatment effects will be estimated:"),
                  
                  conditionalPanel(
                    condition = "output.has_two_modifiers",
                    numericInput("x1_value", "Effect Modifier 1 (x1):", 
                               value = 0.675, min = 0, max = 1, step = 0.01),
                    numericInput("x2_value", "Effect Modifier 2 (x2):", 
                               value = 0.475, min = 0, max = 1, step = 0.01)
                  ),
                  
                  h5("Outcome Type"),
                  radioButtons("outcome_type", "Select outcome type:",
                             choices = list(
                               "Binary (logistic regression)" = "binary",
                               "Continuous (linear regression)" = "continuous",
                               "Count (Poisson regression)" = "count"
                             ),
                             selected = "binary"
                  ),
                  
                  h5("Sample Sizes for AgD Studies"),
                  numericInput("sample_size", "Sample size per study:", 
                             value = 600, min = 10, max = 10000, step = 10),
                  
                  p("Note: This assumes equal sample sizes across studies. 
                    For variable sample sizes, please modify the data preparation.")
                ),
                
                column(6,
                  h4("MCMC Settings"),
                  
                  numericInput("n_chains", "Number of chains:", 
                             value = 3, min = 1, max = 8, step = 1),
                  
                  numericInput("n_iter", "Total iterations:", 
                             value = 1500, min = 500, max = 10000, step = 100),
                  
                  numericInput("burnin", "Burn-in iterations:", 
                             value = 500, min = 100, max = 5000, step = 100),
                  
                  h5("Advanced Options"),
                  
                  checkboxInput("run_diagnostics", "Generate detailed diagnostics", 
                              value = TRUE),
                  
                  checkboxInput("save_results", "Save results for download", 
                              value = TRUE),
                  
                  hr(),
                  
                  h5("Analysis Summary"),
                  verbatimTextOutput("analysis_config")
                )
              )
            )
          )
        )
      ),
      
      # NMI Analysis Tab
      tabItem(
        tabName = "nmi",
        fluidRow(
          box(
            title = "NMI Analysis Execution", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            conditionalPanel(
              condition = "!output.data_loaded",
              
              div(style = "text-align: center; padding: 50px;",
                h4("Please complete data upload and setup first"),
                p("Navigate to the 'Data Upload' and 'Analysis Setup' tabs to configure your analysis.")
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded",
              
              fluidRow(
                column(6,
                  h4("Analysis Steps"),
                  
                  div(
                    h5("1. NMI Interpolation"),
                    p("Perform network meta-interpolation using IPD and AgD."),
                    actionButton("run_interpolation", "Run Interpolation", 
                               class = "btn-info",
                               icon = icon("play")),
                    
                    br(), br(),
                    
                    h5("2. Network Meta-Analysis"),
                    p("Conduct NMA on the interpolated data."),
                    actionButton("run_nma", "Run NMA", 
                               class = "btn-success",
                               icon = icon("calculator")),
                    
                    br(), br(),
                    
                    h5("3. Generate Results"),
                    p("Compile and format analysis results."),
                    actionButton("generate_results", "Generate Results", 
                               class = "btn-warning",
                               icon = icon("chart-line"))
                  )
                ),
                
                column(6,
                  h4("Analysis Progress"),
                  
                  div(id = "progress_area",
                    h5("Status:"),
                    verbatimTextOutput("analysis_status"),
                    
                    conditionalPanel(
                      condition = "output.show_progress",
                      
                      h5("Progress:"),
                      withSpinner(
                        progressBar(id = "analysis_progress", 
                                  value = 0, 
                                  status = "info",
                                  display_pct = TRUE)
                      )
                    )
                  )
                )
              ),
              
              hr(),
              
              conditionalPanel(
                condition = "output.interpolation_complete",
                
                h4("Interpolation Results"),
                
                fluidRow(
                  column(6,
                    h5("Interpolated Data Summary"),
                    verbatimTextOutput("interpolation_summary")
                  ),
                  
                  column(6,
                    h5("Final Analysis Dataset"),
                    DT::dataTableOutput("final_data_preview")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          conditionalPanel(
            condition = "!output.analysis_complete",
            
            box(
              title = "Results", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(style = "text-align: center; padding: 50px;",
                h4("Analysis not yet complete"),
                p("Please complete the NMI analysis first to view results.")
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.analysis_complete",
            
            # Treatment Effects Summary
            box(
              title = "Treatment Effect Estimates", 
              status = "success", 
              solidHeader = TRUE,
              width = 12,
              
              fluidRow(
                column(6,
                  h4("Posterior Summary"),
                  DT::dataTableOutput("treatment_effects")
                ),
                
                column(6,
                  h4("Credible Intervals"),
                  DT::dataTableOutput("credible_intervals")
                )
              )
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.analysis_complete",
            
            # Forest Plot
            box(
              title = "Forest Plot", 
              status = "info", 
              solidHeader = TRUE,
              width = 12,
              
              withSpinner(
                plotlyOutput("forest_plot", height = "500px")
              ),
              
              br(),
              
              downloadButton("download_forest", "Download Forest Plot", 
                           class = "btn-primary")
            )
          )
        )
      ),
      
      # Diagnostics Tab
      tabItem(
        tabName = "diagnostics",
        fluidRow(
          conditionalPanel(
            condition = "!output.analysis_complete",
            
            box(
              title = "Diagnostics", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(style = "text-align: center; padding: 50px;",
                h4("Analysis not yet complete"),
                p("Please complete the NMI analysis first to view diagnostics.")
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.analysis_complete",
            
            # Interpolation Diagnostics
            box(
              title = "Interpolation Quality Assessment", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              tabsetPanel(
                tabPanel("Diagnostic Plot",
                  br(),
                  withSpinner(
                    plotlyOutput("diagnostic_plot", height = "500px")
                  ),
                  
                  br(),
                  
                  downloadButton("download_diagnostic", "Download Diagnostic Plot", 
                               class = "btn-primary")
                ),
                
                tabPanel("Goodness of Fit",
                  br(),
                  
                  fluidRow(
                    column(6,
                      h4("Observed vs Predicted"),
                      verbatimTextOutput("fit_summary")
                    ),
                    
                    column(6,
                      h4("Residual Analysis"),
                      verbatimTextOutput("residual_summary")
                    )
                  )
                ),
                
                tabPanel("Model Diagnostics",
                  br(),
                  
                  h4("MCMC Diagnostics"),
                  verbatimTextOutput("mcmc_diagnostics"),
                  
                  h4("Convergence Assessment"),
                  verbatimTextOutput("convergence_summary")
                )
              )
            )
          )
        )
      ),
      
      # Help Tab
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            title = "Help and Documentation", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              tabPanel("Method Overview",
                br(),
                
                h3("Network Meta-Interpolation (NMI)"),
                
                p(style = "text-align: justify; font-size: 16px;",
                  "Network Meta-Interpolation (NMI) is a statistical method developed by Harari et al. (2023) 
                  for conducting indirect treatment comparisons when effect modification is present. The method 
                  combines Individual Patient Data (IPD) and Aggregate Data (AgD) to estimate treatment effects 
                  at specific covariate levels."
                ),
                
                h4("Key Concepts:"),
                
                tags$ul(
                  tags$li(tags$strong("Effect Modification:"), " When treatment effects vary across patient subgroups"),
                  tags$li(tags$strong("BLUP Imputation:"), " Best Linear Unbiased Predictor for missing covariate values"),
                  tags$li(tags$strong("Subgroup Analysis:"), " Analyzing treatment effects within specific patient subgroups"),
                  tags$li(tags$strong("Bayesian Framework:"), " Using Stan for robust statistical inference")
                ),
                
                h4("Analysis Steps:"),
                
                tags$ol(
                  tags$li("Transform IPD into subgroup analyses"),
                  tags$li("Impute missing covariate values in AgD using BLUP"),
                  tags$li("Interpolate treatment effects at desired covariate levels"),
                  tags$li("Conduct network meta-analysis on interpolated data"),
                  tags$li("Assess interpolation quality through diagnostics")
                )
              ),
              
              tabPanel("Data Requirements",
                br(),
                
                h3("Data Format Requirements"),
                
                h4("Individual Patient Data (IPD):"),
                
                p("The IPD should be a CSV file containing the following columns:"),
                
                tags$table(class = "table table-striped",
                  tags$thead(
                    tags$tr(
                      tags$th("Column"),
                      tags$th("Description"),
                      tags$th("Type"),
                      tags$th("Example")
                    )
                  ),
                  tags$tbody(
                    tags$tr(tags$td("Study"), tags$td("Study identifier"), tags$td("Character/Numeric"), tags$td("1, 2, 3")),
                    tags$tr(tags$td("Treatment"), tags$td("Treatment assignment"), tags$td("Character"), tags$td("A, B, C")),
                    tags$tr(tags$td("Outcome"), tags$td("Patient outcome"), tags$td("Numeric"), tags$td("0, 1 (binary)")),
                    tags$tr(tags$td("x1, x2, ..."), tags$td("Effect modifiers"), tags$td("Numeric"), tags$td("0, 1 (binary)")),
                    tags$tr(tags$td("TrtClass"), tags$td("Treatment class"), tags$td("Character"), tags$td("Ctrl, Trt"))
                  )
                ),
                
                h4("Aggregate Data (AgD):"),
                
                p("The AgD should be a CSV file containing the following columns:"),
                
                tags$table(class = "table table-striped",
                  tags$thead(
                    tags$tr(
                      tags$th("Column"),
                      tags$th("Description"),
                      tags$th("Type"),
                      tags$th("Example")
                    )
                  ),
                  tags$tbody(
                    tags$tr(tags$td("Study"), tags$td("Study identifier"), tags$td("Character/Numeric"), tags$td("1, 2, 3")),
                    tags$tr(tags$td("Trt1"), tags$td("First treatment"), tags$td("Character"), tags$td("A, B, C")),
                    tags$tr(tags$td("Trt2"), tags$td("Second treatment"), tags$td("Character"), tags$td("B, C, D")),
                    tags$tr(tags$td("TE"), tags$td("Treatment effect"), tags$td("Numeric"), tags$td("0.5, -0.2")),
                    tags$tr(tags$td("se"), tags$td("Standard error"), tags$td("Numeric"), tags$td("0.1, 0.15")),
                    tags$tr(tags$td("x1, x2, ..."), tags$td("Effect modifier proportions"), tags$td("Numeric"), tags$td("0.6, 0.4")),
                    tags$tr(tags$td("n"), tags$td("Sample size (optional)"), tags$td("Numeric"), tags$td("100, 200"))
                  )
                )
              ),
              
              tabPanel("Interpretation",
                br(),
                
                h3("Interpreting Results"),
                
                h4("Treatment Effect Estimates:"),
                
                p("Treatment effects are presented as posterior means with 95% credible intervals. 
                  For binary outcomes, effects are on the log-odds scale. For continuous outcomes, 
                  effects are mean differences. For count outcomes, effects are log-rate ratios."),
                
                h4("Diagnostic Plots:"),
                
                p("The diagnostic plot shows observed vs predicted treatment effects. Points close to 
                  the diagonal line indicate good interpolation quality. Large deviations suggest 
                  potential issues with the interpolation model."),
                
                h4("Credible Intervals:"),
                
                p("95% credible intervals represent the range of plausible values for the treatment 
                  effect. Intervals that exclude zero (for differences) or one (for ratios) indicate 
                  statistically significant effects."),
                
                h4("Model Convergence:"),
                
                p("MCMC diagnostics assess whether the Bayesian analysis has converged. R-hat values 
                  close to 1.0 indicate good convergence. Values > 1.1 suggest convergence issues.")
              ),
              
              tabPanel("Troubleshooting",
                br(),
                
                h3("Common Issues and Solutions"),
                
                h4("Data Upload Issues:"),
                
                tags$ul(
                  tags$li(tags$strong("File not recognized:"), " Ensure files are in CSV format with proper headers"),
                  tags$li(tags$strong("Missing columns:"), " Check that all required columns are present"),
                  tags$li(tags$strong("Data type errors:"), " Verify that numeric columns contain only numbers")
                ),
                
                h4("Analysis Issues:"),
                
                tags$ul(
                  tags$li(tags$strong("Convergence problems:"), " Increase the number of iterations or chains"),
                  tags$li(tags$strong("Interpolation errors:"), " Check for extreme covariate values or missing data"),
                  tags$li(tags$strong("Memory issues:"), " Reduce the number of iterations or use fewer chains")
                ),
                
                h4("Performance Tips:"),
                
                tags$ul(
                  tags$li("Start with fewer iterations for testing"),
                  tags$li("Use the example data to verify the workflow"),
                  tags$li("Monitor R console for detailed error messages"),
                  tags$li("Ensure sufficient system memory for large datasets")
                )
              ),
              
              tabPanel("References",
                br(),
                
                h3("References and Further Reading"),
                
                h4("Primary Reference:"),
                
                p(style = "margin-left: 20px;",
                  "Harari, O., Abrams, K. R., Ades, A. E., Sutton, A. J., & Cooper, N. J. (2023). 
                  Network meta‐interpolation: Effect modification adjustment in network meta‐analysis 
                  using subgroup analyses. ", em("Journal of the Royal Statistical Society: Series A"), 
                  ", 186(2), 643-670."
                ),
                
                h4("Additional Resources:"),
                
                tags$ul(
                  tags$li("NICE Decision Support Unit Technical Support Documents"),
                  tags$li("Cochrane Handbook for Systematic Reviews of Interventions"),
                  tags$li("Stan User's Guide and Reference Manual"),
                  tags$li("multinma R package documentation")
                ),
                
                h4("Software:"),
                
                tags$ul(
                  tags$li(tags$strong("Stan:"), " https://mc-stan.org/"),
                  tags$li(tags$strong("multinma:"), " https://cran.r-project.org/package=multinma"),
                  tags$li(tags$strong("R:"), " https://www.r-project.org/"),
                  tags$li(tags$strong("Shiny:"), " https://shiny.rstudio.com/")
                )
              )
            )
          )
        )
      )
    )
  )
)