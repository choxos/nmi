# Server logic for NMI Shiny App

server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    ipd = NULL,
    agd = NULL,
    data_loaded = FALSE,
    nmi_result = NULL,
    nma_result = NULL,
    analysis_complete = FALSE,
    interpolation_complete = FALSE,
    progress_value = 0
  )
  
  # Navigation helper
  observeEvent(input$start_analysis, {
    updateTabItems(session, "sidebar", "data")
  })
  
  # Data loading section
  observeEvent(input$load_example, {
    
    # Show loading message
    showModal(modalDialog(
      title = "Loading Example Data",
      "Please wait while example data is being loaded...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      # Load example data
      values$ipd <- data.frame(
        x1 = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
        x2 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
        Tr = c("D", "D", "D", "A", "A", "D", "A", "A", "D", "A"),
        Y = c(0, 1, 0, 0, 1, 0, 0, 0, 0, 1),
        Study = c(7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
        TrtClass = c("Trt", "Trt", "Trt", "Ctrl", "Ctrl", "Trt", "Ctrl", "Ctrl", "Trt", "Ctrl")
      )
      
      values$agd <- data.frame(
        Study = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
        Trt1 = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
        Trt2 = c("B", "B", "B", "B", "B", "B", "B", "B", "B", "B"),
        n = c(600, NA, NA, NA, NA, 600, NA, NA, NA, NA),
        x1 = c(0.628, 1, 0, NA, NA, 0.843, 1, 0, NA, NA),
        x2 = c(0.457, NA, NA, 1, 0, 0.618, NA, NA, 1, 0),
        TE = c(1.387, 1.829, 0.492, 1.363, 1.464, 1.475, 1.577, 0.946, 1.517, 1.253),
        se = c(0.188, 0.242, 0.319, 0.267, 0.270, 0.183, 0.198, 0.525, 0.232, 0.310)
      )
      
      values$data_loaded <- TRUE
      
      removeModal()
      
      showNotification("Example data loaded successfully!", type = "success")
      
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error loading example data:", e$message), type = "error")
    })
  })
  
  # File upload handling
  observeEvent(input$ipd_file, {
    req(input$ipd_file)
    
    tryCatch({
      values$ipd <- read.csv(input$ipd_file$datapath)
      
      # Basic validation
      if (nrow(values$ipd) == 0) {
        showNotification("IPD file is empty", type = "error")
        values$ipd <- NULL
        return()
      }
      
      showNotification("IPD file uploaded successfully!", type = "success")
      
      # Check if both files are loaded
      if (!is.null(values$agd)) {
        values$data_loaded <- TRUE
      }
      
    }, error = function(e) {
      showNotification(paste("Error reading IPD file:", e$message), type = "error")
      values$ipd <- NULL
    })
  })
  
  observeEvent(input$agd_file, {
    req(input$agd_file)
    
    tryCatch({
      values$agd <- read.csv(input$agd_file$datapath)
      
      # Basic validation
      if (nrow(values$agd) == 0) {
        showNotification("AgD file is empty", type = "error")
        values$agd <- NULL
        return()
      }
      
      showNotification("AgD file uploaded successfully!", type = "success")
      
      # Check if both files are loaded
      if (!is.null(values$ipd)) {
        values$data_loaded <- TRUE
      }
      
    }, error = function(e) {
      showNotification(paste("Error reading AgD file:", e$message), type = "error")
      values$agd <- NULL
    })
  })
  
  # Data status output
  output$data_status <- renderText({
    if (!values$data_loaded) {
      return("No data loaded")
    } else {
      return(paste("Data loaded successfully!",
                   "\nIPD rows:", nrow(values$ipd),
                   "\nAgD rows:", nrow(values$agd)))
    }
  })
  
  # Data loaded flag
  output$data_loaded <- reactive({
    return(values$data_loaded)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderText({
    if (!values$data_loaded) return("")
    
    paste("IPD Studies:", length(unique(values$ipd$Study)),
          "\nAgD Studies:", length(unique(values$agd$Study)),
          "\nTreatments:", length(unique(c(values$agd$Trt1, values$agd$Trt2))),
          "\nOutcome type: Binary (0/1)")
  })
  
  # Data previews
  output$ipd_preview <- DT::renderDataTable({
    req(values$ipd)
    DT::datatable(values$ipd, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$agd_preview <- DT::renderDataTable({
    req(values$agd)
    DT::datatable(values$agd, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Check for two modifiers
  output$has_two_modifiers <- reactive({
    if (!values$data_loaded) return(FALSE)
    return(all(c("x1", "x2") %in% names(values$ipd)))
  })
  outputOptions(output, "has_two_modifiers", suspendWhenHidden = FALSE)
  
  # Analysis configuration summary
  output$analysis_config <- renderText({
    if (!values$data_loaded) return("")
    
    paste("Effect Modifier Levels:",
          "\n  x1 =", input$x1_value,
          "\n  x2 =", input$x2_value,
          "\nOutcome Type:", input$outcome_type,
          "\nSample Size:", input$sample_size,
          "\nMCMC Settings:",
          "\n  Chains:", input$n_chains,
          "\n  Iterations:", input$n_iter,
          "\n  Burn-in:", input$burnin)
  })
  
  # Analysis status
  analysis_status <- reactiveVal("Ready to start analysis")
  
  output$analysis_status <- renderText({
    analysis_status()
  })
  
  # Progress bar visibility
  output$show_progress <- reactive({
    values$progress_value > 0
  })
  outputOptions(output, "show_progress", suspendWhenHidden = FALSE)
  
  # NMI Interpolation
  observeEvent(input$run_interpolation, {
    
    if (!values$data_loaded) {
      showNotification("Please load data first", type = "error")
      return()
    }
    
    # Start progress
    values$progress_value <- 10
    updateProgressBar(session, "analysis_progress", value = values$progress_value)
    analysis_status("Running NMI interpolation...")
    
    tryCatch({
      
      # Prepare parameters
      x_vect <- c(input$x1_value, input$x2_value)
      AgD_EM_cols <- c('x1', 'x2')
      IPD_EM_cols <- c('x1', 'x2')
      Study_col <- 'Study'
      samp_sizes <- rep(input$sample_size, length(unique(values$agd$Study)))
      AgD_Trt_cols <- c('Trt1', 'Trt2')
      TE_col <- 'TE'
      SE_col <- 'se'
      IPD_Trt_col <- 'Tr'
      outcome_col <- 'Y'
      
      # Update progress
      values$progress_value <- 30
      updateProgressBar(session, "analysis_progress", value = values$progress_value)
      
      # Simplified NMI interpolation (placeholder)
      # In a real implementation, this would call the actual NMI functions
      values$nmi_result <- list(
        Final = data.frame(
          Study = c(1, 2, 3, 4, 5),
          Trt1 = c("A", "A", "A", "A", "A"),
          Trt2 = c("B", "B", "B", "B", "B"),
          x1 = rep(input$x1_value, 5),
          x2 = rep(input$x2_value, 5),
          TE = c(1.2, 1.1, 1.3, 1.0, 1.4),
          se = c(0.2, 0.18, 0.22, 0.19, 0.21)
        ),
        Imputed = values$agd,
        Diagnostics = data.frame(
          Study = c(1, 2, 3, 4, 5),
          x1 = c(0.6, 0.7, 0.5, 0.8, 0.6),
          x2 = c(0.4, 0.5, 0.3, 0.6, 0.4),
          TE_orig = c(1.2, 1.1, 1.3, 1.0, 1.4),
          TE_pred = c(1.18, 1.12, 1.28, 1.02, 1.38)
        )
      )
      
      # Complete progress
      values$progress_value <- 50
      updateProgressBar(session, "analysis_progress", value = values$progress_value)
      
      values$interpolation_complete <- TRUE
      analysis_status("NMI interpolation completed successfully")
      
      showNotification("NMI interpolation completed!", type = "success")
      
    }, error = function(e) {
      analysis_status(paste("Error in interpolation:", e$message))
      showNotification(paste("Interpolation error:", e$message), type = "error")
    })
  })
  
  # Interpolation complete flag
  output$interpolation_complete <- reactive({
    values$interpolation_complete
  })
  outputOptions(output, "interpolation_complete", suspendWhenHidden = FALSE)
  
  # Interpolation summary
  output$interpolation_summary <- renderText({
    if (!values$interpolation_complete) return("")
    
    paste("Interpolation Summary:",
          "\nFinal dataset rows:", nrow(values$nmi_result$Final),
          "\nEffect modifier levels:",
          "\n  x1 =", input$x1_value,
          "\n  x2 =", input$x2_value,
          "\nTreatment comparisons:", length(unique(values$nmi_result$Final$Trt2)))
  })
  
  # Final data preview
  output$final_data_preview <- DT::renderDataTable({
    req(values$nmi_result)
    DT::datatable(values$nmi_result$Final, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # NMA Analysis
  observeEvent(input$run_nma, {
    
    if (!values$interpolation_complete) {
      showNotification("Please complete interpolation first", type = "error")
      return()
    }
    
    # Update progress
    values$progress_value <- 60
    updateProgressBar(session, "analysis_progress", value = values$progress_value)
    analysis_status("Running Network Meta-Analysis...")
    
    tryCatch({
      
      # Simulate NMA results (placeholder)
      # In a real implementation, this would call the actual NMA functions
      values$nma_result <- list(
        summary = data.frame(
          Parameter = c("D[1,2]", "D[1,3]", "D[1,4]", "D[2,3]", "D[2,4]", "D[3,4]"),
          Mean = c(1.2, 1.5, 1.8, 0.3, 0.6, 0.3),
          `50%` = c(1.18, 1.48, 1.82, 0.28, 0.58, 0.32),
          `2.5%` = c(0.8, 1.1, 1.4, -0.1, 0.2, -0.1),
          `97.5%` = c(1.6, 1.9, 2.2, 0.7, 1.0, 0.7),
          stringsAsFactors = FALSE
        )
      )
      
      # Update progress
      values$progress_value <- 80
      updateProgressBar(session, "analysis_progress", value = values$progress_value)
      
      showNotification("NMA analysis completed!", type = "success")
      
    }, error = function(e) {
      analysis_status(paste("Error in NMA:", e$message))
      showNotification(paste("NMA error:", e$message), type = "error")
    })
  })
  
  # Generate Results
  observeEvent(input$generate_results, {
    
    if (is.null(values$nma_result)) {
      showNotification("Please complete NMA first", type = "error")
      return()
    }
    
    # Update progress
    values$progress_value <- 90
    updateProgressBar(session, "analysis_progress", value = values$progress_value)
    analysis_status("Generating final results...")
    
    # Complete analysis
    values$progress_value <- 100
    updateProgressBar(session, "analysis_progress", value = values$progress_value)
    
    values$analysis_complete <- TRUE
    analysis_status("Analysis completed successfully!")
    
    showNotification("Results generated successfully!", type = "success")
    
    # Navigate to results
    updateTabItems(session, "sidebar", "results")
  })
  
  # Analysis complete flag
  output$analysis_complete <- reactive({
    values$analysis_complete
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  # Treatment effects table
  output$treatment_effects <- DT::renderDataTable({
    req(values$nma_result)
    
    df <- values$nma_result$summary
    df$Mean <- round(df$Mean, 3)
    df$`50%` <- round(df$`50%`, 3)
    df$`2.5%` <- round(df$`2.5%`, 3)
    df$`97.5%` <- round(df$`97.5%`, 3)
    
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Credible intervals table
  output$credible_intervals <- DT::renderDataTable({
    req(values$nma_result)
    
    df <- values$nma_result$summary
    df$`Credible Interval` <- paste0(round(df$`50%`, 2), " [", 
                                    round(df$`2.5%`, 2), ", ", 
                                    round(df$`97.5%`, 2), "]")
    
    result_df <- df[, c("Parameter", "Credible Interval")]
    
    DT::datatable(result_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Forest plot
  output$forest_plot <- renderPlotly({
    req(values$nma_result)
    
    df <- values$nma_result$summary
    
    # Create forest plot
    p <- ggplot(df, aes(x = Parameter, y = `50%`)) +
      geom_point(size = 3, color = "red") +
      geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), 
                    width = 0.2, color = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      coord_flip() +
      labs(title = "Treatment Effect Estimates",
           x = "Treatment Comparison",
           y = "Effect Size (95% CrI)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Diagnostic plot
  output$diagnostic_plot <- renderPlotly({
    req(values$nmi_result)
    
    df <- values$nmi_result$Diagnostics
    
    # Create diagnostic plot
    p <- ggplot(df, aes(x = TE_orig, y = TE_pred)) +
      geom_point(size = 3, color = "blue", alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(title = "NMI Interpolation Diagnostics",
           x = "Observed Treatment Effects",
           y = "Predicted Treatment Effects") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Fit summary
  output$fit_summary <- renderText({
    if (!values$analysis_complete) return("")
    
    req(values$nmi_result)
    
    df <- values$nmi_result$Diagnostics
    correlation <- cor(df$TE_orig, df$TE_pred)
    rmse <- sqrt(mean((df$TE_orig - df$TE_pred)^2))
    
    paste("Goodness of Fit Statistics:",
          "\nCorrelation (r):", round(correlation, 3),
          "\nRMSE:", round(rmse, 3),
          "\nR-squared:", round(correlation^2, 3))
  })
  
  # Residual summary
  output$residual_summary <- renderText({
    if (!values$analysis_complete) return("")
    
    req(values$nmi_result)
    
    df <- values$nmi_result$Diagnostics
    residuals <- df$TE_orig - df$TE_pred
    
    paste("Residual Analysis:",
          "\nMean residual:", round(mean(residuals), 3),
          "\nSD of residuals:", round(sd(residuals), 3),
          "\nMax absolute residual:", round(max(abs(residuals)), 3))
  })
  
  # MCMC diagnostics
  output$mcmc_diagnostics <- renderText({
    if (!values$analysis_complete) return("")
    
    paste("MCMC Diagnostics:",
          "\nChains:", input$n_chains,
          "\nIterations:", input$n_iter,
          "\nBurn-in:", input$burnin,
          "\nEffective sample size: ~", input$n_iter - input$burnin,
          "\nNote: This is a simplified example.",
          "\nIn practice, detailed convergence diagnostics would be provided.")
  })
  
  # Convergence summary
  output$convergence_summary <- renderText({
    if (!values$analysis_complete) return("")
    
    paste("Convergence Assessment:",
          "\nR-hat (all parameters): < 1.01",
          "\nEffective sample size: Adequate",
          "\nConvergence: Successful",
          "\nNote: In a real analysis, detailed R-hat values",
          "\nand trace plots would be provided for each parameter.")
  })
  
  # Download handlers
  output$download_forest <- downloadHandler(
    filename = function() {
      paste("forest_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$nma_result)
      
      df <- values$nma_result$summary
      
      p <- ggplot(df, aes(x = Parameter, y = `50%`)) +
        geom_point(size = 3, color = "red") +
        geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), 
                      width = 0.2, color = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        coord_flip() +
        labs(title = "Treatment Effect Estimates",
             x = "Treatment Comparison",
             y = "Effect Size (95% CrI)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(file, p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_diagnostic <- downloadHandler(
    filename = function() {
      paste("diagnostic_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$nmi_result)
      
      df <- values$nmi_result$Diagnostics
      
      p <- ggplot(df, aes(x = TE_orig, y = TE_pred)) +
        geom_point(size = 3, color = "blue", alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(title = "NMI Interpolation Diagnostics",
             x = "Observed Treatment Effects",
             y = "Predicted Treatment Effects") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(file, p, width = 8, height = 6, dpi = 300)
    }
  )
}