#' Single-Arm Study Integration for NMI
#' 
#' Functions for incorporating single-arm studies into network meta-analysis
#' Part of NMI v1.3.0 Network Extensions
#' 
#' @author Ahmad Sofi-Mahmudi
#' @email a.sofimahmudi@gmail.com

#' Detect Single-Arm Studies
#' 
#' Identifies single-arm studies in aggregate data and prepares them for integration
#' 
#' @param AgD Aggregate data containing treatment comparisons
#' @param trt_cols Character vector of treatment column names
#' @param study_col Character name of study identifier column
#' @param outcome_col Character name of outcome column (for single-arm studies)
#' @param n_col Character name of sample size column (for single-arm studies)
#' @return List containing single-arm study information and cleaned comparative data
#' 
#' @examples
#' \dontrun{
#' # Detect single-arm studies in dataset
#' single_arm_info <- detect_single_arm_studies(
#'   AgD, c("Trt1", "Trt2"), "Study", "Events", "N"
#' )
#' print(single_arm_info$n_single_arm)
#' }
#' 
#' @export
detect_single_arm_studies <- function(AgD, trt_cols, study_col, outcome_col = NULL, n_col = NULL) {
  
  # Validate inputs
  if (!all(trt_cols %in% colnames(AgD))) {
    stop("Treatment columns not found in AgD: ", paste(setdiff(trt_cols, colnames(AgD)), collapse = ", "))
  }
  
  if (!study_col %in% colnames(AgD)) {
    stop("Study column '", study_col, "' not found in AgD")
  }
  
  # Identify single-arm studies (rows where Trt1 == Trt2 or one treatment is NA/missing)
  single_arm_mask <- with(AgD, {
    (AgD[[trt_cols[1]]] == AgD[[trt_cols[2]]]) |
    is.na(AgD[[trt_cols[1]]]) | is.na(AgD[[trt_cols[2]]]) |
    AgD[[trt_cols[1]]] == "" | AgD[[trt_cols[2]]] == ""
  })
  
  # Split data
  single_arm_studies <- AgD[single_arm_mask, ]
  comparative_studies <- AgD[!single_arm_mask, ]
  
  # Process single-arm studies
  if (nrow(single_arm_studies) > 0) {
    # Identify the actual treatment for each single-arm study
    single_arm_studies$Treatment <- with(single_arm_studies, {
      ifelse(!is.na(.data[[trt_cols[1]]]) & .data[[trt_cols[1]]] != "", 
             .data[[trt_cols[1]]], .data[[trt_cols[2]]])
    })
    
    # Clean up missing or duplicate treatment entries
    single_arm_studies <- single_arm_studies[
      !is.na(single_arm_studies$Treatment) & single_arm_studies$Treatment != "", 
    ]
    
    # Add study-level information
    single_arm_summary <- single_arm_studies %>%
      group_by(.data[[study_col]], Treatment) %>%
      summarise(
        n_arms = n(),
        has_outcome = !is.null(outcome_col) && outcome_col %in% colnames(.),
        has_sample_size = !is.null(n_col) && n_col %in% colnames(.),
        .groups = "drop"
      )
  } else {
    single_arm_studies <- data.frame()
    single_arm_summary <- data.frame()
  }
  
  # Calculate summary statistics
  n_single_arm_studies <- length(unique(single_arm_studies[[study_col]]))
  n_comparative_studies <- length(unique(comparative_studies[[study_col]]))
  
  treatments_in_single_arm <- unique(single_arm_studies$Treatment)
  treatments_in_comparative <- unique(c(comparative_studies[[trt_cols[1]]], 
                                      comparative_studies[[trt_cols[2]]]))
  
  overlapping_treatments <- intersect(treatments_in_single_arm, treatments_in_comparative)
  unique_single_arm_treatments <- setdiff(treatments_in_single_arm, treatments_in_comparative)
  
  result <- list(
    single_arm_studies = single_arm_studies,
    comparative_studies = comparative_studies,
    single_arm_summary = single_arm_summary,
    n_single_arm = n_single_arm_studies,
    n_comparative = n_comparative_studies,
    treatments_single_arm = treatments_in_single_arm,
    treatments_comparative = treatments_in_comparative,
    overlapping_treatments = overlapping_treatments,
    unique_single_arm_treatments = unique_single_arm_treatments,
    integration_feasible = length(overlapping_treatments) > 0
  )
  
  class(result) <- "nmi_single_arm_detection"
  return(result)
}

#' Integrate Single-Arm Studies via Reference Treatment
#' 
#' Integrates single-arm studies into comparative network using a reference treatment approach
#' 
#' @param AgD Aggregate data containing both comparative and single-arm studies
#' @param single_arm_info Output from detect_single_arm_studies()
#' @param reference_treatment Name of reference treatment for integration
#' @param integration_method Method for integration: "reference_connect", "outcome_model", "network_bridge"
#' @param outcome_type Type of outcome: "binary", "continuous", "count"
#' @param ... Additional arguments for specific integration methods
#' @return List containing integrated network data and integration diagnostics
#' 
#' @examples
#' \dontrun{
#' # Integrate single-arm studies using reference treatment
#' integrated_data <- integrate_single_arm_studies(
#'   AgD = AgD_with_single_arm,
#'   single_arm_info = single_arm_detection,
#'   reference_treatment = "Placebo",
#'   integration_method = "reference_connect",
#'   outcome_type = "binary"
#' )
#' }
#' 
#' @export
integrate_single_arm_studies <- function(AgD, single_arm_info, reference_treatment,
                                        integration_method = "reference_connect",
                                        outcome_type = "binary", ...) {
  
  # Validate inputs
  if (!inherits(single_arm_info, "nmi_single_arm_detection")) {
    stop("single_arm_info must be output from detect_single_arm_studies()")
  }
  
  if (!single_arm_info$integration_feasible) {
    warning("Integration not feasible: no overlapping treatments between single-arm and comparative studies")
    return(list(
      success = FALSE,
      message = "No overlapping treatments found",
      original_data = AgD
    ))
  }
  
  if (!reference_treatment %in% single_arm_info$overlapping_treatments) {
    available_refs <- paste(single_arm_info$overlapping_treatments, collapse = ", ")
    stop("Reference treatment '", reference_treatment, 
         "' not available in both study types. Available: ", available_refs)
  }
  
  message("Integrating ", single_arm_info$n_single_arm, " single-arm studies using ", integration_method)
  message("Reference treatment: ", reference_treatment)
  
  # Dispatch to specific integration method
  switch(integration_method,
    "reference_connect" = integrate_via_reference_connect(AgD, single_arm_info, reference_treatment, outcome_type, ...),
    "outcome_model" = integrate_via_outcome_model(AgD, single_arm_info, reference_treatment, outcome_type, ...),
    "network_bridge" = integrate_via_network_bridge(AgD, single_arm_info, reference_treatment, outcome_type, ...),
    stop("Unknown integration method: ", integration_method, 
         ". Available: 'reference_connect', 'outcome_model', 'network_bridge'")
  )
}

#' Reference Connection Integration Method
#' 
#' Creates pseudo-comparisons between single-arm studies and reference treatment
#' 
#' @param AgD Original aggregate data
#' @param single_arm_info Single-arm study information
#' @param reference_treatment Reference treatment name
#' @param outcome_type Outcome type
#' @param external_reference_data External data for reference treatment (optional)
#' @param uncertainty_factor Uncertainty inflation factor for pseudo-comparisons
#' @return List with integrated data and diagnostics
integrate_via_reference_connect <- function(AgD, single_arm_info, reference_treatment, 
                                          outcome_type, external_reference_data = NULL,
                                          uncertainty_factor = 1.5) {
  
  # Get reference treatment data
  if (!is.null(external_reference_data)) {
    ref_data <- external_reference_data
  } else {
    # Extract reference treatment data from existing comparative studies
    ref_data <- extract_reference_treatment_data(single_arm_info$comparative_studies, 
                                                reference_treatment, outcome_type)
  }
  
  if (nrow(ref_data) == 0) {
    stop("No reference treatment data available for '", reference_treatment, "'")
  }
  
  # Create pseudo-comparisons
  pseudo_comparisons <- create_pseudo_comparisons(
    single_arm_info$single_arm_studies,
    ref_data,
    reference_treatment,
    outcome_type,
    uncertainty_factor
  )
  
  # Combine with original comparative data
  integrated_AgD <- rbind(
    single_arm_info$comparative_studies,
    pseudo_comparisons
  )
  
  # Add integration diagnostics
  diagnostics <- list(
    method = "reference_connect",
    reference_treatment = reference_treatment,
    n_pseudo_comparisons = nrow(pseudo_comparisons),
    uncertainty_factor = uncertainty_factor,
    reference_studies_used = nrow(ref_data),
    treatments_integrated = unique(pseudo_comparisons$Trt1[pseudo_comparisons$Trt1 != reference_treatment])
  )
  
  return(list(
    success = TRUE,
    integrated_data = integrated_AgD,
    pseudo_comparisons = pseudo_comparisons,
    reference_data = ref_data,
    diagnostics = diagnostics,
    original_comparative = single_arm_info$comparative_studies,
    original_single_arm = single_arm_info$single_arm_studies
  ))
}

#' Outcome Model Integration Method
#' 
#' Uses outcome modeling to predict comparative effects from single-arm data
#' 
#' @param AgD Original aggregate data
#' @param single_arm_info Single-arm study information
#' @param reference_treatment Reference treatment name
#' @param outcome_type Outcome type
#' @param model_covariates Covariates for outcome modeling
#' @return List with integrated data and model diagnostics
integrate_via_outcome_model <- function(AgD, single_arm_info, reference_treatment, 
                                       outcome_type, model_covariates = NULL) {
  
  message("Outcome modeling integration not yet fully implemented")
  message("Falling back to reference connection method")
  
  # For now, fall back to reference connection
  # In full implementation, this would fit outcome models to predict comparative effects
  return(integrate_via_reference_connect(AgD, single_arm_info, reference_treatment, outcome_type))
}

#' Network Bridge Integration Method
#' 
#' Creates bridges between single-arm studies and network using matching
#' 
#' @param AgD Original aggregate data
#' @param single_arm_info Single-arm study information
#' @param reference_treatment Reference treatment name
#' @param outcome_type Outcome type
#' @param matching_variables Variables for matching studies
#' @return List with integrated data and matching diagnostics
integrate_via_network_bridge <- function(AgD, single_arm_info, reference_treatment, 
                                        outcome_type, matching_variables = NULL) {
  
  message("Network bridge integration not yet fully implemented")
  message("Falling back to reference connection method")
  
  # For now, fall back to reference connection
  # In full implementation, this would use propensity score matching or similar
  return(integrate_via_reference_connect(AgD, single_arm_info, reference_treatment, outcome_type))
}

#' Extract Reference Treatment Data
#' 
#' Extracts reference treatment arm data from comparative studies
#' 
#' @param comparative_data Comparative study data
#' @param reference_treatment Reference treatment name
#' @param outcome_type Outcome type
#' @return Data frame with reference treatment arm data
extract_reference_treatment_data <- function(comparative_data, reference_treatment, outcome_type) {
  
  if (nrow(comparative_data) == 0) {
    return(data.frame())
  }
  
  # Find studies with reference treatment in either arm
  ref_in_trt1 <- comparative_data$Trt1 == reference_treatment
  ref_in_trt2 <- comparative_data$Trt2 == reference_treatment
  
  ref_studies <- comparative_data[ref_in_trt1 | ref_in_trt2, ]
  
  if (nrow(ref_studies) == 0) {
    return(data.frame())
  }
  
  # Extract reference arm data (this is simplified - real implementation would be more sophisticated)
  # For now, use average effect size and standard error from studies containing reference treatment
  ref_summary <- ref_studies %>%
    summarise(
      n_studies = n(),
      mean_effect = mean(TE, na.rm = TRUE),
      mean_se = mean(se, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create reference data structure
  ref_data <- data.frame(
    Treatment = reference_treatment,
    TE_vs_internal = 0,  # Reference treatment vs itself
    se_vs_internal = ref_summary$mean_se * 0.5,  # Reduced uncertainty for internal reference
    n_studies = ref_summary$n_studies,
    source = "internal_comparative"
  )
  
  return(ref_data)
}

#' Create Pseudo-Comparisons
#' 
#' Creates pseudo-comparative data from single-arm studies and reference data
#' 
#' @param single_arm_data Single-arm study data
#' @param reference_data Reference treatment data
#' @param reference_treatment Reference treatment name
#' @param outcome_type Outcome type
#' @param uncertainty_factor Factor to inflate uncertainty for pseudo-comparisons
#' @return Data frame with pseudo-comparison data
create_pseudo_comparisons <- function(single_arm_data, reference_data, reference_treatment,
                                     outcome_type, uncertainty_factor = 1.5) {
  
  if (nrow(single_arm_data) == 0 || nrow(reference_data) == 0) {
    return(data.frame())
  }
  
  # Create pseudo-comparisons for each single-arm study
  pseudo_comparisons <- data.frame()
  
  for (i in 1:nrow(single_arm_data)) {
    study_data <- single_arm_data[i, ]
    treatment <- study_data$Treatment
    
    if (treatment == reference_treatment) next  # Skip if already reference treatment
    
    # Create pseudo-comparison
    # In real implementation, this would use sophisticated methods to estimate comparative effect
    # For now, use simple approach with inflated uncertainty
    
    # Estimate pseudo effect size (simplified)
    if ("TE" %in% colnames(study_data) && !is.na(study_data$TE)) {
      pseudo_te <- study_data$TE - reference_data$mean_effect[1]
    } else {
      pseudo_te <- 0  # Default when no direct effect estimate available
    }
    
    # Estimate pseudo standard error (inflated for uncertainty)
    if ("se" %in% colnames(study_data) && !is.na(study_data$se)) {
      pseudo_se <- study_data$se * uncertainty_factor
    } else {
      pseudo_se <- reference_data$se_vs_internal[1] * uncertainty_factor
    }
    
    # Create pseudo-comparison row
    pseudo_row <- data.frame(
      Study = paste0(study_data$Study, "_pseudo"),
      Trt1 = treatment,
      Trt2 = reference_treatment,
      TE = pseudo_te,
      se = pseudo_se,
      type = "pseudo_comparison",
      original_study = study_data$Study,
      stringsAsFactors = FALSE
    )
    
    # Copy over effect modifier columns if they exist
    em_cols <- setdiff(colnames(study_data), c("Study", "Treatment", "TE", "se", "type"))
    for (col in em_cols) {
      if (col %in% colnames(study_data)) {
        pseudo_row[[col]] <- study_data[[col]]
      }
    }
    
    pseudo_comparisons <- rbind(pseudo_comparisons, pseudo_row)
  }
  
  return(pseudo_comparisons)
}

#' Run NMI with Single-Arm Integration
#' 
#' Complete workflow for NMI analysis including single-arm study integration
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data (may include single-arm studies)
#' @param x_vect Target effect modifier values
#' @param AgD_EM_cols Effect modifier columns in AgD
#' @param IPD_EM_cols Effect modifier columns in IPD
#' @param trt_cols Treatment comparison columns
#' @param study_col Study identifier column
#' @param reference_treatment Reference treatment for single-arm integration
#' @param integration_method Method for single-arm integration
#' @param outcome_type Outcome type
#' @param auto_detect_single_arm Whether to automatically detect single-arm studies
#' @param ... Additional arguments passed to integration and NMI functions
#' 
#' @return List containing NMI results with single-arm integration details
#' 
#' @examples
#' \dontrun{
#' # Run NMI with automatic single-arm integration
#' results <- nmi_with_single_arm_integration(
#'   IPD = IPD, AgD = AgD_mixed,
#'   x_vect = c(age = 65), 
#'   AgD_EM_cols = "age_mean", IPD_EM_cols = "age",
#'   trt_cols = c("Trt1", "Trt2"), study_col = "Study",
#'   reference_treatment = "Placebo",
#'   integration_method = "reference_connect",
#'   outcome_type = "binary"
#' )
#' }
#' 
#' @export
nmi_with_single_arm_integration <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                           trt_cols, study_col, reference_treatment,
                                           integration_method = "reference_connect",
                                           outcome_type = "binary",
                                           auto_detect_single_arm = TRUE, ...) {
  
  message("=== NMI Analysis with Single-Arm Integration ===")
  
  # Step 1: Detect single-arm studies
  if (auto_detect_single_arm) {
    message("Step 1: Detecting single-arm studies...")
    single_arm_info <- detect_single_arm_studies(AgD, trt_cols, study_col)
    
    message("Found ", single_arm_info$n_single_arm, " single-arm studies and ", 
            single_arm_info$n_comparative, " comparative studies")
    
    if (single_arm_info$n_single_arm == 0) {
      message("No single-arm studies found. Proceeding with standard NMI analysis.")
      result <- NMI_interpolation(
        IPD = IPD, AgD = AgD, x_vect = x_vect,
        AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols,
        Study_col = study_col, ...
      )
      
      return(list(
        nmi_result = result,
        single_arm_integration = list(used = FALSE, reason = "No single-arm studies detected"),
        method = "standard_nmi"
      ))
    }
  } else {
    # Use all data as comparative
    single_arm_info <- list(
      n_single_arm = 0,
      comparative_studies = AgD,
      integration_feasible = FALSE
    )
  }
  
  # Step 2: Integrate single-arm studies if present
  if (single_arm_info$n_single_arm > 0 && single_arm_info$integration_feasible) {
    message("Step 2: Integrating single-arm studies...")
    
    integration_result <- integrate_single_arm_studies(
      AgD = AgD,
      single_arm_info = single_arm_info,
      reference_treatment = reference_treatment,
      integration_method = integration_method,
      outcome_type = outcome_type,
      ...
    )
    
    if (integration_result$success) {
      message("Integration successful. Created ", integration_result$diagnostics$n_pseudo_comparisons, 
              " pseudo-comparisons.")
      analysis_data <- integration_result$integrated_data
    } else {
      warning("Integration failed: ", integration_result$message)
      analysis_data <- single_arm_info$comparative_studies
      integration_result <- list(used = FALSE, reason = integration_result$message)
    }
  } else {
    message("Step 2: Skipping single-arm integration (not feasible or not requested)")
    analysis_data <- single_arm_info$comparative_studies
    integration_result <- list(used = FALSE, reason = "Integration not feasible")
  }
  
  # Step 3: Run NMI analysis on integrated data
  message("Step 3: Running NMI analysis on integrated network...")
  
  nmi_result <- NMI_interpolation(
    IPD = IPD, 
    AgD = analysis_data, 
    x_vect = x_vect,
    AgD_EM_cols = AgD_EM_cols, 
    IPD_EM_cols = IPD_EM_cols,
    Study_col = study_col, 
    ...
  )
  
  message("NMI analysis with single-arm integration completed!")
  
  # Return comprehensive results
  return(list(
    nmi_result = nmi_result,
    single_arm_integration = integration_result,
    single_arm_detection = single_arm_info,
    method = ifelse(integration_result$used, "nmi_with_single_arm", "standard_nmi"),
    analysis_data = analysis_data,
    original_data = AgD
  ))
}

#' Validate Single-Arm Integration
#' 
#' Validates and summarizes single-arm study integration results
#' 
#' @param integration_result Output from integrate_single_arm_studies()
#' @param connectivity_result Optional network connectivity analysis
#' @return List with validation results and recommendations
#' 
#' @export
validate_single_arm_integration <- function(integration_result, connectivity_result = NULL) {
  
  if (!integration_result$success) {
    return(list(
      valid = FALSE,
      issues = "Integration failed",
      recommendations = "Check reference treatment availability and data quality"
    ))
  }
  
  diagnostics <- integration_result$diagnostics
  issues <- character()
  recommendations <- character()
  
  # Check number of pseudo-comparisons
  if (diagnostics$n_pseudo_comparisons == 0) {
    issues <- c(issues, "No pseudo-comparisons created")
    recommendations <- c(recommendations, "Verify single-arm data and reference treatment selection")
  }
  
  if (diagnostics$n_pseudo_comparisons > 20) {
    issues <- c(issues, "Many pseudo-comparisons may inflate network connectivity")
    recommendations <- c(recommendations, "Consider sensitivity analysis with subset of single-arm studies")
  }
  
  # Check uncertainty inflation
  if (diagnostics$uncertainty_factor < 1.2) {
    issues <- c(issues, "Low uncertainty factor may underestimate pseudo-comparison uncertainty")
    recommendations <- c(recommendations, "Consider increasing uncertainty_factor to 1.5 or higher")
  }
  
  # Check treatment overlap
  if (length(diagnostics$treatments_integrated) > 5) {
    issues <- c(issues, "Many new treatments from single-arm studies")
    recommendations <- c(recommendations, "Verify clinical similarity of integrated treatments")
  }
  
  # Connectivity assessment
  if (!is.null(connectivity_result) && !connectivity_result$is_connected) {
    issues <- c(issues, "Network remains disconnected after single-arm integration")
    recommendations <- c(recommendations, "Consider additional bridging strategies or component-wise analysis")
  }
  
  # Overall validation
  valid <- length(issues) == 0
  
  return(list(
    valid = valid,
    issues = if (length(issues) > 0) issues else "No major issues detected",
    recommendations = if (length(recommendations) > 0) recommendations else "Integration appears successful",
    summary = list(
      n_pseudo_comparisons = diagnostics$n_pseudo_comparisons,
      n_treatments_added = length(diagnostics$treatments_integrated),
      uncertainty_factor = diagnostics$uncertainty_factor,
      reference_treatment = diagnostics$reference_treatment
    )
  ))
} 