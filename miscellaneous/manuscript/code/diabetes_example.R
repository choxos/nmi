# Diabetes Example Analysis for NMI Package Manuscript
# Author: Ahmad Sofi-Mahmudi
# Date: 2025-01-21

library(nmi)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

set.seed(2025)

# ====================================================================
# 1. CREATE REALISTIC DIABETES TREATMENT NETWORK DATA
# ====================================================================

#' Generate Diabetes IPD Dataset
generate_diabetes_ipd <- function() {
  # 4 IPD studies with different characteristics
  studies <- c("PRAGMATIC_TRIAL", "ACADEMIC_RCT", "REGISTRY_STUDY", "BIOMARKER_STUDY")
  
  ipd_data <- data.frame()
  
  for (study in studies) {
    if (study == "PRAGMATIC_TRIAL") {
      n_patients <- 1500
      treatments <- c("Metformin", "Sulfonylurea", "Placebo")
      age_mean <- 62
      hba1c_mean <- 8.2
    } else if (study == "ACADEMIC_RCT") {
      n_patients <- 800
      treatments <- c("Metformin", "DPP4_inhibitor", "Placebo")
      age_mean <- 58
      hba1c_mean <- 8.5
    } else if (study == "REGISTRY_STUDY") {
      n_patients <- 2000
      treatments <- c("GLP1_agonist", "SGLT2_inhibitor", "Insulin")
      age_mean <- 65
      hba1c_mean <- 9.1
    } else { # BIOMARKER_STUDY
      n_patients <- 547
      treatments <- c("Metformin", "Combination_therapy")
      age_mean <- 60
      hba1c_mean <- 8.0
    }
    
    # Generate patient characteristics
    age <- rnorm(n_patients, age_mean, 10)
    age <- pmax(18, pmin(80, age)) # Constrain to realistic range
    
    bmi <- rnorm(n_patients, 28, 5)
    bmi <- pmax(18, pmin(45, bmi))
    
    diabetes_duration <- rexp(n_patients, 1/8) # Mean 8 years
    diabetes_duration <- pmin(diabetes_duration, 30)
    
    baseline_hba1c <- rnorm(n_patients, hba1c_mean, 1.2)
    baseline_hba1c <- pmax(6.5, pmin(12, baseline_hba1c))
    
    sex <- rbinom(n_patients, 1, 0.45) # 45% female
    
    # Treatment assignment (realistic allocation)
    treatment <- sample(treatments, n_patients, replace = TRUE)
    
    # Generate outcomes based on realistic treatment effects
    # Effect modification by baseline HbA1c is key feature
    hba1c_reduction <- numeric(n_patients)
    
    for (i in 1:n_patients) {
      trt <- treatment[i]
      baseline <- baseline_hba1c[i]
      
      # Base effect (higher baseline = more reduction potential)
      baseline_effect <- max(0, (baseline - 7) * 0.3)
      
      # Treatment-specific effects
      if (trt == "Placebo") {
        treatment_effect <- 0.1 + 0.05 * (baseline - 7) # Minimal effect
      } else if (trt == "Metformin") {
        treatment_effect <- 0.7 + 0.15 * (baseline - 7) + rnorm(1, 0, 0.1)
      } else if (trt == "Sulfonylurea") {
        treatment_effect <- 0.8 + 0.12 * (baseline - 7) + rnorm(1, 0, 0.12)
      } else if (trt == "DPP4_inhibitor") {
        treatment_effect <- 0.6 + 0.10 * (baseline - 7) + rnorm(1, 0, 0.1)
      } else if (trt == "GLP1_agonist") {
        treatment_effect <- 1.2 + 0.18 * (baseline - 7) + rnorm(1, 0, 0.15)
      } else if (trt == "SGLT2_inhibitor") {
        treatment_effect <- 1.0 + 0.14 * (baseline - 7) + rnorm(1, 0, 0.13)
      } else if (trt == "Insulin") {
        treatment_effect <- 1.5 + 0.25 * (baseline - 7) + rnorm(1, 0, 0.2)
      } else { # Combination_therapy
        treatment_effect <- 1.4 + 0.20 * (baseline - 7) + rnorm(1, 0, 0.18)
      }
      
      # Patient-specific modifiers
      age_modifier <- -0.01 * (age[i] - 60) # Slight decrease with age
      bmi_modifier <- 0.02 * (bmi[i] - 28) # Slight increase with BMI
      
      total_effect <- treatment_effect + age_modifier + bmi_modifier
      
      # Add random variation
      hba1c_reduction[i] <- max(0, total_effect + rnorm(1, 0, 0.3))
    }
    
    study_data <- data.frame(
      Study = study,
      age = age,
      bmi = bmi,
      diabetes_duration = diabetes_duration,
      baseline_hba1c = baseline_hba1c,
      sex = sex,
      treatment = treatment,
      hba1c_reduction = hba1c_reduction,
      stringsAsFactors = FALSE
    )
    
    ipd_data <- rbind(ipd_data, study_data)
  }
  
  return(ipd_data)
}

#' Generate Diabetes AgD Dataset
generate_diabetes_agd <- function() {
  # Define all treatments in network
  treatments <- c("Metformin", "Sulfonylurea", "DPP4_inhibitor", "GLP1_agonist", 
                 "SGLT2_inhibitor", "Insulin", "Combination_therapy", "Placebo")
  
  # Create network of comparisons
  comparisons <- data.frame(
    Study = paste0("AgD_Study_", sprintf("%02d", 1:19)),
    Trt1 = c("Metformin", "Sulfonylurea", "DPP4_inhibitor", "GLP1_agonist", 
             "SGLT2_inhibitor", "Insulin", "Metformin", "Sulfonylurea",
             "DPP4_inhibitor", "GLP1_agonist", "SGLT2_inhibitor", "Metformin",
             "GLP1_agonist", "SGLT2_inhibitor", "Insulin", "Combination_therapy",
             "Combination_therapy", "Metformin", "GLP1_agonist"),
    Trt2 = c("Placebo", "Placebo", "Placebo", "Placebo", 
             "Placebo", "Placebo", "Sulfonylurea", "DPP4_inhibitor",
             "GLP1_agonist", "SGLT2_inhibitor", "Insulin", "DPP4_inhibitor",
             "Insulin", "Combination_therapy", "Combination_therapy", "Metformin",
             "GLP1_agonist", "SGLT2_inhibitor", "Combination_therapy"),
    stringsAsFactors = FALSE
  )
  
  agd_data <- data.frame()
  
  for (i in 1:nrow(comparisons)) {
    study <- comparisons$Study[i]
    trt1 <- comparisons$Trt1[i]
    trt2 <- comparisons$Trt2[i]
    
    # Study characteristics (with some variation)
    n_total <- sample(80:400, 1)
    age_mean <- rnorm(1, 62, 8)
    age_mean <- pmax(45, pmin(75, age_mean))
    
    baseline_hba1c_mean <- rnorm(1, 8.3, 1.0)
    baseline_hba1c_mean <- pmax(7.0, pmin(11.0, baseline_hba1c_mean))
    
    bmi_mean <- rnorm(1, 29, 3)
    bmi_mean <- pmax(22, pmin(38, bmi_mean))
    
    sex_prop <- runif(1, 0.35, 0.65)
    diabetes_duration_mean <- rexp(1, 1/10)
    diabetes_duration_mean <- pmin(diabetes_duration_mean, 25)
    
    # Calculate treatment effects based on study characteristics
    baseline_for_calc <- baseline_hba1c_mean
    
    # Treatment 1 effect
    if (trt1 == "Placebo") {
      eff1 <- 0.1 + 0.05 * (baseline_for_calc - 7)
    } else if (trt1 == "Metformin") {
      eff1 <- 0.7 + 0.15 * (baseline_for_calc - 7)
    } else if (trt1 == "Sulfonylurea") {
      eff1 <- 0.8 + 0.12 * (baseline_for_calc - 7)
    } else if (trt1 == "DPP4_inhibitor") {
      eff1 <- 0.6 + 0.10 * (baseline_for_calc - 7)
    } else if (trt1 == "GLP1_agonist") {
      eff1 <- 1.2 + 0.18 * (baseline_for_calc - 7)
    } else if (trt1 == "SGLT2_inhibitor") {
      eff1 <- 1.0 + 0.14 * (baseline_for_calc - 7)
    } else if (trt1 == "Insulin") {
      eff1 <- 1.5 + 0.25 * (baseline_for_calc - 7)
    } else { # Combination_therapy
      eff1 <- 1.4 + 0.20 * (baseline_for_calc - 7)
    }
    
    # Treatment 2 effect
    if (trt2 == "Placebo") {
      eff2 <- 0.1 + 0.05 * (baseline_for_calc - 7)
    } else if (trt2 == "Metformin") {
      eff2 <- 0.7 + 0.15 * (baseline_for_calc - 7)
    } else if (trt2 == "Sulfonylurea") {
      eff2 <- 0.8 + 0.12 * (baseline_for_calc - 7)
    } else if (trt2 == "DPP4_inhibitor") {
      eff2 <- 0.6 + 0.10 * (baseline_for_calc - 7)
    } else if (trt2 == "GLP1_agonist") {
      eff2 <- 1.2 + 0.18 * (baseline_for_calc - 7)
    } else if (trt2 == "SGLT2_inhibitor") {
      eff2 <- 1.0 + 0.14 * (baseline_for_calc - 7)
    } else if (trt2 == "Insulin") {
      eff2 <- 1.5 + 0.25 * (baseline_for_calc - 7)
    } else { # Combination_therapy
      eff2 <- 1.4 + 0.20 * (baseline_for_calc - 7)
    }
    
    # Treatment effect (difference)
    true_te <- eff1 - eff2
    
    # Add study-level variation
    study_variation <- rnorm(1, 0, 0.1)
    observed_te <- true_te + study_variation
    
    # Standard error (decreases with sample size)
    se <- runif(1, 0.08, 0.20) * sqrt(100 / n_total)
    
    study_data <- data.frame(
      Study = study,
      Trt1 = trt1,
      Trt2 = trt2,
      age_mean = age_mean,
      age_sd = runif(1, 8, 15),
      baseline_hba1c_mean = baseline_hba1c_mean,
      baseline_hba1c_sd = runif(1, 0.8, 1.5),
      bmi_mean = bmi_mean,
      bmi_sd = runif(1, 3, 6),
      sex_prop = sex_prop,
      diabetes_duration_mean = diabetes_duration_mean,
      n = n_total,
      TE = observed_te,
      se = se,
      true_TE = true_te,
      stringsAsFactors = FALSE
    )
    
    agd_data <- rbind(agd_data, study_data)
  }
  
  return(agd_data)
}

#' Introduce Realistic Missing Data
introduce_missing_data <- function(agd_data, missing_percent = 15) {
  n_studies <- nrow(agd_data)
  n_missing <- round(n_studies * missing_percent / 100)
  
  # Missing data more likely in older studies (proxy: higher study number)
  study_nums <- as.numeric(gsub("AgD_Study_", "", agd_data$Study))
  prob_missing <- 0.05 + 0.3 * (study_nums > median(study_nums))
  
  missing_indices <- which(runif(n_studies) < prob_missing)
  
  # Adjust to target percentage
  if (length(missing_indices) > n_missing) {
    missing_indices <- sample(missing_indices, n_missing)
  } else if (length(missing_indices) < n_missing) {
    additional_missing <- sample(setdiff(1:n_studies, missing_indices), 
                                n_missing - length(missing_indices))
    missing_indices <- c(missing_indices, additional_missing)
  }
  
  # Make baseline HbA1c missing
  agd_data$baseline_hba1c_mean[missing_indices] <- NA
  agd_data$baseline_hba1c_sd[missing_indices] <- NA
  
  return(agd_data)
}

# ====================================================================
# 2. GENERATE DATASETS
# ====================================================================

cat("Generating diabetes treatment network datasets...\n")

# Generate IPD and AgD
diabetes_ipd <- generate_diabetes_ipd()
diabetes_agd <- generate_diabetes_agd()

# Introduce missing data
diabetes_agd_missing <- introduce_missing_data(diabetes_agd, 15)

cat("Dataset generation completed:\n")
cat("  IPD studies:", length(unique(diabetes_ipd$Study)), "\n")
cat("  IPD patients:", nrow(diabetes_ipd), "\n")
cat("  AgD studies:", nrow(diabetes_agd_missing), "\n")
cat("  Missing baseline HbA1c:", sum(is.na(diabetes_agd_missing$baseline_hba1c_mean)), "studies\n")

# ====================================================================
# 3. NETWORK CONNECTIVITY ANALYSIS
# ====================================================================

cat("\n=== NETWORK CONNECTIVITY ANALYSIS ===\n")

connectivity <- detect_network_connectivity(
  diabetes_agd, 
  trt_cols = c("Trt1", "Trt2"), 
  study_col = "Study"
)

cat("Network connectivity results:\n")
cat("  Connected:", connectivity$is_connected, "\n")
cat("  Number of treatments:", length(connectivity$treatments), "\n")
cat("  Treatments:", paste(connectivity$treatments, collapse = ", "), "\n")

if (connectivity$is_connected) {
  cat("  ✅ Network is fully connected\n")
} else {
  cat("  ⚠️ Network has", connectivity$n_components, "components\n")
}

# ====================================================================
# 4. MISSING DATA ANALYSIS
# ====================================================================

cat("\n=== MISSING DATA ANALYSIS ===\n")

missing_analysis <- detect_missing_patterns(
  diabetes_ipd, 
  diabetes_agd_missing, 
  c("baseline_hba1c"), 
  "hba1c_reduction"
)

cat("Missing data summary:\n")
cat("  IPD completeness:", round(missing_analysis$ipd_analysis$completeness_rate * 100, 1), "%\n")
cat("  AgD completeness:", round(missing_analysis$agd_analysis$study_completeness_rate * 100, 1), "%\n")
cat("  Recommended strategy:", missing_analysis$strategy$strategy_level, "\n")
cat("  Reason:", missing_analysis$strategy$reason, "\n")

# ====================================================================
# 5. NMI ANALYSIS WITH MISSING DATA IMPUTATION
# ====================================================================

cat("\n=== NMI ANALYSIS WITH MISSING DATA IMPUTATION ===\n")

# Target HbA1c levels for analysis
target_hba1c_levels <- c(7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0)

nmi_results <- list()

for (target_hba1c in target_hba1c_levels) {
  cat("Analyzing target HbA1c:", target_hba1c, "%\n")
  
  x_vect <- c(baseline_hba1c = target_hba1c)
  
  # Run NMI with ML imputation
  result <- nmi_with_ml_imputation(
    IPD = diabetes_ipd,
    AgD = diabetes_agd_missing,
    x_vect = x_vect,
    AgD_EM_cols = "baseline_hba1c_mean",
    IPD_EM_cols = "baseline_hba1c",
    imputation_method = "random_forest",
    n_imputations = 5
  )
  
  nmi_results[[as.character(target_hba1c)]] <- result
}

cat("NMI analysis completed for all target HbA1c levels\n")

# ====================================================================
# 6. TRADITIONAL NMA COMPARISON
# ====================================================================

cat("\n=== TRADITIONAL NMA COMPARISON ===\n")

# Run standard NMA (ignoring effect modification)
traditional_nma <- nmi_standard_nma(
  AgD = diabetes_agd_missing[complete.cases(diabetes_agd_missing), ],
  Trt_col_1 = "Trt1",
  Trt_col_2 = "Trt2",
  TE_col = "TE",
  se_col = "se"
)

cat("Traditional NMA completed\n")

# ====================================================================
# 7. RESULTS ANALYSIS AND VISUALIZATION
# ====================================================================

cat("\n=== RESULTS ANALYSIS ===\n")

# Extract treatment rankings across HbA1c levels
extract_rankings <- function(nmi_results_list) {
  rankings_data <- data.frame()
  
  for (hba1c_level in names(nmi_results_list)) {
    result <- nmi_results_list[[hba1c_level]]
    
    if (!is.null(result$pooled_result$Final)) {
      estimates <- result$pooled_result$Final
      
      # Calculate treatment effects vs placebo
      placebo_studies <- estimates[grepl("Placebo", estimates$Study), ]
      
      if (nrow(placebo_studies) > 0) {
        # Rank treatments by estimated effect
        estimates$ranking <- rank(-estimates$TE)
        
        summary_data <- data.frame(
          hba1c_level = as.numeric(hba1c_level),
          treatment = estimates$Trt1, # Assuming Trt1 is the active treatment
          effect_estimate = estimates$TE,
          se = estimates$se,
          ranking = estimates$ranking
        )
        
        rankings_data <- rbind(rankings_data, summary_data)
      }
    }
  }
  
  return(rankings_data)
}

rankings <- extract_rankings(nmi_results)

# Print key findings
if (nrow(rankings) > 0) {
  cat("\nKey findings across HbA1c levels:\n")
  
  for (hba1c in target_hba1c_levels) {
    level_data <- rankings[rankings$hba1c_level == hba1c, ]
    if (nrow(level_data) > 0) {
      top_treatment <- level_data[level_data$ranking == 1, ]
      if (nrow(top_treatment) > 0) {
        cat("  HbA1c", hba1c, "%: Best treatment =", top_treatment$treatment[1], 
            "(effect =", round(top_treatment$effect_estimate[1], 2), ")\n")
      }
    }
  }
}

# ====================================================================
# 8. SAVE RESULTS AND GENERATE REPORT
# ====================================================================

cat("\n=== SAVING RESULTS ===\n")

# Save all results
save(diabetes_ipd, diabetes_agd, diabetes_agd_missing, 
     connectivity, missing_analysis, nmi_results, traditional_nma, rankings,
     file = "diabetes_example_results.RData")

cat("All results saved to diabetes_example_results.RData\n")

# Generate summary table
create_summary_table <- function(rankings) {
  if (nrow(rankings) == 0) return(NULL)
  
  # Calculate mean effects and rankings by treatment
  summary_table <- rankings %>%
    group_by(treatment) %>%
    summarise(
      mean_effect = mean(effect_estimate, na.rm = TRUE),
      mean_ranking = mean(ranking, na.rm = TRUE),
      effect_range = paste(round(min(effect_estimate, na.rm = TRUE), 2), 
                          "to", 
                          round(max(effect_estimate, na.rm = TRUE), 2)),
      n_analyses = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_effect))
  
  return(summary_table)
}

summary_table <- create_summary_table(rankings)

if (!is.null(summary_table)) {
  cat("\nSummary of treatment effects across HbA1c levels:\n")
  print(summary_table)
}

cat("\n=== DIABETES EXAMPLE ANALYSIS COMPLETED ===\n")
cat("Results demonstrate effect modification by baseline HbA1c\n")
cat("Treatment rankings change substantially across the glycemic spectrum\n")
cat("NMI approach reveals patterns hidden by traditional NMA\n") 