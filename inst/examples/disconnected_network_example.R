# Example: Handling Disconnected Networks in NMI
# Author: Ahmad Sofi-Mahmudi
# Date: 2025-01-21
# Part of NMI v1.3.0 Network Extensions

# Installation (if not already installed):
# devtools::install_github("choxos/nmi", build_vignettes = TRUE)

library(nmi)

cat("=== Disconnected Network Handling Example ===\n")

# Example: Disconnected Drug Network
# ==================================

# Scenario: We have two separate treatment networks:
# Component 1: Diabetes drugs (Metformin, Insulin, GLP-1)
# Component 2: Blood pressure drugs (ACE-I, ARB, Diuretic)
# These were never compared head-to-head in studies

set.seed(2025)

# 1. Create Example Data with Disconnected Network
# ------------------------------------------------

# Individual Patient Data (patients on various treatments)
IPD_disconnected <- data.frame(
  Study = rep("RWD_Study", 300),
  age = rnorm(300, mean = 65, sd = 12),
  bmi = rnorm(300, mean = 28, sd = 5),
  treatment = sample(c("Metformin", "Insulin", "ACE_I", "ARB"), 300, replace = TRUE),
  outcome = rbinom(300, 1, 0.3)
)

# Add treatment-specific effects
treatment_effects <- data.frame(
  treatment = c("Metformin", "Insulin", "ACE_I", "ARB"),
  base_effect = c(0.2, 0.5, 0.1, 0.3),
  age_interaction = c(0.01, 0.02, 0.005, 0.015)
)

for (i in 1:nrow(IPD_disconnected)) {
  trt <- IPD_disconnected$treatment[i]
  age_centered <- (IPD_disconnected$age[i] - 65) / 10
  
  base_eff <- treatment_effects$base_effect[treatment_effects$treatment == trt]
  age_eff <- treatment_effects$age_interaction[treatment_effects$treatment == trt] * age_centered
  
  prob <- plogis(-1 + base_eff + age_eff)
  IPD_disconnected$outcome[i] <- rbinom(1, 1, prob)
}

cat("IPD created with", nrow(IPD_disconnected), "patients\n")
cat("Treatments:", paste(unique(IPD_disconnected$treatment), collapse = ", "), "\n")

# Aggregate Data (separate networks)
AgD_disconnected <- data.frame(
  Study = paste0("Study_", 1:6),
  Trt1 = c("Metformin", "Insulin", "Metformin", "ACE_I", "ARB", "ACE_I"),
  Trt2 = c("Insulin", "GLP1", "GLP1", "ARB", "Diuretic", "Diuretic"),
  age_mean = c(60, 65, 62, 70, 68, 72),
  age_sd = c(10, 12, 11, 8, 9, 7),
  sample_size = c(200, 180, 220, 160, 170, 150),
  TE = c(0.3, 0.2, 0.5, 0.15, 0.1, 0.25),
  se = c(0.12, 0.15, 0.11, 0.18, 0.16, 0.14)
)

cat("AgD created with", nrow(AgD_disconnected), "studies\n")
cat("Comparisons include two separate networks\n")

# 2. Analyze Network Connectivity
# -------------------------------

cat("\n=== NETWORK CONNECTIVITY ANALYSIS ===\n")

connectivity <- detect_network_connectivity(
  AgD_disconnected, 
  trt_cols = c("Trt1", "Trt2"), 
  study_col = "Study"
)

cat("Network Status:\n")
cat("  Connected:", connectivity$is_connected, "\n")
cat("  Components:", connectivity$n_components, "\n")
cat("  Treatments per component:\n")

for (i in seq_along(connectivity$components)) {
  cat("    Component", i, ":", paste(connectivity$components[[i]], collapse = ", "), "\n")
}

cat("  Connectivity ratio:", round(connectivity$connectivity_ratio, 3), "\n")
cat("  Bridges found:", nrow(connectivity$bridges), "\n")
cat("  Articulation points:", length(connectivity$articulation_points), "\n")

# 3. Handle Disconnected Network - Component-wise Strategy
# --------------------------------------------------------

cat("\n=== COMPONENT-WISE ANALYSIS ===\n")

target_age <- 67
x_vect <- c(age = target_age)

results_componentwise <- handle_disconnected_network(
  IPD = IPD_disconnected,
  AgD = AgD_disconnected,
  x_vect = x_vect,
  AgD_EM_cols = "age_mean",
  IPD_EM_cols = "age",
  trt_cols = c("Trt1", "Trt2"),
  study_col = "Study",
  strategy = "component_wise"
)

cat("Strategy used:", results_componentwise$strategy_used, "\n")
cat("Components analyzed:", length(results_componentwise$components), "\n")

if (!is.null(results_componentwise$summary)) {
  cat("Success rate:", round(results_componentwise$summary$success_rate * 100, 1), "%\n")
}

# Display results for each component
for (comp_name in names(results_componentwise$components)) {
  comp_result <- results_componentwise$components[[comp_name]]
  cat("\n", comp_name, ":\n")
  cat("  Treatments:", paste(comp_result$treatments, collapse = ", "), "\n")
  cat("  Comparisons:", comp_result$n_comparisons, "\n")
  
  if (!is.null(comp_result$result)) {
    cat("  Analysis: SUCCESS\n")
    cat("  Interpolated studies:", nrow(comp_result$result$Final), "\n")
  } else {
    cat("  Analysis: FAILED -", comp_result$error, "\n")
  }
}

# 4. Handle Disconnected Network - Bridge Augmentation Strategy
# ------------------------------------------------------------

cat("\n=== BRIDGE AUGMENTATION ANALYSIS ===\n")

results_bridge <- handle_disconnected_network(
  IPD = IPD_disconnected,
  AgD = AgD_disconnected,
  x_vect = x_vect,
  AgD_EM_cols = "age_mean",
  IPD_EM_cols = "age",
  trt_cols = c("Trt1", "Trt2"),
  study_col = "Study",
  strategy = "bridge_augment"
)

cat("Strategy used:", results_bridge$strategy_used, "\n")

if (results_bridge$strategy_used == "bridge_augment") {
  cat("Bridges added:", length(results_bridge$bridges_added), "\n")
  cat("Original comparisons:", results_bridge$original_comparisons, "\n")
  cat("Augmented comparisons:", results_bridge$augmented_comparisons, "\n")
} else {
  cat("Bridge augmentation not possible - fell back to component-wise\n")
}

# 5. Visualization of Network Structure
# ------------------------------------

cat("\n=== NETWORK VISUALIZATION ===\n")

# Create a simple network visualization function
visualize_network_structure <- function(connectivity) {
  cat("Network Structure:\n")
  
  # Display adjacency matrix
  cat("Adjacency Matrix:\n")
  print(connectivity$adj_matrix)
  
  # Show component structure
  cat("\nComponent Structure:\n")
  for (i in seq_along(connectivity$components)) {
    treatments <- connectivity$components[[i]]
    cat("Component", i, ":", paste(treatments, collapse = " -- "), "\n")
  }
  
  # Highlight critical elements
  if (length(connectivity$bridges) > 0) {
    cat("\nCritical Bridges:\n")
    for (i in 1:nrow(connectivity$bridges)) {
      cat(" ", connectivity$bridges$from[i], "<->", connectivity$bridges$to[i], "\n")
    }
  }
  
  if (length(connectivity$articulation_points) > 0) {
    cat("\nArticulation Points:", paste(connectivity$articulation_points, collapse = ", "), "\n")
  }
}

visualize_network_structure(connectivity)

# 6. Practical Recommendations
# ----------------------------

cat("\n=== PRACTICAL RECOMMENDATIONS ===\n")

generate_recommendations <- function(connectivity, results) {
  cat("Based on the network analysis:\n\n")
  
  if (connectivity$is_connected) {
    cat("✅ CONNECTED NETWORK\n")
    cat("   → Use standard NMI analysis\n")
    cat("   → All treatments can be compared indirectly\n")
  } else {
    cat("⚠️  DISCONNECTED NETWORK (", connectivity$n_components, " components)\n")
    cat("   → Component-wise analysis recommended\n")
    cat("   → Consider seeking bridging studies\n")
    
    # Identify largest component
    largest_comp_id <- which.max(sapply(connectivity$components, length))
    largest_comp <- connectivity$components[[largest_comp_id]]
    
    cat("   → Largest component (", length(largest_comp), " treatments): ", 
        paste(largest_comp, collapse = ", "), "\n")
    
    # Suggest potential bridges
    all_treatments <- unique(unlist(connectivity$components))
    if (length(all_treatments) > length(largest_comp)) {
      isolated_treatments <- setdiff(all_treatments, largest_comp)
      cat("   → Consider studies comparing isolated treatments:\n")
      cat("     ", paste(isolated_treatments, collapse = ", "), "\n")
      cat("     with main network treatments:\n")
      cat("     ", paste(largest_comp, collapse = ", "), "\n")
    }
  }
  
  # Quality assessment
  if (length(connectivity$articulation_points) > 0) {
    cat("\n🔗 CRITICAL TREATMENTS (removal would disconnect network):\n")
    cat("   ", paste(connectivity$articulation_points, collapse = ", "), "\n")
    cat("   → Consider additional studies involving these treatments\n")
  }
  
  if (nrow(connectivity$bridges) > 0) {
    cat("\n🌉 CRITICAL COMPARISONS (removal would disconnect network):\n")
    for (i in 1:nrow(connectivity$bridges)) {
      cat("   ", connectivity$bridges$from[i], "vs", connectivity$bridges$to[i], "\n")
    }
    cat("   → These comparisons are essential for network connectivity\n")
  }
}

generate_recommendations(connectivity, results_componentwise)

# 7. Export Results
# ----------------

cat("\n=== SUMMARY ===\n")
cat("✅ Disconnected network handling demonstrated\n")
cat("✅ Component-wise analysis completed\n")
cat("✅ Bridge augmentation attempted\n")
cat("✅ Network structure analyzed\n")
cat("✅ Practical recommendations provided\n")

cat("\nKey Insights:\n")
cat("• Networks may be disconnected in practice\n")
cat("• Component-wise analysis provides valid results within each component\n")
cat("• Bridge studies or reference treatments can potentially connect components\n")
cat("• Critical treatments and comparisons should be prioritized for future research\n")

cat("\nExample completed successfully!\n") 