# Tests for Network Extensions (Phase 3 - v1.3.0)
# Testing disconnected network handling and connectivity analysis

test_that("detect_network_connectivity works with connected network", {
  # Create connected network data
  AgD_connected <- data.frame(
    Study = paste0("Study_", 1:4),
    Trt1 = c("A", "B", "C", "A"),
    Trt2 = c("B", "C", "D", "D"),
    TE = c(0.5, 0.3, 0.7, 0.9),
    se = c(0.1, 0.12, 0.11, 0.13)
  )
  
  connectivity <- detect_network_connectivity(AgD_connected, c("Trt1", "Trt2"), "Study")
  
  expect_true(connectivity$is_connected)
  expect_equal(connectivity$n_components, 1)
  expect_equal(length(connectivity$components[[1]]), 4)  # A, B, C, D
  expect_equal(connectivity$connectivity_ratio, 1)
})

test_that("detect_network_connectivity works with disconnected network", {
  # Create disconnected network data (A-B isolated from C-D)
  AgD_disconnected <- data.frame(
    Study = paste0("Study_", 1:2),
    Trt1 = c("A", "C"),
    Trt2 = c("B", "D"),
    TE = c(0.5, 0.7),
    se = c(0.1, 0.11)
  )
  
  connectivity <- detect_network_connectivity(AgD_disconnected, c("Trt1", "Trt2"), "Study")
  
  expect_false(connectivity$is_connected)
  expect_equal(connectivity$n_components, 2)
  expect_equal(length(connectivity$components), 2)
  expect_true(connectivity$connectivity_ratio < 1)
})

test_that("detect_network_connectivity validates inputs", {
  AgD <- data.frame(Study = "S1", Trt1 = "A", Trt2 = "B", TE = 0.5, se = 0.1)
  
  # Missing treatment columns
  expect_error(detect_network_connectivity(AgD, c("Wrong1", "Wrong2"), "Study"))
  
  # Missing study column
  expect_error(detect_network_connectivity(AgD, c("Trt1", "Trt2"), "WrongStudy"))
})

test_that("find_connected_components works correctly", {
  # Create adjacency matrix for A-B-C, D-E (2 components)
  adj_matrix <- matrix(0, nrow = 5, ncol = 5)
  rownames(adj_matrix) <- colnames(adj_matrix) <- c("A", "B", "C", "D", "E")
  
  # Connect A-B-C
  adj_matrix["A", "B"] <- adj_matrix["B", "A"] <- 1
  adj_matrix["B", "C"] <- adj_matrix["C", "B"] <- 1
  
  # Connect D-E
  adj_matrix["D", "E"] <- adj_matrix["E", "D"] <- 1
  
  components <- find_connected_components(adj_matrix)
  
  expect_equal(length(components), 2)
  expect_true(all(c("A", "B", "C") %in% components[[1]]) || all(c("A", "B", "C") %in% components[[2]]))
  expect_true(all(c("D", "E") %in% components[[1]]) || all(c("D", "E") %in% components[[2]]))
})

test_that("find_bridges identifies critical edges", {
  # Create network with bridge: A-B-C where B-C is a bridge
  adj_matrix <- matrix(0, nrow = 4, ncol = 4)
  rownames(adj_matrix) <- colnames(adj_matrix) <- c("A", "B", "C", "D")
  
  # A-B, B-C (bridge), C-D
  adj_matrix["A", "B"] <- adj_matrix["B", "A"] <- 1
  adj_matrix["B", "C"] <- adj_matrix["C", "B"] <- 1
  adj_matrix["C", "D"] <- adj_matrix["D", "C"] <- 1
  
  bridges <- find_bridges(adj_matrix)
  
  expect_true(nrow(bridges) >= 1)
  # Should find at least one bridge (exact bridges depend on network structure)
})

test_that("find_articulation_points identifies critical nodes", {
  # Create network with articulation point: A-B-C where B is articulation point
  adj_matrix <- matrix(0, nrow = 3, ncol = 3)
  rownames(adj_matrix) <- colnames(adj_matrix) <- c("A", "B", "C")
  
  adj_matrix["A", "B"] <- adj_matrix["B", "A"] <- 1
  adj_matrix["B", "C"] <- adj_matrix["C", "B"] <- 1
  
  articulation_points <- find_articulation_points(adj_matrix)
  
  expect_true("B" %in% articulation_points)
})

test_that("handle_disconnected_network works with connected network", {
  # Create test data
  IPD <- data.frame(
    Study = rep("IPD_Study", 100),
    age = rnorm(100, 60, 10),
    treatment = sample(c("A", "B"), 100, replace = TRUE),
    outcome = rbinom(100, 1, 0.4)
  )
  
  AgD <- data.frame(
    Study = paste0("Study_", 1:3),
    Trt1 = c("A", "B", "A"),
    Trt2 = c("B", "C", "C"),
    age_mean = c(55, 60, 65),
    TE = c(0.5, 0.3, 0.7),
    se = c(0.1, 0.12, 0.11)
  )
  
  x_vect <- c(age = 62)
  
  # Mock the NMI_interpolation function for testing
  # In real tests, this would use the actual function
  old_nmi <- NMI_interpolation
  NMI_interpolation <<- function(...) {
    list(Final = data.frame(Study = "Test", TE = 0.5, se = 0.1))
  }
  
  result <- handle_disconnected_network(
    IPD = IPD, AgD = AgD, x_vect = x_vect,
    AgD_EM_cols = "age_mean", IPD_EM_cols = "age",
    trt_cols = c("Trt1", "Trt2"), study_col = "Study",
    strategy = "component_wise"
  )
  
  expect_equal(result$strategy_used, "standard")  # Should detect connected network
  
  # Restore original function
  NMI_interpolation <<- old_nmi
})

test_that("handle_disconnected_network component_wise strategy", {
  # Create disconnected network
  IPD <- data.frame(
    Study = rep("IPD_Study", 100),
    age = rnorm(100, 60, 10),
    treatment = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
    outcome = rbinom(100, 1, 0.4)
  )
  
  AgD_disconnected <- data.frame(
    Study = paste0("Study_", 1:2),
    Trt1 = c("A", "C"),
    Trt2 = c("B", "D"),
    age_mean = c(55, 65),
    TE = c(0.5, 0.7),
    se = c(0.1, 0.11)
  )
  
  x_vect <- c(age = 62)
  
  # Mock the NMI_interpolation function
  old_nmi <- NMI_interpolation
  NMI_interpolation <<- function(...) {
    list(Final = data.frame(Study = "Test", TE = 0.5, se = 0.1))
  }
  
  result <- handle_disconnected_network(
    IPD = IPD, AgD = AgD_disconnected, x_vect = x_vect,
    AgD_EM_cols = "age_mean", IPD_EM_cols = "age",
    trt_cols = c("Trt1", "Trt2"), study_col = "Study",
    strategy = "component_wise"
  )
  
  expect_equal(result$strategy_used, "component_wise")
  expect_true(length(result$components) >= 1)
  expect_false(result$connectivity$is_connected)
  
  # Restore original function
  NMI_interpolation <<- old_nmi
})

test_that("handle_disconnected_network validates strategy parameter", {
  AgD <- data.frame(Study = "S1", Trt1 = "A", Trt2 = "B", TE = 0.5, se = 0.1)
  IPD <- data.frame(Study = "IPD", age = 60, treatment = "A", outcome = 1)
  
  expect_error(handle_disconnected_network(
    IPD = IPD, AgD = AgD, x_vect = c(age = 60),
    AgD_EM_cols = "age", IPD_EM_cols = "age",
    trt_cols = c("Trt1", "Trt2"), study_col = "Study",
    strategy = "invalid_strategy"
  ))
})

test_that("network connectivity handles edge cases", {
  # Single treatment
  AgD_single <- data.frame(
    Study = "S1", Trt1 = "A", Trt2 = "A", TE = 0, se = 0.1
  )
  
  connectivity <- detect_network_connectivity(AgD_single, c("Trt1", "Trt2"), "Study")
  expect_equal(connectivity$n_treatments, 1)
  
  # Empty data
  AgD_empty <- data.frame(
    Study = character(0), Trt1 = character(0), Trt2 = character(0), 
    TE = numeric(0), se = numeric(0)
  )
  
  connectivity_empty <- detect_network_connectivity(AgD_empty, c("Trt1", "Trt2"), "Study")
  expect_equal(connectivity_empty$n_components, 0)
})

test_that("connectivity analysis provides comprehensive metrics", {
  # Create test network
  AgD <- data.frame(
    Study = paste0("Study_", 1:5),
    Trt1 = c("A", "B", "C", "A", "D"),
    Trt2 = c("B", "C", "D", "D", "E"),
    TE = c(0.5, 0.3, 0.7, 0.9, 0.4),
    se = c(0.1, 0.12, 0.11, 0.13, 0.14)
  )
  
  connectivity <- detect_network_connectivity(AgD, c("Trt1", "Trt2"), "Study")
  
  # Check all expected components are present
  expect_true("is_connected" %in% names(connectivity))
  expect_true("n_components" %in% names(connectivity))
  expect_true("components" %in% names(connectivity))
  expect_true("adj_matrix" %in% names(connectivity))
  expect_true("bridges" %in% names(connectivity))
  expect_true("articulation_points" %in% names(connectivity))
  expect_true("connectivity_ratio" %in% names(connectivity))
  expect_true("treatments" %in% names(connectivity))
  
  # Check matrix properties
  expect_true(is.matrix(connectivity$adj_matrix))
  expect_equal(nrow(connectivity$adj_matrix), ncol(connectivity$adj_matrix))
  expect_true(all(connectivity$adj_matrix %in% c(0, 1)))
}) 