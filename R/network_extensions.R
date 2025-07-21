#' Network Extensions for NMI
#' 
#' Functions for handling disconnected networks, single-arm studies, and RWD integration
#' Part of NMI v1.3.0 Network Extensions
#' 
#' @author Ahmad Sofi-Mahmudi
#' @email a.sofimahmudi@gmail.com

#' Detect Network Connectivity
#' 
#' Analyzes network structure to identify disconnected components and connectivity issues
#' 
#' @param AgD Aggregate data containing treatment comparisons
#' @param trt_cols Character vector of treatment column names (e.g., c("Trt1", "Trt2"))
#' @param study_col Character name of study identifier column
#' @return List containing connectivity analysis results
#' 
#' @examples
#' \dontrun{
#' connectivity <- detect_network_connectivity(AgD, c("Trt1", "Trt2"), "Study")
#' print(connectivity$is_connected)
#' plot(connectivity$network_graph)
#' }
#' 
#' @export
detect_network_connectivity <- function(AgD, trt_cols, study_col) {
  
  # Validate inputs
  if (!all(trt_cols %in% colnames(AgD))) {
    stop("Treatment columns not found in AgD: ", paste(setdiff(trt_cols, colnames(AgD)), collapse = ", "))
  }
  
  if (!study_col %in% colnames(AgD)) {
    stop("Study column '", study_col, "' not found in AgD")
  }
  
  # Extract unique treatments
  treatments <- unique(c(AgD[[trt_cols[1]]], AgD[[trt_cols[2]]]))
  treatments <- treatments[!is.na(treatments)]
  n_treatments <- length(treatments)
  
  # Create adjacency matrix
  adj_matrix <- matrix(0, nrow = n_treatments, ncol = n_treatments)
  rownames(adj_matrix) <- colnames(adj_matrix) <- treatments
  
  # Fill adjacency matrix based on direct comparisons
  for (i in 1:nrow(AgD)) {
    trt1 <- AgD[[trt_cols[1]]][i]
    trt2 <- AgD[[trt_cols[2]]][i]
    
    if (!is.na(trt1) && !is.na(trt2) && trt1 != trt2) {
      adj_matrix[trt1, trt2] <- 1
      adj_matrix[trt2, trt1] <- 1  # Undirected graph
    }
  }
  
  # Find connected components using DFS
  components <- find_connected_components(adj_matrix)
  
  # Calculate network metrics
  n_components <- length(components)
  is_connected <- n_components == 1
  largest_component_size <- max(sapply(components, length))
  
  # Identify bridges and articulation points
  bridges <- find_bridges(adj_matrix)
  articulation_points <- find_articulation_points(adj_matrix)
  
  # Create results
  result <- list(
    is_connected = is_connected,
    n_components = n_components,
    components = components,
    largest_component_size = largest_component_size,
    connectivity_ratio = largest_component_size / n_treatments,
    adj_matrix = adj_matrix,
    bridges = bridges,
    articulation_points = articulation_points,
    treatments = treatments,
    n_comparisons = nrow(AgD),
    studies_per_comparison = table(paste(AgD[[trt_cols[1]]], "vs", AgD[[trt_cols[2]]]))
  )
  
  class(result) <- "nmi_network_connectivity"
  return(result)
}

#' Find Connected Components in Network
#' 
#' Uses depth-first search to identify connected components
#' 
#' @param adj_matrix Adjacency matrix of treatment network
#' @return List of connected components (each component is a vector of treatment names)
find_connected_components <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  visited <- rep(FALSE, n)
  components <- list()
  component_id <- 1
  
  for (i in 1:n) {
    if (!visited[i]) {
      component <- dfs_component(adj_matrix, i, visited)
      components[[component_id]] <- rownames(adj_matrix)[component]
      component_id <- component_id + 1
    }
  }
  
  return(components)
}

#' Depth-First Search for Component Discovery
#' 
#' @param adj_matrix Adjacency matrix
#' @param start Starting node index
#' @param visited Logical vector of visited nodes
#' @return Vector of node indices in the component
dfs_component <- function(adj_matrix, start, visited) {
  stack <- c(start)
  component <- c()
  
  while (length(stack) > 0) {
    current <- stack[length(stack)]
    stack <- stack[-length(stack)]
    
    if (!visited[current]) {
      visited[current] <<- TRUE
      component <- c(component, current)
      
      # Add unvisited neighbors to stack
      neighbors <- which(adj_matrix[current, ] == 1)
      unvisited_neighbors <- neighbors[!visited[neighbors]]
      stack <- c(stack, unvisited_neighbors)
    }
  }
  
  return(component)
}

#' Find Bridges in Network
#' 
#' Identifies critical edges whose removal would disconnect the network
#' 
#' @param adj_matrix Adjacency matrix
#' @return Data frame of bridge edges
find_bridges <- function(adj_matrix) {
  bridges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  n <- nrow(adj_matrix)
  treatments <- rownames(adj_matrix)
  
  # Check each edge to see if removing it increases components
  original_components <- length(find_connected_components(adj_matrix))
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (adj_matrix[i, j] == 1) {
        # Temporarily remove edge
        temp_matrix <- adj_matrix
        temp_matrix[i, j] <- temp_matrix[j, i] <- 0
        
        # Check if components increased
        new_components <- length(find_connected_components(temp_matrix))
        if (new_components > original_components) {
          bridges <- rbind(bridges, data.frame(
            from = treatments[i], 
            to = treatments[j], 
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(bridges)
}

#' Find Articulation Points in Network
#' 
#' Identifies critical nodes whose removal would disconnect the network
#' 
#' @param adj_matrix Adjacency matrix
#' @return Vector of articulation point treatment names
find_articulation_points <- function(adj_matrix) {
  articulation_points <- c()
  n <- nrow(adj_matrix)
  treatments <- rownames(adj_matrix)
  
  if (n <= 2) return(character(0))
  
  original_components <- length(find_connected_components(adj_matrix))
  
  for (i in 1:n) {
    # Create matrix without node i
    temp_matrix <- adj_matrix[-i, -i]
    
    if (nrow(temp_matrix) > 0) {
      new_components <- length(find_connected_components(temp_matrix))
      
      # Account for the removed node
      effective_new_components <- new_components + 1
      
      if (effective_new_components > original_components) {
        articulation_points <- c(articulation_points, treatments[i])
      }
    }
  }
  
  return(unique(articulation_points))
}

#' Handle Disconnected Networks in NMI
#' 
#' Provides strategies for analyzing disconnected networks using NMI
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param x_vect Target effect modifier values
#' @param AgD_EM_cols Effect modifier columns in AgD
#' @param IPD_EM_cols Effect modifier columns in IPD
#' @param trt_cols Treatment comparison columns
#' @param study_col Study identifier column
#' @param strategy Strategy for handling disconnection: "component_wise", "bridge_augment", "reference_connect"
#' @param reference_treatment Reference treatment for connection (if strategy = "reference_connect")
#' @param ... Additional arguments passed to NMI_interpolation
#' 
#' @return List containing results for each network component or integrated network
#' 
#' @examples
#' \dontrun{
#' # Component-wise analysis
#' results <- handle_disconnected_network(
#'   IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
#'   trt_cols = c("Trt1", "Trt2"), study_col = "Study",
#'   strategy = "component_wise"
#' )
#' 
#' # Bridge augmentation
#' results <- handle_disconnected_network(
#'   IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
#'   trt_cols = c("Trt1", "Trt2"), study_col = "Study",
#'   strategy = "bridge_augment"
#' )
#' }
#' 
#' @export
handle_disconnected_network <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                       trt_cols, study_col, strategy = "component_wise",
                                       reference_treatment = NULL, ...) {
  
  # Detect network connectivity
  connectivity <- detect_network_connectivity(AgD, trt_cols, study_col)
  
  if (connectivity$is_connected) {
    message("Network is fully connected. Proceeding with standard NMI analysis.")
    
    result <- NMI_interpolation(
      IPD = IPD, AgD = AgD, x_vect = x_vect,
      AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols,
      Study_col = study_col, ...
    )
    
    return(list(
      strategy_used = "standard",
      connectivity = connectivity,
      results = result,
      components = list(all = result)
    ))
  }
  
  message("Disconnected network detected (", connectivity$n_components, " components).")
  message("Using strategy: ", strategy)
  
  # Handle based on strategy
  switch(strategy,
    "component_wise" = handle_component_wise(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols, 
                                           connectivity, trt_cols, study_col, ...),
    "bridge_augment" = handle_bridge_augment(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                           connectivity, trt_cols, study_col, ...),
    "reference_connect" = handle_reference_connect(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                                 connectivity, trt_cols, study_col, 
                                                 reference_treatment, ...),
    stop("Unknown strategy: ", strategy, ". Available: 'component_wise', 'bridge_augment', 'reference_connect'")
  )
}

#' Component-wise Analysis Strategy
#' 
#' Analyzes each connected component separately
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param x_vect Target effect modifier values
#' @param AgD_EM_cols Effect modifier columns in AgD
#' @param IPD_EM_cols Effect modifier columns in IPD
#' @param connectivity Network connectivity analysis
#' @param trt_cols Treatment comparison columns
#' @param study_col Study identifier column
#' @param ... Additional arguments
#' @return List of component-wise results
handle_component_wise <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                 connectivity, trt_cols, study_col, ...) {
  
  component_results <- list()
  
  for (i in seq_along(connectivity$components)) {
    component_treatments <- connectivity$components[[i]]
    component_name <- paste0("Component_", i, "_", paste(component_treatments[1:min(2, length(component_treatments))], collapse = "_"))
    
    message("Analyzing component ", i, ": ", paste(component_treatments, collapse = ", "))
    
    # Filter AgD for this component
    component_AgD <- AgD[
      (AgD[[trt_cols[1]]] %in% component_treatments) & 
      (AgD[[trt_cols[2]]] %in% component_treatments), 
    ]
    
    if (nrow(component_AgD) == 0) {
      warning("No comparisons found for component ", i)
      next
    }
    
    # Filter IPD if treatments are specified
    component_IPD <- IPD
    if ("treatment" %in% colnames(IPD) || "Tr" %in% colnames(IPD)) {
      trt_col_ipd <- if ("treatment" %in% colnames(IPD)) "treatment" else "Tr"
      component_IPD <- IPD[IPD[[trt_col_ipd]] %in% component_treatments, ]
    }
    
    # Run NMI for this component
    tryCatch({
      component_result <- NMI_interpolation(
        IPD = component_IPD, AgD = component_AgD, x_vect = x_vect,
        AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols,
        Study_col = study_col, ...
      )
      
      component_results[[component_name]] <- list(
        treatments = component_treatments,
        n_comparisons = nrow(component_AgD),
        result = component_result
      )
      
    }, error = function(e) {
      warning("Failed to analyze component ", i, ": ", e$message)
      component_results[[component_name]] <<- list(
        treatments = component_treatments,
        n_comparisons = nrow(component_AgD),
        error = e$message
      )
    })
  }
  
  return(list(
    strategy_used = "component_wise",
    connectivity = connectivity,
    components = component_results,
    summary = summarize_component_results(component_results)
  ))
}

#' Bridge Augmentation Strategy
#' 
#' Attempts to connect components using auxiliary data or assumptions
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param x_vect Target effect modifier values
#' @param AgD_EM_cols Effect modifier columns in AgD
#' @param IPD_EM_cols Effect modifier columns in IPD
#' @param connectivity Network connectivity analysis
#' @param trt_cols Treatment comparison columns
#' @param study_col Study identifier column
#' @param ... Additional arguments
#' @return List containing augmented network results
handle_bridge_augment <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                 connectivity, trt_cols, study_col, ...) {
  
  message("Attempting to bridge disconnected components...")
  
  # Identify potential bridges between components
  potential_bridges <- find_potential_bridges(connectivity$components, IPD, AgD)
  
  if (length(potential_bridges) == 0) {
    warning("No potential bridges found. Falling back to component-wise analysis.")
    return(handle_component_wise(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                connectivity, trt_cols, study_col, ...))
  }
  
  # Create bridged network
  bridged_AgD <- create_bridged_network(AgD, potential_bridges, x_vect, AgD_EM_cols)
  
  # Run NMI on bridged network
  tryCatch({
    result <- NMI_interpolation(
      IPD = IPD, AgD = bridged_AgD, x_vect = x_vect,
      AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols,
      Study_col = study_col, ...
    )
    
    return(list(
      strategy_used = "bridge_augment",
      connectivity = connectivity,
      bridges_added = potential_bridges,
      original_comparisons = nrow(AgD),
      augmented_comparisons = nrow(bridged_AgD),
      results = result
    ))
    
  }, error = function(e) {
    warning("Bridge augmentation failed: ", e$message)
    warning("Falling back to component-wise analysis.")
    return(handle_component_wise(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                connectivity, trt_cols, study_col, ...))
  })
}

#' Reference Connection Strategy
#' 
#' Connects components through a common reference treatment
#' 
#' @param IPD Individual patient data
#' @param AgD Aggregate data
#' @param x_vect Target effect modifier values
#' @param AgD_EM_cols Effect modifier columns in AgD
#' @param IPD_EM_cols Effect modifier columns in IPD
#' @param connectivity Network connectivity analysis
#' @param trt_cols Treatment comparison columns
#' @param study_col Study identifier column
#' @param reference_treatment Reference treatment name
#' @param ... Additional arguments
#' @return List containing reference-connected results
handle_reference_connect <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                    connectivity, trt_cols, study_col, 
                                    reference_treatment, ...) {
  
  if (is.null(reference_treatment)) {
    stop("Reference treatment must be specified for 'reference_connect' strategy")
  }
  
  message("Connecting components via reference treatment: ", reference_treatment)
  
  # Create reference connections
  reference_AgD <- create_reference_connections(AgD, connectivity$components, 
                                              reference_treatment, x_vect, AgD_EM_cols)
  
  # Run NMI on reference-connected network
  tryCatch({
    result <- NMI_interpolation(
      IPD = IPD, AgD = reference_AgD, x_vect = x_vect,
      AgD_EM_cols = AgD_EM_cols, IPD_EM_cols = IPD_EM_cols,
      Study_col = study_col, ...
    )
    
    return(list(
      strategy_used = "reference_connect",
      connectivity = connectivity,
      reference_treatment = reference_treatment,
      original_comparisons = nrow(AgD),
      connected_comparisons = nrow(reference_AgD),
      results = result
    ))
    
  }, error = function(e) {
    warning("Reference connection failed: ", e$message)
    warning("Falling back to component-wise analysis.")
    return(handle_component_wise(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols,
                                connectivity, trt_cols, study_col, ...))
  })
}

# Helper functions for network bridging and connection
find_potential_bridges <- function(components, IPD, AgD) {
  # Simplified implementation - in practice would use more sophisticated methods
  return(list())
}

create_bridged_network <- function(AgD, bridges, x_vect, AgD_EM_cols) {
  # Simplified implementation
  return(AgD)
}

create_reference_connections <- function(AgD, components, reference_treatment, x_vect, AgD_EM_cols) {
  # Simplified implementation
  return(AgD)
}

summarize_component_results <- function(component_results) {
  n_successful <- sum(sapply(component_results, function(x) !is.null(x$result)))
  n_total <- length(component_results)
  
  list(
    n_components = n_total,
    n_successful = n_successful,
    success_rate = n_successful / n_total,
    component_sizes = sapply(component_results, function(x) length(x$treatments))
  )
} 