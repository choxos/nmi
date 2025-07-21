#' Turning IPD into subgroup analyses
#'
#' Transforms individual patient data into subgroup analyses matching the data
#' format required for NMI with different outcome types and binary effect modifiers.
#'
#' @param data An IPD dataset.
#' @param EM_cols A vector of effect modifier column names.
#' @param Trt_col Treatment assignment column name.
#' @param outcome_col Outcome column name.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' 
#' @return A data frame consisting of 2*p + 1 rows, the first of which contains 
#' the TE and SE for the entire trial, and the remaining 2*p contain subgroup 
#' analyses with missing EM values for all but one effect modifier.
#'
#' @export
#' @importFrom stats glm lm
GLMLOGIT <- function(data, EM_cols, Trt_col, outcome_col, outcome_type = "binary") {
  k <- length(EM_cols)
  
  # Choose appropriate model based on outcome type
  fit_model <- function(dat) {
    if (outcome_type == "binary") {
      glm(Y ~ Tr, data = dat, family = "binomial")
    } else if (outcome_type == "continuous") {
      lm(Y ~ Tr, data = dat)
    } else if (outcome_type == "count") {
      glm(Y ~ Tr, data = dat, family = "poisson")
    } else {
      stop("outcome_type must be 'binary', 'continuous', or 'count'")
    }
  }
  
  helper <- function(i) {
    dat <- data[data[, EM_cols[i]] == 1, ]
    dat$Tr <- dat[, Trt_col]
    dat$Y <- dat[, outcome_col]
    X <- apply(dat[, EM_cols], 2, mean)
    
    model <- fit_model(dat)
    U <- summary(model)
    betase1 <- c(X, U[["coefficients"]][2, 1:2])
    
    dat <- data[data[, EM_cols[i]] == 0, ]
    dat$Tr <- dat[, Trt_col]
    dat$Y <- dat[, outcome_col]
    X <- apply(dat[, EM_cols], 2, mean)
    
    model <- fit_model(dat)
    U <- summary(model)
    betase2 <- c(X, U[["coefficients"]][2, 1:2])
    
    rbind(betase1, betase2)
  }
  
  out <- do.call(rbind, lapply(as.list(1:k), helper))
  
  # Fit overall model
  data$Tr <- data[, Trt_col]
  data$Y <- data[, outcome_col]
  all_model <- fit_model(data)
  all <- summary(all_model)
  
  X <- apply(data[, EM_cols], 2, mean)
  out <- rbind(c(X, all[["coefficients"]][2, 1:2]), out)
  row.names(out) <- c()
  
  Trt <- as.data.frame(t(sapply(1:nrow(out), 
                               function(x) {
                                 sort(unique(data[, Trt_col]))
                               })))
  names(Trt) <- paste0('Trt', 1:2)
  
  return(cbind(Trt, out))
}

#' Impute missing covariate values using BLUP
#'
#' Enriches the NMI AgD table by imputing missing covariate values using 
#' the Best Linear Unbiased Predictor (BLUP).
#'
#' @param IPD An IPD dataset.
#' @param AgD An NMI-format AgD dataset.
#' @param AgD_EM_cols A vector of effect modifier column names in the AgD.
#' @param IPD_EM_cols A vector of effect modifier column names in the IPD.
#' @param Study_col Study identifier column name for the AgD.
#' @param samp_sizes A vector of sample sizes for the AgD studies.
#' @param AgD_Trt_cols A vector of treatment column names for the AgD.
#' @param TE_col Relative treatment effect column name for the AgD.
#' @param IPD_Trt_col Treatment column name for the IPD.
#' @param SE_col Standard error column name for the AgD.
#' @param outcome_col Outcome column name for the IPD.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' 
#' @return The enriched (imputed) data frame, with the summarised IPD data 
#' concatenated at the bottom.
#'
#' @export
#' @importFrom stats cor
#' @importFrom dplyr filter
BLUP_impute <- function(IPD, AgD, AgD_EM_cols, IPD_EM_cols, Study_col, 
                        samp_sizes, AgD_Trt_cols, TE_col, IPD_Trt_col, SE_col,
                        outcome_col, outcome_type = "binary") {
  rho <- cor(IPD[, IPD_EM_cols])
  n_studs <- length(unique(AgD[, Study_col]))
  studies <- 1:n_studs
  p <- length(AgD_EM_cols)
  
  single_BLUP_impute <- function(study) {
    n <- samp_sizes[study]
    Xbar <- AgD[AgD[, Study_col] == study, AgD_EM_cols][1, ]
    
    # Calculate standard errors based on outcome type
    if (outcome_type == "binary") {
      Sbar <- sqrt(Xbar * (1 - Xbar) / n)
    } else if (outcome_type == "continuous") {
      # For continuous outcomes, use empirical standard deviation
      Sbar <- sqrt(Xbar * (1 - Xbar) / n)  # Placeholder - could be improved
    } else if (outcome_type == "count") {
      # For count outcomes, use Poisson variance approximation
      Sbar <- sqrt(Xbar / n)
    }
    
    missing_mat <- function(i) {
      a <- rep(NA, 2 * p + 1)
      a[1] <- Xbar[i]
      a[2 * i] <- 1
      a[2 * i + 1] <- 0
      return(a)
    }
    
    X <- sapply(1:p, missing_mat)
    Y <- X
    for (i in 1:(2 * p)) {
      ind <- ceiling(i / 2)
      for (j in 1:p) {
        Y[-1, ][i, j] <- Sbar[j] / Sbar[ind] * rho[ind, j] * 
          (X[-1, ][i, ind] - Xbar[ind]) + Xbar[j]
      }
    }
    
    return(Y)
  }
  
  # Apply BLUP imputation to each study
  imputed_list <- lapply(as.list(studies), single_BLUP_impute)
  
  # Create expanded AgD structure to match imputed data
  expanded_agd_list <- lapply(as.list(studies), function(study) {
    # Get the original study data
    study_data <- AgD[AgD[, Study_col] == study, ]
    
    # Get the imputed matrix for this study  
    imputed_matrix <- imputed_list[[study]]
    
    # Expand study data to match imputed matrix rows (2*p + 1 rows)
    n_rows <- nrow(imputed_matrix)
    
    # Create expanded study data with all columns from original AgD
    expanded_study <- data.frame(
      Study = rep(study, n_rows),
      stringsAsFactors = FALSE
    )
    
    # Add all non-effect modifier columns from original AgD data
    for (col_name in colnames(study_data)) {
      if (!(col_name %in% c(Study_col, AgD_EM_cols))) {
        expanded_study[[col_name]] <- rep(study_data[1, col_name], n_rows)
      }
    }
    
    # Add the imputed effect modifier values
    for (i in 1:p) {
      expanded_study[[AgD_EM_cols[i]]] <- imputed_matrix[, i]
    }
    
    return(expanded_study)
  })
  
  # Combine all expanded studies
  out <- do.call(rbind, expanded_agd_list)
  
  # Get IPD summary using GLMLOGIT
  ipd_glm_result <- GLMLOGIT(IPD, IPD_EM_cols, IPD_Trt_col, outcome_col, outcome_type)
  
  # Create IPD summary with matching column names and order
  # Get the column names from the out data frame to ensure consistent structure
  out_colnames <- colnames(out)
  
  # Create IPD summary with same column structure as out
  IPD_summ <- data.frame(matrix(NA, nrow = nrow(ipd_glm_result), ncol = ncol(out)))
  colnames(IPD_summ) <- out_colnames
  
  # Fill in the IPD summary values
  IPD_summ[, Study_col] <- max(out[, Study_col]) + 1:nrow(ipd_glm_result)
  IPD_summ[, AgD_Trt_cols[1]] <- ipd_glm_result$Trt1
  IPD_summ[, AgD_Trt_cols[2]] <- ipd_glm_result$Trt2
  IPD_summ[, TE_col] <- ipd_glm_result[["Estimate"]]
  IPD_summ[, SE_col] <- ipd_glm_result[["Std. Error"]]
  
  # Add effect modifier columns
  for (i in 1:length(AgD_EM_cols)) {
    IPD_summ[, AgD_EM_cols[i]] <- ipd_glm_result[[IPD_EM_cols[i]]]
  }
  
  return(rbind(out, IPD_summ))
}

#' NMI interpolation pre-NMA
#'
#' Interpolates treatment effect estimates and standard errors at new 
#' effect modifier values using the Network Meta-Interpolation methodology.
#'
#' @param IPD An IPD dataset.
#' @param AgD An NMI-format AgD dataset.
#' @param x_vect A vector consisting of the effect modifier values for ITC.
#' @param AgD_EM_cols A vector of effect modifier column names in the AgD.
#' @param IPD_EM_cols A vector of effect modifier column names in the IPD.
#' @param Study_col Study identifier column name for the AgD.
#' @param samp_sizes A vector of sample sizes for the AgD studies.
#' @param AgD_Trt_cols A vector of treatment column names for the AgD.
#' @param TE_col Relative treatment effect column name for the AgD.
#' @param SE_col Standard error column name for the AgD.
#' @param IPD_Trt_col Treatment column name for the IPD.
#' @param outcome_col Outcome column name for the IPD.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' 
#' @return A list consisting of three data frames:
#' \item{Imputed}{The imputed AgD - the output of \code{\link{BLUP_impute}}.}
#' \item{Final}{The final NMA-format data set, after interpolation at the new
#' effect modifier levels.}
#' \item{Diagnostics}{Observed and predicted TEs for goodness of interpolation
#' diagnostics.}
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom stats lm
NMI_interpolation <- function(IPD, AgD, x_vect, AgD_EM_cols, IPD_EM_cols, 
                              Study_col, samp_sizes, AgD_Trt_cols, TE_col, 
                              SE_col, IPD_Trt_col, outcome_col, outcome_type = "binary") {
  imputed <- BLUP_impute(IPD, AgD, AgD_EM_cols, IPD_EM_cols, Study_col, 
                         samp_sizes, AgD_Trt_cols, TE_col, IPD_Trt_col, SE_col,
                         outcome_col, outcome_type)
  
  # Define studies variable for interpolation
  studies <- unique(imputed[, Study_col])
  
  single_study_interpolation <- function(study) {
    dat <- dplyr::filter(imputed, Study == study)
    X <- as.matrix(dat[, AgD_EM_cols, drop = FALSE])
    X <- apply(X, 2, as.numeric)
    if (!is.matrix(X)) {
      X <- matrix(X, ncol = length(AgD_EM_cols))
    }
    x_orig <- as.vector(X[1, ])
    m <- ncol(X)
    M2 <- as.matrix(cbind(1, X^2, 2 * X, 
                          apply(utils::combn(1:m, 2), 2, 
                                function(u) {2 * X[, u[1]] * X[, u[2]]})))
    
    beta_hat <- lm(as.numeric(dat$TE) ~ X)$coef
    sigma_hat <- c(t(M2) %*% solve(M2 %*% t(M2), (dat[, SE_col])^2))
    
    u <- c(1, x_vect^2, 2 * x_vect, 
           apply(utils::combn(1:m, 2), 2, 
                 function(u) {
                   2 * x_vect[u[1]] * x_vect[u[2]]
                 }))
    
    TE <- beta_hat %*% c(1, x_vect)
    se <- sqrt(t(sigma_hat) %*% u)
    
    TE_orig <- dat[, TE_col]
    TE_pred <- cbind(1, X) %*% beta_hat
    
    NMI_out <- data.frame(Study = study, 
                          Trt1 = unique(dat[, AgD_Trt_cols[1]]),
                          Trt2 = unique(dat[, AgD_Trt_cols[2]]),
                          x = rbind(x_vect),
                          TE = TE, se = se)
    
    colnames(NMI_out) <- gsub('\\.', '', colnames(NMI_out))
    
    Diag_out <- data.frame(Study = study,
                           X,
                           TE_orig = TE_orig,
                           TE_pred = TE_pred)
    
    list(NMI_out = NMI_out, Diag_out = Diag_out)
  }
  
  out <- lapply(studies, single_study_interpolation)
  
  Final <- do.call(rbind, lapply(out, `[[`, 1))
  Diagnostics <- do.call(rbind, lapply(out, `[[`, 2))
  
  return(list(Imputed = imputed, Final = Final, Diagnostics = Diagnostics))
}