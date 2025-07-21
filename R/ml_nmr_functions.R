#' Run ML-NMR for different outcome types with 2 effect modifiers
#'
#' Runs multilevel network meta-regression for different outcome types with 
#' two effect modifiers using the multinma package.
#'
#' @param ML_NMR_data A list consisting of the following elements:
#' \item{AgD}{Aggregate-level data.}
#' \item{IPD}{Individual patient-level data}
#' @param N_iter Number of total iterations per chain (including burn in).
#' @param N_chains Number of Markov chains.
#' @param burnin Length of burn in, i.e. number of iterations to discard at the 
#' beginning.
#' @param n_int Number of Monte Carlo integration points.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' 
#' @return A \code{\link[multinma]{stan_nma}} object.
#'
#' @export
#' @importFrom multinma combine_network set_ipd set_agd_arm add_integration
#' @importFrom multinma nma distr qbern normal
#' @importFrom dplyr mutate
ML_NMR_Run_2D <- function(ML_NMR_data, N_iter, N_chains, burnin, n_int, 
                           outcome_type = "binary") {
  
  # Set up network based on outcome type
  if (outcome_type == "binary") {
    net <- combine_network(
      set_ipd(ML_NMR_data$IPD %>%
                mutate(TrC = Tr),
              study = Study,
              trt = Tr,
              trt_class = TrtClass,
              r = Y),
      set_agd_arm(ML_NMR_data$AgD %>%
                    mutate(TrC = Tr),
                  study = Study,
                  trt = Tr,
                  trt_class = TrtClass,
                  r = r,
                  n = n),
      trt_ref = "A")
    
    likelihood <- "bernoulli2"
    link <- "logit"
  } else if (outcome_type == "continuous") {
    net <- combine_network(
      set_ipd(ML_NMR_data$IPD %>%
                mutate(TrC = Tr),
              study = Study,
              trt = Tr,
              trt_class = TrtClass,
              y = Y),
      set_agd_arm(ML_NMR_data$AgD %>%
                    mutate(TrC = Tr),
                  study = Study,
                  trt = Tr,
                  trt_class = TrtClass,
                  y = y,
                  se = se),
      trt_ref = "A")
    
    likelihood <- "normal"
    link <- "identity"
  } else if (outcome_type == "count") {
    net <- combine_network(
      set_ipd(ML_NMR_data$IPD %>%
                mutate(TrC = Tr),
              study = Study,
              trt = Tr,
              trt_class = TrtClass,
              r = Y),
      set_agd_arm(ML_NMR_data$AgD %>%
                    mutate(TrC = Tr),
                  study = Study,
                  trt = Tr,
                  trt_class = TrtClass,
                  r = r,
                  E = E),  # Exposure time
      trt_ref = "A")
    
    likelihood <- "poisson"
    link <- "log"
  }
  
  net <- add_integration(net,
                         x1 = distr(qbern, prob = x1),
                         x2 = distr(qbern, prob = x2),
                         n_int = n_int)
  
  fit.FE <- nma(net, 
                trt_effects = "fixed",
                link = link, 
                likelihood = likelihood,
                regression = ~(x1 + x2)*.trt,
                center = FALSE,
                prior_intercept = normal(scale = 10),
                prior_trt = normal(scale = 10),
                prior_reg = normal(scale = 10),
                init_r = 0.1,
                QR = TRUE,
                iter = N_iter/N_chains,
                warmup = burnin/N_chains,
                chains = N_chains)
  
  return(fit.FE)
}

#' Run ML-NMR for different outcome types with 3 effect modifiers
#'
#' Runs multilevel network meta-regression for different outcome types with 
#' three effect modifiers using the multinma package.
#'
#' @param ML_NMR_data A list consisting of the following elements:
#' \item{AgD}{Aggregate-level data.}
#' \item{IPD}{Individual patient-level data}
#' @param N_iter Number of total iterations per chain (including burn in).
#' @param N_chains Number of Markov chains.
#' @param burnin Length of burn in, i.e. number of iterations to discard at the 
#' beginning.
#' @param n_int Number of Monte Carlo integration points.
#' @param outcome_type Type of outcome: "binary", "continuous", or "count".
#' 
#' @return A \code{\link[multinma]{stan_nma}} object.
#'
#' @export
#' @importFrom multinma combine_network set_ipd set_agd_arm add_integration
#' @importFrom multinma nma distr qbern normal
#' @importFrom dplyr mutate
ML_NMR_Run_3D <- function(ML_NMR_data, N_iter, N_chains, burnin, n_int,
                           outcome_type = "binary") {
  
  # Set up network based on outcome type
  if (outcome_type == "binary") {
    net <- combine_network(
      set_ipd(ML_NMR_data$IPD %>%
                mutate(TrC = Tr),
              study = Study,
              trt = Tr,
              trt_class = TrtClass,
              r = Y),
      set_agd_arm(ML_NMR_data$AgD %>%
                    mutate(TrC = Tr),
                  study = Study,
                  trt = Tr,
                  trt_class = TrtClass,
                  r = r,
                  n = n),
      trt_ref = "A")
    
    likelihood <- "bernoulli2"
    link <- "logit"
  } else if (outcome_type == "continuous") {
    net <- combine_network(
      set_ipd(ML_NMR_data$IPD %>%
                mutate(TrC = Tr),
              study = Study,
              trt = Tr,
              trt_class = TrtClass,
              y = Y),
      set_agd_arm(ML_NMR_data$AgD %>%
                    mutate(TrC = Tr),
                  study = Study,
                  trt = Tr,
                  trt_class = TrtClass,
                  y = y,
                  se = se),
      trt_ref = "A")
    
    likelihood <- "normal"
    link <- "identity"
  } else if (outcome_type == "count") {
    net <- combine_network(
      set_ipd(ML_NMR_data$IPD %>%
                mutate(TrC = Tr),
              study = Study,
              trt = Tr,
              trt_class = TrtClass,
              r = Y),
      set_agd_arm(ML_NMR_data$AgD %>%
                    mutate(TrC = Tr),
                  study = Study,
                  trt = Tr,
                  trt_class = TrtClass,
                  r = r,
                  E = E),  # Exposure time
      trt_ref = "A")
    
    likelihood <- "poisson"
    link <- "log"
  }
  
  net <- add_integration(net,
                         x1 = distr(qbern, prob = x1),
                         x2 = distr(qbern, prob = x2),
                         x3 = distr(qbern, prob = x3),
                         n_int = n_int)
  
  fit.FE <- nma(net, 
                trt_effects = "fixed",
                link = link, 
                likelihood = likelihood,
                regression = ~(x1 + x2 + x3)*.trt,
                center = FALSE,
                prior_intercept = normal(scale = 10),
                prior_trt = normal(scale = 10),
                prior_reg = normal(scale = 10),
                init_r = 0.1,
                QR = TRUE,
                iter = N_iter/N_chains,
                warmup = burnin/N_chains,
                chains = N_chains)
  
  return(fit.FE)
}

#' Extract treatment effects from ML-NMR run (2D)
#'
#' Summarizes posterior distributions of treatment effects from a multilevel 
#' network meta-regression run with two effect modifiers.
#'
#' @param n_trts Number of distinct treatments in the network.
#' @param ML_NMR_Fit A \code{\link[multinma]{stan_nma}} object. The output of 
#' \code{\link{ML_NMR_Run_2D}}.
#' @param x_vect A vector of length two, consisting of the effect modifier 
#' levels at which the ITC will be conducted.
#' 
#' @return A data frame of posterior summaries for the treatment effects at 
#' x_vect.
#'
#' @export
#' @importFrom rstan extract
#' @importFrom stats quantile
ML_NMR_summary_2D <- function(n_trts, ML_NMR_Fit, x_vect) {
  inds <- names(ML_NMR_Fit$stanfit)[grep("beta|^d\\[", 
                                          names(ML_NMR_Fit$stanfit))]
  inds1 <- grep("trt", inds)
  inds2 <- grep("d", inds)
  inds <- c(inds1, inds2)
  
  stan_out <- rstan::extract(ML_NMR_Fit$stanfit)
  df <- as.data.frame(cbind(stan_out$beta, stan_out$d))[, inds]
  
  df1 <- df[, inds1 - 2]
  df2 <- df[, inds2 - 2]
  
  x1 <- x_vect[1]
  x2 <- x_vect[2]
  
  post_samp <- as.matrix(df2) + 
    apply(as.matrix(df1) %*% c(x1, x2), 1, sum)
  colnames(post_samp) <- paste0("A", LETTERS[2:n_trts])
  
  for (i in 1:(n_trts - 2)) {
    for (j in (i + 1):(n_trts - 1)) {
      temp <- as.matrix(post_samp[, j] - post_samp[, i], ncol = 1)
      colnames(temp) <- paste0(LETTERS[2:n_trts][i],
                               LETTERS[2:n_trts][j])
      post_samp <- cbind(post_samp, temp)
    }
  }
  
  estimates <- t(apply(post_samp, 2, 
                       function(x) {
                         c(mean(x), quantile(x, c(.5, .025, .975)))
                       }))
  
  estimates <- as.data.frame(cbind(estimates))
  
  temp <- t(utils::combn(1:4, 2))
  estimates$Parameter <- paste0("D[", temp[, 1], ",", temp[, 2], "]")
  
  names(estimates)[1] <- "Mean"
  row.names(estimates) <- c()
  
  return(estimates)
}

#' Extract treatment effects from ML-NMR run (3D)
#'
#' Summarizes posterior distributions of treatment effects from a multilevel 
#' network meta-regression run with three effect modifiers.
#'
#' @param n_trts Number of distinct treatments in the network.
#' @param ML_NMR_Fit A \code{\link[multinma]{stan_nma}} object. The output of 
#' \code{\link{ML_NMR_Run_3D}}.
#' @param x_vect A vector of length three, consisting of the effect modifier 
#' levels at which the ITC will be conducted.
#' 
#' @return A data frame of posterior summaries for the treatment effects at 
#' x_vect.
#'
#' @export
#' @importFrom rstan extract
#' @importFrom stats quantile
ML_NMR_summary_3D <- function(n_trts, ML_NMR_Fit, x_vect) {
  inds <- names(ML_NMR_Fit$stanfit)[grep("beta|^d\\[", 
                                          names(ML_NMR_Fit$stanfit))]
  inds1 <- grep("trt", inds)
  inds2 <- grep("d", inds)
  inds <- c(inds1, inds2)
  
  stan_out <- rstan::extract(ML_NMR_Fit$stanfit)
  df <- as.data.frame(cbind(stan_out$beta, stan_out$d))[, inds]
  
  df1 <- df[, inds1 - 3]
  df2 <- df[, inds2 - 3]
  
  x1 <- x_vect[1]
  x2 <- x_vect[2]
  x3 <- x_vect[3]
  
  post_samp <- as.matrix(df2) + 
    apply(as.matrix(df1) %*% c(x1, x2, x3), 1, sum)
  colnames(post_samp) <- paste0("A", LETTERS[2:n_trts])
  
  for (i in 1:(n_trts - 2)) {
    for (j in (i + 1):(n_trts - 1)) {
      temp <- as.matrix(post_samp[, j] - post_samp[, i], ncol = 1)
      colnames(temp) <- paste0(LETTERS[2:n_trts][i],
                               LETTERS[2:n_trts][j])
      post_samp <- cbind(post_samp, temp)
    }
  }
  
  estimates <- t(apply(post_samp, 2, 
                       function(x) {
                         c(mean(x), quantile(x, c(.5, .025, .975)))
                       }))
  
  estimates <- as.data.frame(cbind(estimates))
  
  temp <- t(utils::combn(1:4, 2))
  estimates$Parameter <- paste0("D[", temp[, 1], ",", temp[, 2], "]")
  
  names(estimates)[1] <- "Mean"
  row.names(estimates) <- c()
  
  return(estimates)
}