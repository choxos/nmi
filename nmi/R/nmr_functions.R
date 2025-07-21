#' Stan model for fixed effects NMR with 2 effect modifiers
#'
#' Stan model code for fixed effects network meta-regression with continuous 
#' outcomes and two effect modifiers in a treatment-difference format.
#'
#' @return Character string containing Stan model code
#'
#' @export
stan_model_2D <- function() {
  return("
    data {
      int<lower=0> ns;  // number of studies
      int<lower=0> nt;  // number of treatments
      array[ns,2] int t;   // treatment indicators
      vector[ns] y;     // treatment effects
      vector[ns] se;    // standard errors
      vector[ns] x1;    // first covariate
      vector[ns] x2;    // second covariate
    }
    
    parameters {
      vector[nt-1] d_raw;  // treatment effects (relative to reference)
      real B1;             // covariate effect 1
      real B2;             // covariate effect 2
    }
    
    transformed parameters {
      vector[nt] d;
      vector[ns] theta;
      
      d[1] = 0;  // reference treatment effect
      d[2:nt] = d_raw;
      
      for(i in 1:ns) {
        theta[i] = d[t[i,2]] - d[t[i,1]] + 
                   B1 * x1[i] + B2 * x2[i];
      }
    }
    
    model {
      // Priors
      d_raw ~ normal(0, 10);
      B1 ~ normal(0, 10);
      B2 ~ normal(0, 10);
      
      // Likelihood
      y ~ normal(theta, se);
    }
    
    generated quantities {
      matrix[nt-1,nt] D;
      
      // Pairwise treatment effects
      for(c in 1:(nt-1)) {
        for(k in (c+1):nt) {
          D[c,k] = d[k] - d[c];
        }
      }
    }
  ")
}

#' Stan model for fixed effects NMR with 3 effect modifiers
#'
#' Stan model code for fixed effects network meta-regression with continuous 
#' outcomes and three effect modifiers in a treatment-difference format.
#'
#' @return Character string containing Stan model code
#'
#' @export
stan_model_3D <- function() {
  return("
    data {
      int<lower=0> ns;  // number of studies
      int<lower=0> nt;  // number of treatments
      array[ns,2] int t;   // treatment indicators
      vector[ns] y;     // treatment effects
      vector[ns] se;    // standard errors
      vector[ns] x1;    // first covariate
      vector[ns] x2;    // second covariate
      vector[ns] x3;    // third covariate
    }
    
    parameters {
      vector[nt-1] d_raw;  // treatment effects (relative to reference)
      real B1;             // covariate effect 1
      real B2;             // covariate effect 2
      real B3;             // covariate effect 3
    }
    
    transformed parameters {
      vector[nt] d;
      vector[ns] theta;
      
      d[1] = 0;  // reference treatment effect
      d[2:nt] = d_raw;
      
      for(i in 1:ns) {
        theta[i] = d[t[i,2]] - d[t[i,1]] + 
                   B1 * x1[i] + B2 * x2[i] + B3 * x3[i];
      }
    }
    
    model {
      // Priors
      d_raw ~ normal(0, 10);
      B1 ~ normal(0, 10);
      B2 ~ normal(0, 10);
      B3 ~ normal(0, 10);
      
      // Likelihood
      y ~ normal(theta, se);
    }
    
    generated quantities {
      matrix[nt-1,nt] D;
      
      // Pairwise treatment effects
      for(c in 1:(nt-1)) {
        for(k in (c+1):nt) {
          D[c,k] = d[k] - d[c];
        }
      }
    }
  ")
}

#' Run Network Meta-Regression with 2 effect modifiers
#'
#' Runs a fixed effects network meta-regression with two effect modifiers
#' using Stan.
#'
#' @param dat Aggregate level data.
#' @param N_chains Number of Markov chains.
#' @param N_iter Number of total iterations per chain (including burn in).
#' @param burnin Length of burn in, i.e. number of iterations to discard at the 
#' beginning.
#' 
#' @return A \code{\link[rstan]{stanfit}} object.
#'
#' @export
#' @importFrom rstan stan
#' @importFrom dplyr as_tibble mutate recode
NMA_Meta_Reg_run_2D <- function(dat, N_chains, N_iter, burnin) {
  dat <- dat %>% 
    as_tibble() %>% 
    mutate(across(c(Trt1, Trt2), 
                  ~ dplyr::recode(., 'A' = '1', 'B' = '2', 
                                  'C' = '3', 'D' = '4')))
  
  ns <- nrow(dat)
  
  se <- as.numeric(dat$se)
  y <- as.numeric(dat$TE)
  nt <- length(unique(dat$Trt2)) + 1
  t <- apply(dat[, 2:3], 2, as.numeric)
  x1 <- as.numeric(dat$x1)
  x2 <- as.numeric(dat$x2)
  
  stan_data <- list(ns = ns, nt = nt, t = t, y = y, se = se, 
                    x1 = x1, x2 = x2)
  
  model_code <- stan_model_2D()
  
  sim <- rstan::stan(model_code = model_code,
                     data = stan_data,
                     chains = N_chains,
                     iter = N_iter,
                     warmup = burnin,
                     thin = 1)
  
  return(sim)
}

#' Run Network Meta-Regression with 3 effect modifiers
#'
#' Runs a fixed effects network meta-regression with three effect modifiers
#' using Stan.
#'
#' @param dat Aggregate level data.
#' @param N_chains Number of Markov chains.
#' @param N_iter Number of total iterations per chain (including burn in).
#' @param burnin Length of burn in, i.e. number of iterations to discard at the 
#' beginning.
#' 
#' @return A \code{\link[rstan]{stanfit}} object.
#'
#' @export
#' @importFrom rstan stan
#' @importFrom dplyr as_tibble mutate recode
NMA_Meta_Reg_run_3D <- function(dat, N_chains, N_iter, burnin) {
  dat <- dat %>% 
    as_tibble() %>% 
    mutate(across(c(Trt1, Trt2), 
                  ~ dplyr::recode(., 'A' = '1', 'B' = '2', 
                                  'C' = '3', 'D' = '4')))
  
  ns <- nrow(dat)
  
  se <- as.numeric(dat$se)
  y <- as.numeric(dat$TE)
  nt <- length(unique(dat$Trt2)) + 1
  t <- apply(dat[, 2:3], 2, as.numeric)
  x1 <- as.numeric(dat$x1)
  x2 <- as.numeric(dat$x2)
  x3 <- as.numeric(dat$x3)
  
  stan_data <- list(ns = ns, nt = nt, t = t, y = y, se = se, 
                    x1 = x1, x2 = x2, x3 = x3)
  
  model_code <- stan_model_3D()
  
  sim <- rstan::stan(model_code = model_code,
                     data = stan_data,
                     chains = N_chains,
                     iter = N_iter,
                     warmup = burnin,
                     thin = 1)
  
  return(sim)
}

#' Extract treatment effects from NMR run (2D)
#'
#' Summarizes posterior distributions of treatment effects from a network 
#' meta-regression run with two effect modifiers.
#'
#' @param sim A \code{\link[rstan]{stanfit}} object. The output of 
#' \code{\link{NMA_Meta_Reg_run_2D}}.
#' @param x_vect A vector of length two, consisting of the effect modifier 
#' levels at which the ITC will be conducted.
#' 
#' @return A data frame of posterior summaries for the treatment effects at x1 
#' and x2.
#'
#' @export
#' @importFrom rstan extract
#' @importFrom stats quantile
NMA_Metareg_summary_2D <- function(sim, x_vect) {
  x1 <- x_vect[1]
  x2 <- x_vect[2]
  
  # Extract posterior samples
  samples <- rstan::extract(sim)
  
  # Calculate treatment effects at specified covariate levels
  nt <- ncol(samples$d) + 1
  n_samples <- length(samples$B1)
  
  # Create matrix for pairwise differences
  D_samples <- array(NA, dim = c(n_samples, nt-1, nt))
  
  for(i in 1:n_samples) {
    d_full <- c(0, samples$d[i,])  # Include reference treatment
    d_adj <- d_full + c(0, rep(samples$B1[i] * x1 + samples$B2[i] * x2, nt-1))
    
    # Calculate pairwise differences
    for(c in 1:(nt-1)) {
      for(k in (c+1):nt) {
        D_samples[i, c, k] <- d_adj[k] - d_adj[c]
      }
    }
  }
  
  # Summarize posterior distributions
  out_list <- list()
  for(c in 1:(nt-1)) {
    for(k in (c+1):nt) {
      if(!is.na(D_samples[1, c, k])) {
        samp <- D_samples[, c, k]
        out_list[[paste0("D[", c, ",", k, "]")]] <- c(
          mean(samp), 
          quantile(samp, c(.5, .025, .975))
        )
      }
    }
  }
  
  out <- as.data.frame(do.call(rbind, out_list))
  colnames(out) <- c("Mean", "50%", "2.5%", "97.5%")
  out$Parameter <- row.names(out)
  row.names(out) <- c()
  
  return(out)
}

#' Extract treatment effects from NMR run (3D)
#'
#' Summarizes posterior distributions of treatment effects from a network 
#' meta-regression run with three effect modifiers.
#'
#' @param sim An \code{\link[R2OpenBUGS]{bugs}} object. The output of 
#' \code{\link{NMA_Meta_Reg_run_3D}}.
#' @param x_vect A vector of length three, consisting of the effect modifier 
#' levels at which the ITC will be conducted.
#' 
#' @return A data frame of posterior summaries for the treatment effects at x1, 
#' x2, and x3.
#'
#' @export
#' @importFrom stats quantile
NMA_Metareg_summary_3D <- function(sim, x_vect) {
  x1 <- x_vect[1]
  x2 <- x_vect[2]
  x3 <- x_vect[3]
  
  cols <- grepl('D\\[|B1|B2|B3', colnames(sim$sims.matrix))
  M <- sim$sims.matrix[, cols]
  M[, 1:3] <- apply(M[, 1:3], 2,
                    function(col) {
                      col + M[, "B1"] * x1 + M[, "B2"] * x2 + M[, "B3"] * x3
                    })
  
  out <- as.data.frame(
    t(apply(M[, 1:(ncol(M) - 3)], 2, function(x) {
      c(mean(x), quantile(x, c(.5, .025, .975)))
    })))
  
  out$Parameter <- row.names(out)
  names(out)[1] <- 'Mean'
  row.names(out) <- c()
  
  return(out)
}