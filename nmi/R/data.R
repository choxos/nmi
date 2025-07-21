#' Example Individual Patient Data (IPD)
#'
#' A dataset containing individual patient data for use with the NMI methodology.
#' This dataset includes binary effect modifiers and a binary outcome.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{x1}{First effect modifier (0 or 1)}
#'   \item{x2}{Second effect modifier (0 or 1)}
#'   \item{Tr}{Treatment assignment (A or D)}
#'   \item{Y}{Binary outcome (0 or 1)}
#'   \item{Study}{Study identifier}
#'   \item{TrtClass}{Treatment class (Ctrl or Trt)}
#' }
#'
#' @source Simulated data based on Harari et al. (2023) Network Meta-Interpolation paper
#' @references Harari et al. (2023) Network meta-interpolation: Effect modification 
#' adjustment in network meta-analysis using subgroup analyses. 
#' Journal of the Royal Statistical Society: Series A.
"Example_IPD"

#' Example Aggregate Data (AgD) for NMI
#'
#' A dataset containing aggregate data in the format required for Network 
#' Meta-Interpolation (NMI) analysis.
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{Study}{Study identifier}
#'   \item{Trt1}{First treatment in comparison}
#'   \item{Trt2}{Second treatment in comparison}
#'   \item{n}{Sample size (for some rows)}
#'   \item{x1}{Proportion of patients with first effect modifier}
#'   \item{x2}{Proportion of patients with second effect modifier}
#'   \item{TE}{Treatment effect estimate}
#'   \item{se}{Standard error of treatment effect}
#' }
#'
#' @source Simulated data based on Harari et al. (2023) Network Meta-Interpolation paper
#' @references Harari et al. (2023) Network meta-interpolation: Effect modification 
#' adjustment in network meta-analysis using subgroup analyses. 
#' Journal of the Royal Statistical Society: Series A.
"Example_AgD_NMI"

#' Load example IPD data
#'
#' @return A data frame with example IPD data
#' @export
load_example_ipd <- function() {
  ipd_path <- system.file("extdata", "Example_IPD.csv", package = "nmi")
  if (ipd_path == "") {
    stop("Example IPD data not found. Please check package installation.")
  }
  utils::read.csv(ipd_path)
}

#' Load example AgD data for NMI
#'
#' @return A data frame with example AgD data
#' @export
load_example_agd <- function() {
  agd_path <- system.file("extdata", "Example_AgD_NMI.csv", package = "nmi")
  if (agd_path == "") {
    stop("Example AgD data not found. Please check package installation.")
  }
  utils::read.csv(agd_path)
}