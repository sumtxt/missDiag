#' Construct parameter list for ebalance
#' 
#' @param coefs starting values for model coefficients.
#' @param max.iterations maximum number of iterations.
#' @param base.weight vector of base weights.
#' @param constraint.tolerance tolerance level.
#' @param norm.constant An optional normalizing constant.
#' @param trim trim weights via \code{\link[ebal]{ebalance.trim}}
#' @param max.weight Target for the ratio of the maximum to mean weight.
#' @param min.weight Target for the ratio of the minimum to mean weight.
#' @param max.trim.iterations Maximum number of trimming iterations.
#' @param max.weight.increment Increment for iterative trimming of the 
#'  ratio of the maximum to mean weight.
#' @param min.weight.increment Increment for iterative trimming of the 
#'  ratio of the minimum to mean weight.
#' 
#' @details 
#' For more information about these parameters, see 
#' [ebal::ebalance()] and [ebal::ebalance.trim()].
#' 
#' @return A \code{list} of parameters that can be passed to [missDiag()].
#' 
#' @export
param_ebal <- function(
  coefs = NULL,
  max.iterations = 200,
  base.weight = NULL,
  constraint.tolerance = 1,
  norm.constant = NULL,
  trim = FALSE,
  max.weight = NULL,
  min.weight = 0,
  max.trim.iterations = 200,
  max.weight.increment = 0.92,
  min.weight.increment = 1.08){
  return(as.list(environment()))
  }

#' Construct parameter list for sbw
#' 
#' @param bal_alg use tuning algorithm as in Wang and Zubizarreta (2020)? 
#' @param bal_tol tolerance level.
#' @param bal_std tolerances adjustment.
#' @param bal_gri grid of values for the tuning algorithm.
#' @param bal_sam number of replicates to be used by the tuning algorithm.
#' @param sol_nam solver name. Either "cplex", "gurobi", "mosek", 
#'  "pogs", or "quadprog".
#' 
#' 
#' @details 
#' For more information about these parameters, see [sbw::sbw()].
#' 
#' @return A \code{list} of parameters that can be passed to [missDiag()].
#' 
#' @export
param_sbw <- function(
    bal_alg = TRUE,
    bal_tol = 0,
    bal_std = "group",
    bal_gri = c(1e-04,0.001,0.002,0.005,0.01,0.02,0.05,0.1),
    bal_sam = 1000,
    sol_nam = "quadprog"){

  return(as.list(environment()))  
  }

#' Construct parameter list for cobalt
#' 
#' @param stats character; which statistic(s) should be reported.
#' @param continuous whether mean differences for continuous variables 
#'  should be standardized ("std") or raw ("raw").
#' @param binary whether mean differences for binary variables (i.e., 
#'  difference in proportion) should be standardized ("std") or raw ("raw").
#' @param abs logical; whether displayed balance statistics 
#'  should be in absolute value or not.
#' @param s.d.denom character; how the denominator for standardized 
#'  mean differences should be calculated, if requested.
#' 
#' 
#' @details 
#' For more information about these parameters, see 
#'  [cobalt::bal.tab()].
#' 
#' @return A \code{list} of parameters that can be passed to [missDiag()].
#' 
#' @export
param_cobalt <- function(
  stats = c("m", "v", "ks", "ovl"), 
  continuous = "std", 
  binary = "std", 
  abs = TRUE,
  s.d.denom = 'pooled'){
  return(as.list(environment()))  
  }


