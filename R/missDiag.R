#' Comparing Observed and Imputed Values under MAR and MCAR
#'
#' Function to compute discrepancy statistics comparing the (reweighted) 
#' density of imputed and observed values. To compute the weights balancing
#' covariates that are part of the missing data mechanism, the function 
#' relies on either entropy balancing or stable balancing weights.
#' 
#' @param original data frame with missing values. 
#' @param imputed an imputed data frame, a list of imputed data frames or 
#'  a \code{mids} object from the [mice::mice()] package.
#' @param formula two-sided formula with the variables to compute discrepancy
#'  statistics (right-hand side) and variables to balance (left-hand side). 
#' @param skip_n skip variables with fewer than \code{skip_n} missing values. 
#' @param scale scale the design matrix?
#' @param adjust either 'none', 'sbw' or 'ebal'.
#' @param use_imputed_cov use imputed covariates? 
#' @param convert vector of variable types to bin before coercion
#'  (ignored if \code{use_imputed_cov=TRUE}).
#' @param categories how many bins (ignored if \code{use_imputed_cov=TRUE}).
#' @param verbose one of three levels (0,1 or 2). 
#' @param output_diag add covariate balance statistics to output? 
#' @param cobalt_param list of parameters passed to \code{cobalt}.
#' @param ebal_param list of parameters passed to \code{ebal}.
#' @param sbw_param list of parameters passed to \code{sbw}.
#'   
#' @details 
#' Let y be a variable with observed and imputed values and let m be 
#' a corresponding indicator if a value in y is observed (0) or imputed (1). 
#' We use X to denote all K covariates and x_k as the k^th covariate. 
#' 
#' By default, \code{missDiag} computes discrepancy statistics (balance
#' statistics) comparing the distribution of observed and imputed values, 
#' ie. comparing f(y|m=1) vs. f(y|m=0). The distribution are expected 
#' to be equal under MCAR. 
#' 
#' If the data are missing at random (MAR), set \code{adjust="ebal"} 
#' or \code{adjust="sbw"}, to construct weights to balance the 
#' covariate distribution X before computing the discrepancy statistics. 
#' 
#' The left-hand side (lhs) of the \code{formula} gives the name of y while the 
#' right-hand side (rhs) defines the covariates X to balance under MAR.
#' Rhs variables are transformed into a numeric matrix via the 
#' [stats::model.matrix()] function. Therefore  factor and string variables 
#' are dummy encoded and functional transformations such as \code{log()} 
#' are applied. 
#' 
#' To run \code{missDiag} on many y variables sequentially, supply a list 
#' of formulas or name all y variables on the lhs separating each variable 
#' with a "|", e.g. \code{y1 | y2 ~ x1 + ... + xK}.
#' 
#' If \code{formula=NULL}, \code{missDiag} runs on all variables with at 
#' least \code{n_skip} missing values in \code{original}. Weights are 
#' constructed such that all X covariates are balanced 
#' (if \code{adjust!='none'}). 
#' 
#' If \code{use_imputed_cov=FALSE}, all imputed values in the covariates X 
#' are deleted, variables are coerced to factor variables with missing 
#' values encoded as one category. Numerical variables are binned by quantiles 
#' before coercion. Use the parameter \code{categories} to define the 
#' number of bins. By default only variables of class "numeric" are binned. 
#' Use \code{convert = c("numeric", "integer")} to also bin integer 
#' variables before coercion to factor variables. 
#' 
#' To balance the covariates, \code{missDiag} computes weights using either 
#' the [ebal::ebalance()] function or the [sbw::sbw()] function. These 
#' two packages have to be installed separately by the user. 
#'  
#' To pass parameters to the [sbw::sbw()] or [ebal::ebalance()] use the 
#' functions [param_sbw()] and [param_sbw()] to generate valid parameter lists 
#' and pass these lists via the parameters \code{sbw_param} or 
#' \code{ebal_param}. 
#' 
#' To display information about the computations set verbose to the value 
#' 1 (some information) or 2 (more information when constructing weights). 
#' 
#' To compute the discrepancy statistics, \code{missDiag} relies on the function
#' [cobalt::bal.tab()] from the cobalt package. To pass parameters to this 
#' function use [param_cobalt()] to generate a valid parameter list and 
#' pass this list via the parameter \code{cobalt_param}.
#' 
#' 
#' @return 
#' A single \code{data.frame} with the results for 
#' each imputed dataset and all lhs variables. 
#' 
#' If \code{output_diag=FALSE}, the dataset only includes 
#' the discrepancy statistics comparing y's observed values and 
#' imputed values.
#' 
#' If y is continuous, there will be exactly one row per imputed 
#' dataset. The discrepancy statistics are reported in the 
#' respective columns. For a categorical y cobalt 
#' computes discrepancy statistics for all K categories which means 
#' that there will be K rows for each imputed dataset in the 
#' output. The label for each category is listed in \code{vname}. 
#' 
#' If the default settings are adopted, the dataset includes
#' the following discrepancy statistics for continuous variables: 
#' 
#' \itemize{
#'  \item \code{diff_adj}: Standardized mean difference
#'  \item \code{v_ratio_adj}: Variance ratio 
#'  \item \code{ks_adj}: Kolmogorov–Smirnov statistic
#'  \item \code{ovl_adj}: 1-Overlap coefficient 
#' }
#' 
#' No variance ratio is reported for categorical variables.
#' 
#' If \code{output_diag=TRUE}, the dataset also includes 
#' the balance statistics for all K covariates. 
#' 
#' These balance statistics are useful to diagnose if the fitted 
#' weights are successful in balancing the covariate distribution.
#' 
#' For these diagnostics the column \code{vname} lists 
#' the covariate name (for continuous variables) or the 
#' covariate category (for categorical variables). 
#' 
#' @references
#' Moritz Marbach. 2021. Choosing Imputation Models.
#' 
#' José R Zubizarreta. 2015. Stable Weights that Balance Covariates 
#' for Estimation with Incomplete Outcome Data, Journal of the American 
#' Statistical Association, 110(511): 910-922.
#' 
#' Jens Hainmueller. 2012. Entropy Balancing for Causal Effects: A 
#' Multivariate Reweighting Method to Produce Balanced Samples in 
#' Observational Studies, Political Analysis 20(1): 25–46.
#' 
#' @examples 
#' 
#' # Compare random value imputation 
#' # with predictive mean imputation 
#' # under MCAR
#' 
#' diag_rng <- missDiag( 
#'  original=anes08, 
#'  imputed=anes08_rng, 
#'  verbose = 1,
#'  adjust = 'none',
#'  formula = time ~ .)
#' 
#' diag_pmm <- missDiag( 
#'  original=anes08, 
#'  imputed=anes08_pmm, 
#'  verbose = 1,
#'  adjust = 'none',
#'  formula = time ~ .)
#' 
#' # SMD: 
#' 
#'  mean(diag_pmm$diff_adj)
#'  mean(diag_rng$diff_adj)
#' 
#' # log(Variance ratio)
#'  mean(log(diag_pmm$v_ratio_adj))
#'  mean(log(diag_rng$v_ratio_adj))
#' 
#' # KS
#'  mean(diag_pmm$ks_adj)
#'  mean(diag_rng$ks_adj)
#'
#' # 1-OVL
#'  mean(diag_pmm$ovl_adj)
#'  mean(diag_rng$ovl_adj)
#'
#' 
#' 
#' @importFrom cobalt bal.tab
#' @importFrom stats as.formula formula model.matrix update.formula var
#' @export
missDiag <- function(
  original, 
  imputed, 
  formula=NULL, 
  skip_n=25,
  scale=FALSE,
  adjust='ebal',
  use_imputed_cov = TRUE,
  convert = c("numeric"),
  categories = 3, 
  verbose=0,
  output_diag=FALSE, 
  ebal_param=param_ebal(),
  sbw_param=param_sbw(),
  cobalt_param=param_cobalt()){

  call_ <- sys.call()

  if(!adjust %in% c("none", "sbw", "ebal")) { 
    stop("Parameter 'adjust' must be one of these: none, sbw or ebal") }

  # Preprocessing: Data 
  original <- as.data.frame(original)

  if("mids" %in% class(imputed)){
  
    imputed <- mice::complete(imputed,action='all')
    M <- length(imputed)
  
  } else if("list" %in% class(imputed)) {

    M <- length(imputed)

  } else {

    imputed <- list(as.data.frame(imputed))
    M <- 1

  }

  # Preprocessing: Formula 
  if(!is.null(formula)){

    formulas <- construct_formulas(formula, 
      vars=colnames(original))

  } else {

    vars <- colnames(original)
    vars_na <- apply(is.na(original), 2, sum)>0
    vars <- vars[vars_na]

    dv <- paste(vars, collapse = "|")
    iv <- paste(colnames(original), collapse="+")
    formula <- as.formula(paste0(dv, "~", iv))
    formulas <- construct_formulas(formula, vars=vars)

    }


  out <- list()

  for(m in 1:M){

    if(verbose>0 & M>1) cat("Dataset Nummer:",m,"\n")

    imp <- as.data.frame(imputed[[m]])

    for(f in formulas){

      # Reprocessing X 
      if( use_imputed_cov == FALSE){

        imp_prep <- make_categorical_cov(
           original_df = original,  
           imputed_df = imp, 
           formula = as.formula(f), 
           categories = categories,
           convert = convert)

      } else {

        imp_prep <- imp 

      }

      # Construct weights      
      if(adjust=='sbw'){
        w <- do.call(run_sbw, c(
            list(original= original,
                 imputed = imp_prep, 
                 formula = as.formula(f),
                 skip_n  = skip_n, 
                 scale = scale,
                 verbose = verbose), 
            sbw_param))
      }

      if(adjust=='ebal'){
        w <- do.call(run_ebal, c(
            list(original= original,
                 imputed = imp_prep, 
                 formula = as.formula(f),
                 skip_n  = skip_n, 
                 scale = scale,
                 verbose = verbose), 
            ebal_param))
      }

      if(adjust=='none'){
        if(verbose>0){
          message("No weights constructed." )
        }

        w <- rep(1,nrow(imp_prep))
      }
      
      if(!is.null(w)){

        # Balance 
        yname <- all.vars(f)[1]
        xname <- all.vars(f)

        res <- do.call(bal.tab, c(
            list(x=imp_prep[,xname,drop=FALSE],
                 treat = is.na(original[[yname]]), 
                 weights = w), 
            cobalt_param))$Balance

        res$m <- m 
        res$vname_lhs <- yname
        res$vname <- rownames(res)

        out[[length(out)+1]] <- res

      }

    }
  }

  out <- do.call(rbind,out) 
  rownames(out) <- NULL

  out <- tolower_remove_dot_vars(out)
  out <- out[,!grepl("_un|_threshold", colnames(out))]
  out$diag <- ifelse(mapply(grepl, out$vname_lhs, out$vname), 
    FALSE, TRUE)

  attr(out, 'call') <- call_

  if(output_diag==FALSE){
    out <- subset(out, diag==FALSE)
  }

  return(out)
  }
