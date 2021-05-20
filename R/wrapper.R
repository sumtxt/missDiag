run_sbw <- function(
    original,
    imputed,
    formula,
  bal_alg,
  bal_tol,
  bal_std,
  bal_gri,
  bal_sam,
  sol_nam,
  skip_n,
  scale,
  verbose,
  fit.obj=FALSE){

  m <- construct_m(df=original,formula=formula)

  if(sum(m)<skip_n) {
   if(verbose>0){
      message("Formula: ", formula_string(formula) )
      message("Number of imputed values: ", sum(m) )
      message("Skipping." )
      }
    return(NULL)
   }

  X_Xbar <- construct_x(df=imputed,formula=formula,m=m,scale=scale,verbose=verbose)
  X <- X_Xbar[['X']]
  Xbar <- X_Xbar[['Xbar']]
  
  Xbar_names <- names(Xbar)
  X <- as.data.frame(X)

  sbw_out <- sbw::sbw(dat=X,
               bal=list(
                bal_cov=Xbar_names,
                bal_alg=bal_alg,
                bal_tol=bal_tol,
                bal_std=bal_std,
                bal_gri=bal_gri,
                bal_sam=bal_sam),
               wei = list(
                wei_sum=TRUE, 
                wei_pos=TRUE),
               sol=list(
                sol_nam=sol_nam,
                sol_dis=(verbose>1)),
               par=list(
                par_est="aux", 
                par_tar=Xbar))

  if(verbose>1){
    sbw::summarize(sbw_out)
  }

  if(fit.obj){
    return(sbw_out)
  } else {
    w <- rep(NA,length(m))
    w[m==0] <- sbw_out$dat_weights$sbw_weights*sum(m)
    w[m==1] <- 1
    return(w) 
    }


  }


run_ebal <- function(
    original,
    imputed,
    formula,
  coefs,
  max.iterations,
  base.weight,
  constraint.tolerance,
  norm.constant,
  max.weight,
  min.weight,
  max.trim.iterations,
  max.weight.increment,
  min.weight.increment,
  trim, 
  skip_n,
  scale,
  verbose,
  fit.obj=FALSE){

  m <- construct_m(df=original,formula=formula)

  if(sum(m)<skip_n) {
   if(verbose>1){
      message("Formula: ", formula_string(formula) )
      message("Number of imputed values: ", sum(m) )
      message("Skipping." )
      }
    return(NULL)
   }

  X_Xbar <- construct_x(df=imputed,formula=formula,m=m,scale=scale,verbose=verbose)
  X <- X_Xbar[['X']]
  Xbar <- X_Xbar[['Xbar']]

  # Modified code from ebal::ebalance() >>>
 
  if (length(max.iterations) != 1 ) {
    stop("length(max.iterations) != 1")
    }

  if (length(constraint.tolerance) != 1 ) {
    stop("length(constraint.tolerance) != 1")
    }
    
  # set up elements
  ntreated  <- sum(m==1)
  ncontrols <- sum(m==0)

  if (is.null(base.weight)) {
    base.weight = rep(1, ncontrols)
    }

  if ( length(base.weight) !=  ncontrols) {
    stop("length(base.weight) !=  number of controls  sum(Treatment==0)")
    }


  #co.x <- X[Treatment==0,]
  #co.x <- cbind(rep(1,ncontrols),co.x)

  co.x <- X
  co.x <- cbind(rep(1,ncontrols),co.x)

  if(qr(co.x)$rank != ncol(co.x)){
    stop("collinearity in covariate matrix for controls (remove collinear covariates)")
    }

  #tr.total <- apply(as.matrix(X[Treatment==1,]),2,sum)
  tr.total <- Xbar*ntreated

  if (is.null(norm.constant)) {
    norm.constant <- ntreated
    }

  if (length(norm.constant) != 1) {
    stop("length(norm.constant) != 1")
    }

  tr.total <- c(norm.constant,tr.total)

  if(is.null(coefs)) {
    coefs = c(log(tr.total[1]/sum(base.weight)),rep(0,(ncol(co.x)-1)))
    }
   
  if(length(coefs) != ncol(co.x)) {
    stop("coefs needs to have same length as number of covariates plus one")
    }
   
  eb.out <- ebal::eb(tr.total=tr.total,
               co.x=co.x,
               coefs=coefs,
               base.weight=base.weight,
               max.iterations=max.iterations,
               constraint.tolerance=constraint.tolerance,
               print.level=verbose)

  ebal_out <- list(
            target.margins = tr.total,
            co.xdata = co.x,
            w=eb.out$Weights.ebal,
            coefs=eb.out$coefs,
            maxdiff=eb.out$maxdiff,
            norm.constant = norm.constant,
            constraint.tolerance=constraint.tolerance,
            max.iterations=max.iterations,
            base.weight=base.weight,
            print.level=verbose,
            converged=eb.out$converged
      )
        
  class(ebal_out) <- "ebalance"

  # <<< end 

  if(trim){

    ebal_out <- ebal::ebalance.trim(ebal_out, 
      max.weight = max.weight,
      min.weight = min.weight, 
      max.trim.iterations = max.trim.iterations,
      max.weight.increment = max.weight.increment,
      min.weight.increment = min.weight.increment,
      print.level = verbose)

  }

  if(fit.obj){
    return(ebal_out)
  } else {
    w <- rep(NA,length(m))
    w[m==0] <- ebal_out$w
    w[m==1] <- 1
    return(w) 
    }
  }




