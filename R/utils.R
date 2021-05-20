expand_rhs_dot <- function(f,all_vars){

  if( sum(all.vars(f) == ".") == 0 ){
  
    return(f)
  
  } else {

    dv <- all.vars(f)[1]
    iv <- paste(all_vars, collapse="+")
    f <- as.formula(paste0(dv, "~", iv))
    return(f)

  }
  }

#' @import Formula
construct_formulas <- function(f, vars){

  if(is.list(f)){

    formulas <- lapply(f, as.formula)
    formulas <- lapply(f, expand_rhs_dot, all_vars=vars)

  } else {

    f <- as.Formula(f)
    K <- length(f)[1]

    formulas <- list()

    for(i in 1:K){
      
       nf <- formula(as.Formula(f),lhs=i)
       nf <- expand_rhs_dot(nf, all_vars=vars)
       nf <- update.formula(nf, 
          paste(" . ~ . -", all.vars(nf)[1]),
          evaluate=FALSE)

      formulas[[i]] <- nf

      }

  }

  return(formulas)
  }


# construct_formulas <- function(f){
# 
#   if(is.list(f)){
# 
#     formulas <- lapply(f, as.formula)
# 
#   } else {
# 
#     f <- as.Formula(f)
#     K <- length(f)[1]
# 
#     formulas <- list()
#     for(i in 1:K){
#        nf <- formula(as.Formula(f),lhs=i)
#        nf <- update.formula(nf, 
#           paste(" . ~ . -", all.vars(nf)[1]),
#           evaluate=FALSE)
#       formulas[[i]] <- nf
#       }
# 
#   }
# 
#   return(formulas)
#   }


formula_string <- function(f){
  f <- paste(deparse(f),collapse="")
  f <- gsub("\\s+", " ", f)
  return(f)
  }

construct_x <- function(df,formula,m,scale,verbose){

  X <- model.matrix(formula,df)
  X <- X[,colnames(X)!="(Intercept)", drop=FALSE]
  if(scale) X <- scale(X)

  # mean among observations with imputed values
  Xbar <- colMeans(X[m==1,,drop=FALSE])

  # observations without imputed values
  X <- X[m==0,,drop=FALSE]
  Xk <- ncol(X)

  X_hasvar <- apply(X, 2, var)!=0 
  X <- X[,X_hasvar,drop=FALSE]
  Xbar <- Xbar[X_hasvar]

  if(verbose>0){
    message("Formula: ", formula_string(formula) )
    message("Number of covariates: ", Xk )
    message("Covariates w/o variation: ", sum(!X_hasvar) )
    message("Final number of covariates: ", ncol(X))
    }

  return(list(X=X,Xbar=Xbar))
  }

construct_m <- function(df,formula){
  yname <- all.vars(formula)[1]
  return(as.numeric(is.na(df[,yname])))
  }


tolower_remove_dot <- function(x){
  return(gsub("\\.","_",tolower(x)))
  }

tolower_remove_dot_vars <- function(df){
  colnames(df) <- tolower_remove_dot(colnames(df))
  return(df)
}


make_categorical_cov <- function(
  original_df, 
  imputed_df, 
  formula, 
  categories, 
  convert){

  is_numeric <- sapply(original_df, is_var_type, type=convert)

  has_na <- apply(is.na(original_df), 2, sum)>0
  num_vars <- names(is_numeric)[is_numeric==TRUE & has_na==TRUE]
  na_vars <- names(has_na)[has_na==TRUE]

  yname <- all.vars(formula)[1]

  # Only retain imputations for y
  tmp <- original_df
  tmp[[yname]] <- imputed_df[[yname]]

  # Numeric variables to categorical
  for(num_var in num_vars){
    if(num_var != yname){
      tmp[[num_var]] <- make_categorical(tmp[[num_var]],C=categories+1)
    }
  }

  # Explicat the missing values 
  for(na_var in na_vars){
    if(na_var != yname){
      tmp[[na_var]] <- explicat_na(tmp[[na_var]])
    }
  }
  
  return(tmp)
  }


#' @importFrom stats quantile
make_categorical <- function(x, C){
  q <- quantile(x, probs=seq(0,1,length=C), na.rm=TRUE)
  return(cut(x, breaks=q, ordered_result=TRUE, include.lowest=TRUE))
  }

# Check if x is of type "type"
is_var_type <- function(x, type){
  return( sum(class(x) %in% type)>0 )
}

# Transform NA in new category
explicat_na <- function(x, na_level="(Missing)" ){
    if( !is.factor(x) ) {
      x <- factor(x)
    }
    is_missing <- is.na(x)
    if (any(is_missing)) {
        levels(x) <- c(levels(x), na_level)
        x[is_missing] <- na_level
    }
    return(x)
}
