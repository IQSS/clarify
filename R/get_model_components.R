#Functions for extracting information from models

# Get the coefficients from a model as a vector
## Uses coef() when possible, and if not, insight::get_parameters().
get_coefs <- function(fit) {

  b <- try(coef(fit), silent = TRUE)

  if (!is_error(b)) {
    b <- try(transform_coefs(b), silent = TRUE)
  }

  if (!check_valid_coef(b)) {
    if (insight::is_model_supported(fit)) {
      b <- insight::get_parameters(fit, summary = TRUE, component = "all")
      b <- try(transform_coefs(b, insight_used = TRUE), silent = TRUE)
    }
  }

  if (!check_valid_coef(b)) {
    chk::err("`sim()` was unable to extract a valid set of coefficients from the model fit; please supply coefficients to the `coefs` argument and a covariance matrix to the `vcov` argument")
  }

  return(b)
}

# Get the covariance from a model
## Uses vcov() when possible, and if not, insight::get_varcov().
get_vcov <- function(fit) {
  check <- TRUE
  if (check_valid_vcov(v <- try(vcov(fit), silent = TRUE))) {
    check <- FALSE
    #v already extracted
  }
  else if (insight::is_model_supported(fit)) {
    v <- try(insight::get_varcov(fit, component = "all"), silent = TRUE)
  }

  if (check && !check_valid_vcov(v)) {
    chk::err("`sim()` was unable to extract a valid covariance matrix from the model fit; please supply a covariance matrix to the `vcov` argument")
  }

  return(v)
}

# Get the model degrees of freedom
## Assesses whether the model is linear and fit with OLS; if not,
## returns Inf. Linear models fit with MLE get Inf.
get_df <- function(fit) {

  if (inherits_any(fit, c("glm", "betareg"))) {
    df <- Inf
  }
  else if (inherits(fit, "fixest")) {
    if (identical(fit[["method"]], "feols")) {
      df <- fit$nobs - fit$nparams
    }
    else {
      df <- Inf
    }
  }
  else {
    df <- try(stats::df.residual(fit), silent = TRUE)
    if (is_error(df)) df <- Inf
  }

  df
}

#Convert coefs from original shape to vector for simulation; helper function
#for get_coefs()
transform_coefs <- function(coefs, insight_used = FALSE) {

  #Transform coefs
  if (insight_used) {
    if ("Response" %in% names(coefs)) {
      b <- setNames(unname(coefs$Estimate), paste(coefs$Response, coefs$Paramater, sep = ":"))
    }
    else {
      b <- setNames(unname(coefs$Estimate), coefs$Parameter)
    }
  }
  else if (is.matrix(coefs)) {
    b <- unlist(lapply(colnames(coefs), function(i) {
      setNames(b[,i], paste(i, rownames(b), sep = ":"))
    }))
  }
  else {
    b <- coefs
  }

  return(b)
}
