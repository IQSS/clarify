#Functions for extracting information from models

# Get the coefficients from a model as a vector
get_coefs <- function(fit) {

  b <- try(marginaleffects::get_coef(fit), silent = TRUE)

  if (!check_valid_coef(b)) {
    .err("`sim()` was unable to extract a valid set of coefficients from the model fit; please supply coefficients to the `coefs` argument and a covariance matrix to the `vcov` argument")
  }

  return(b)
}

# Get the covariance from a model
get_vcov <- function(fit, vcov = NULL) {
  v <- try(marginaleffects::get_vcov(fit, vcov), silent = TRUE)

  if (!check_valid_vcov(v)) {
    .err("`sim()` was unable to extract a valid covariance matrix from the model fit; please supply a covariance matrix to the `vcov` argument")
  }

  return(v)
}

# Get the model degrees of freedom
## Assesses whether the model is linear and fit with OLS; if not,
## returns Inf. Linear models fit with MLE get Inf.
get_df <- function(fit) {

  if (insight::is_model_supported(fit)) {
    statistic <- insight::find_statistic(fit)
    if (statistic == "chi-squared statistic") {
      df <- Inf
    }
    else {
      df <- insight::get_df(fit, type = "wald", statistic = statistic)
    }
  }
  else {
    df <- Inf
  }

  df
}
