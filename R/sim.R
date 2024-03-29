#' Simulate model parameters
#'
#' @description `sim()` simulates model parameters from a multivariate normal or t distribution that are then used by [sim_apply()] to calculate quantities of interest.
#'
#' @param fit a model fit, such as the output of a call to [lm()] or [glm()]. Can be left unspecified if `coefs` and `vcov` are not functions.
#' @param n the number of simulations to run; default is 1000. More is always better but resulting calculations will take longer.
#' @param vcov either a square covariance matrix of the coefficient covariance estimates or a function to use to extract it from `fit`. By default, uses [stats::vcov()] or [insight::get_varcov()] if that doesn't work.
#' @param coefs either a vector of coefficient estimates or a function to use to extract it from `fit`. By default, uses [stats::coef()] or [insight::get_parameters()] if that doesn't work.
#' @param dist a string containing the name of the multivariate distribution to use to draw simulated coefficients. Should be one of `"normal"` (multivariate normal distribution) or `"t({#})"` (multivariate t distribution), where `{#}` corresponds to the desired degrees of freedom (e.g., `"t(100)"`). If `NULL`, the right distribution to use will be determined based on heuristics; see Details.
#'
#' @return
#' A `clarify_sim` object, which has the following components:
#'  \item{sim.coefs}{a matrix containing the simulated coefficients with a column for each coefficient and a row for each simulation}
#'  \item{coefs}{the original coefficients extracted from `fit` or supplied to `coefs`.}
#'  \item{vcov}{the covariance matrix of the coefficients extracted from `fit` or supplied to `vcov`}
#'  \item{fit}{the original model fit supplied to `fit`}
#' The `"dist"` attribute contains `"normal"` if the coefficients were sampled from a multivariate normal distribution and `"t(df)"` if sampled from a multivariate t distribution. The `"clarify_hash"` attribute contains a unique hash generated by [rlang::hash()].
#'
#' @details When `dist` is `NULL`, `sim()` samples from a multivariate normal or t distribution depending on the degrees of freedom extracted from `insight::get_df(., type = "wald")`. If `Inf`, a normal distribution will be used; otherwise, a t-distribution with the returned degrees of freedom will be used. Models not supported by `insight` will use a normal distribution.
#'
#' When a multivariate normal is used, it is sampled from with means equal to the estimated coefficients and the parameter covariance matrix as the covariance matrix using [mvnfast::rmvn()]. When a multivariate t distribution is used, it is sampled from with means equal to the estimated coefficients and scaling matrix equal to `cov*(df - 2)/df`, where `cov` is the parameter covariance matrix and `df` is the residual degrees of freedom for the model, using [mvnfast::rmvt()].
#'
#' @seealso
#' * [misim()] for simulating model coefficients after multiple imputation
#' * [sim_apply()] for applying a function to each set of simulated coefficients
#' * [sim_ame()] for computing average marginal effects in each simulation draw
#' * [sim_setx()] for computing marginal predictions and first differences at typical values in each simulation draw
#' * [sim_adrf()] for computing average dose-response functions in each simulation draw
#'
#' @examples
#'
#' data("lalonde", package = "MatchIt")
#' fit <- lm(re78 ~ treat * (age + race + nodegree + re74), data = lalonde)
#'
#' # Simulate coefficients
#' s <- sim(fit)
#' s
#'
#' ## Could also use a robust covariance matrix, e.g.,
#' s <- sim(fit, vcov = "HC3")
#'
#' # Simulated coefficients assuming a normal distribution
#' # for coefficients; default for `lm` objects is a t-
#' # distribution
#' s <- sim(fit, dist = "normal")
#' s
#'
#' @export
sim <- function(fit,
                n = 1e3,
                vcov = NULL,
                coefs = NULL,
                dist = NULL) {

  if (missing(fit)) fit <- NULL

  if (!is.null(fit)) {
    if (!insight::is_regression_model(fit)) {
      .wrn("`fit` was not detected to be a regression model; proceed with caution")
    }
    # if (insight::is_mixed_model(fit)) {
    #   .wrn("`sim()` may not fully support models with random effects; proceed with caution")
    # }
  }

  chk::chk_count(n)

  coef_supplied <- {
    if (is.null(coefs)) "null"
    else if (is.function(coefs)) "fun"
    else if (check_valid_coef(coefs)) "num"
    else {
      .err("`coefs` must be a vector of coefficients or a function that extracts one from `fit`")
    }
  }

  vcov_supplied <- {
    if (is.null(vcov)) "null"
    else if (is.matrix(vcov)) "num"
    else "marginaleffects_code"
  }

  coefs <- process_coefs(coefs, fit, coef_supplied)

  vcov <- process_vcov(vcov, fit, vcov_supplied)

  check_coefs_vcov_length(vcov, coefs, vcov_supplied, coef_supplied)

  sampler <- get_sampling_dist(fit, dist)

  out <- list(sim.coefs = sampler(n, coefs, vcov),
              coefs = coefs,
              vcov = vcov,
              fit = fit)

  attr(out, "dist") <- attr(sampler, "dist")
  attr(out, "use_fit") <- !is.null(fit)
  attr(out, "sim_hash") <- rlang::hash(out$sim.coefs)
  class(out) <- "clarify_sim"

  out
}

#' @export
print.clarify_sim <- function(x, ...) {
  cat("A `clarify_sim` object\n")
  cat(sprintf(" - %s coefficients, %s simulated values\n", ncol(x$sim.coefs), nrow(x$sim.coefs)))
  cat(sprintf(" - sampled distribution: multivariate %s\n", attr(x, "dist")))
  if (!is.null(x$fit)) {
    cat(" - original fitting function call:\n\n")
    print(insight::get_call(x$fit))
  }

  invisible(x)
}

#Returns a function that generates random variates, with arguments
#`n`, `mu`, and `cov`; name of distribution is stored in attr(., "dist")
get_sampling_dist <- function(fit = NULL, dist = NULL) {

  if (!is.null(dist)) {
    chk::chk_string(dist)
    dist <- tolower(dist)
    if (startsWith(dist, "t(") && endsWith(dist, ")")) {
      df <- substr(dist, 3, nchar(dist) - 1)
      if (nchar(df) == 0 || anyNA(suppressWarnings(df <- as.numeric(df))) || !chk::vld_number(df)) {
        .err("when `dist` is supplied as t({#}), `{#}` must be a number")
      }
      df <- as.numeric(df)
      dist <- "t"
    }
    else if (!anyNA(pmatch(dist, "normal"))) {
      dist <- "normal"
    }
    else {
      .err("`dist` must be \"normal\" or \"t({#})\", where `{#}` corresponds to the desired degrees of freedom")
    }
  }
  else if (is.null(fit)) {
    dist <- "normal"
  }
  else {
    df <- get_df(fit)

    if (any(is.finite(df)) && all(df > 0)) dist <- "t"
    else dist <- "normal"
  }

  f <- {
    if (dist == "t")
      function(n, mu, cov) {
        sigma <- cov * (df - 2) / df
        #Need pivoted cholesky for when cov isn't PSD (sometimes true for fixed effects models)
        ch <- suppressWarnings(chol(sigma, pivot = TRUE))
        piv <- attr(ch, "pivot")
        x <- mvnfast::rmvt(n, mu = mu[piv], sigma = ch, isChol = TRUE, df = df, kpnames = TRUE)
        x[, order(piv), drop = FALSE]
      }
    else
      function(n, mu, cov) {
        #Need pivoted cholesky for when cov isn't PSD (sometimes true for fixed effects models)
        ch <- suppressWarnings(chol(cov, pivot = TRUE))
        piv <- attr(ch, "pivot")
        x <- mvnfast::rmvn(n, mu = mu[piv], sigma = ch, isChol = TRUE, kpnames = TRUE)
        x[, order(piv), drop = FALSE]
      }
  }

  attr(f, "dist") <- if (dist == "t") sprintf("t(%s)", df) else dist

  f
}

#Extracts coefs based on given inputs
process_coefs <- function(coefs, fit = NULL, coef_supplied) {
  if (coef_supplied == "null") {
    if (is.null(fit)) {
      .err("`coefs` must be supplied when `fit` is not specified")
    }
    coefs <- marginaleffects::get_coef(fit)
    if (!check_valid_coef(coefs)) {
      .err("a valid set of coefficients could not be extracted automatically; please supply coefficients to the `coefs` argument and a covariance matrix to the `vcov` argument")
    }
  }
  if (coef_supplied == "fun") {
    if (is.null(fit)) {
      .err("`fit` must be supplied when `coefs` is a function")
    }

    coefs <- try_chk(coefs(fit))
    if (!check_valid_coef(coefs)) {
      .err("the output of the function supplied to `coefs` must be a numeric vector")
    }
  }
  else if (coef_supplied == "num") {
    #do nothing
  }

  if (anyNA(coefs) || any(!is.finite(coefs))) {
    .err("the coefficients cannot contain `NA` or non-finite values. This can occur with rank-deficient fits")
  }

  coefs
}

#Extracts vcov based on given inputs
process_vcov <- function(vcov, fit = NULL, vcov_supplied) {
  if (vcov_supplied == "null") {
    if (is.null(fit)) {
      .err("`vcov` must be supplied when `fit` is not specified")
    }
    vcov <- marginaleffects::get_vcov(fit)
    if (!check_valid_vcov(vcov)) {
      .err("a valid covariance matrix could not be extracted automatically; please supply an argument to `vcov`")
    }
  }
  else if (vcov_supplied == "num") {
    if (!check_valid_vcov(vcov)) {
      .err("when supplied as a matrix, `vcov` must be a square, symmetric, numeric matrix")
    }
  }
  else {
    if (is.null(fit)) {
      .err("`fit` must be supplied when `vcov` is a not supplied as a matrix")
    }

    vcov <- marginaleffects::get_vcov(fit, vcov)
    if (!check_valid_vcov(vcov)) {
      .err("a valid covariance matrix could not be extracted using the argument supplied to `vcov`")
    }
  }

  if (anyNA(vcov) || any(!is.finite(vcov))) {
    .err("the covariance matrix cannot contain `NA` or non-finite values. This can occur with rank-deficient fits")
  }

  vcov
}
