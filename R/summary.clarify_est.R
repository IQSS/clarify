#' Inference for `clarify_est` objects
#'
#' `summary()` tabulates the estimates and confidence intervals and (optionally) p-values from a `clarify_est` object. `confint()` computes confidence intervals.
#'
#' @param object,x a `clarify_est` object; the output of a call to [sim_apply()] or its wrappers.
#' @param parm a vector of the names or indices of the estimates to plot. If unspecified, all estimates will be displayed.
#' @param level the confidence level desired. Default is .95 for 95% confidence intervals.
#' @param method the method used to compute p-values and confidence intervals. Can be `"wald"` to use a Normal approximation or `"quantile"` to use the simulated sampling distribution (default). See Details. Abbreviations allowed.
#' @param null the values of the parameters under the null hypothesis for the p-value calculations. Should have length equal to the number of quantities estimated, or one, in which case it will be recycled. Set values to `NA` to omit p-values for those quantities. When all values are `NA`, the default, no p-values are produced.
#' @param ci `logical`; whether to display confidence interval limits for the estimates. Default is `TRUE`.
#' @param ... for `plot()`, further arguments passed to [ggplot2::geom_density()].
#'
#' @return For `summary()`, a `summary.clarify_est` object, which is a matrix containing the coefficient estimates, standard errors, test statistics, p-values, and confidence intervals. Not all columns will be present depending on the arguments supplied to `summary()`.
#'
#' For `confint()`, a matrix containing the confidence intervals for the requested quantities.
#'
#' For `plot()`, a `ggplot` object.
#'
#' @details `summary()` uses the estimates computed from the original model as its estimates and uses the simulated parameters for inference only.
#'
#' When `method = "wald"`, the standard deviation of the simulation estimates is used as the standard error, which is used in the z-statistics and the confidence intervals. The p-values and confidence intervals are valid only when the sampling distribution of the resulting statistic is normal (which can be assessed using `plot()`). When `method = "quantile"`, the confidence interval is calculated using the quantiles of the simulation estimates corresponding to `level`, and the p-value is calculated as twice the proportion of simulation estimates less than or greater than `null`, whichever is smaller; this is equivalent to inverting the confidence interval but is only truly valid when the true sampling distribution is only a location shift from the sampling distribution under the null hypothesis and should therefore be interpreted with caution. Using `"method = "quantile"` (the default) is recommended because the confidence intervals will be valid even if the sampling distribution is not Normally distributed. The precision of the p-values and confidence intervals depends on the number of simulations requested (the value of `n` supplied to [sim()]).
#'
#' The plots are produced using [ggplot2::geom_density()] and can be customized with \pkg{ggplot2} functions.
#'
#' @seealso
#' * [sim_apply()] for applying a function to each set of simulated coefficients
#'
#' @examples
#' data("lalonde", package = "MatchIt")
#' fit <- glm(I(re78 > 0) ~ treat + age + race + nodegree + re74,
#'           data = lalonde)
#'
#' s <- sim(fit, n = 100)
#'
#' # Compute average marginal means for `treat`
#' est <- sim_ame(s, var = "treat", verbose = FALSE)
#' coef(est)
#'
#' # Compute average marginal effects on risk difference
#' # (RD) and risk ratio (RR) scale
#' est <- transform(est,
#'                  RD = `E[Y(1)]` - `E[Y(0)]`,
#'                  RR = `E[Y(1)]` / `E[Y(0)]`)
#'
#' # Compute confidence intervals and p-values,
#' # using given null values for computing p-values
#' summary(est, null = c(NA, NA, 0, 1))
#'
#' # Same tests using normal approximation
#' summary(est, null = c(NA, NA, 0, 1),
#'         normal = TRUE)
#'
#' # Plot the RD and RR
#' plot(est, parm = c("RD", "RR"))
#'
#' @export
summary.clarify_est <- function(object,
                                parm,
                                level = .95,
                                method = "quantile",
                                null = NA,
                                ...) {
  ans <- list()

  if (is.null(attr(object, "original")) ||
      is.null(ncol(object)) ||
      !identical(ncol(object), length(attr(object, "original"))) ||
      !identical(names(object), names(attr(object, "original")))) {
    .err("the `clarify_est` object is malformed, possibly due to tampering")
  }

  original_est <- coef(object)

  parm <- process_parm(object, parm)
  if (anyNA(parm)) {
    .err("`parm` must be a numeric or character vector identifying the estimates to summarize")
  }

  if (length(null) == 0 || (is.atomic(null) && all(is.na(null)))) {
    null <- NA_real_
  }
  chk::chk_numeric(null)

  if (length(null) == 1) null <- rep(null, length(parm))
  test <- !all(is.na(null))

  if (test) {
    if (!chk::vld_length(null, length(parm))) {
      .err(sprintf("`null` must have length 1 or length equal to the number of quantities estimated (%s)",
                   length(parm)))
    }
  }

  nas <- anyNA(object[, parm])
  if (nas) chk::wrn("NA values present among the estimates")

  ans <- cbind(Estimate = original_est[parm],
               confint(object, parm = parm, level = level,
                       method = method))

  if (test) {
    se <- z <- p <- NULL
    object <- drop_sim_class(object)

    if (method == "wald") {
      se <- apply(object[, parm, drop = FALSE], 2, sd, na.rm = TRUE)
      z <- (original_est[parm] - null) / se
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)
      p[p < .Machine$double.eps] <- 0
    }
    else if (method == "quantile") {
      ns <- {
        if (nas) colSums(!is.na(object[, parm, drop = FALSE]))
        else rep(nrow(object), length(parm))
      }
      p <- vapply(seq_along(parm), function(i) {
        if (is.na(null[i])) return(NA_real_)
        x <- object[, parm[i]]
        2 * min(sum(x < null[i], na.rm = nas),
                sum(x > null[i], na.rm = nas)) / ns[i]
      }, numeric(1L))
    }

    ans <- cbind(ans,
                 `Std. Error` = se,
                 `Z value` = z,
                 `P-value` = p)
  }

  class(ans) <- c("summary.clarify_est", class(ans))
  ans
}

#' @exportS3Method print summary.clarify_est
print.summary.clarify_est <- function(x, digits = 3, ...) {
  chk::chk_whole_number(digits)
  stats::printCoefmat(x, digits = digits,
                      cs.ind = c(1:3, (4)["Std. Error" %in% colnames(x)]),
                      tst.ind = which(colnames(x) == "Z value"),
                      has.Pvalue = "P-value" %in% colnames(x),
                      na.print = ".")
  invisible(x)
}

#' @exportS3Method confint clarify_est
#' @rdname summary.clarify_est
confint.clarify_est <- function(object,
                                parm,
                                level = .95,
                                method = "quantile",
                                ...) {

  chk::chk_number(level)
  if (level <= .5 || level >= 1) {
    .err("`level` must be between .5 and 1")
  }

  chk::chk_string(method)
  method <- match_arg(method, c("quantile", "wald"))

  parm <- process_parm(object, parm)
  if (anyNA(parm)) {
    .err("`parm` must be a numeric or character vector identifying the estimates for which to compute confidence intervals")
  }

  est_names <- names(object)[parm]

  a <- (1 - level) / 2
  a <- c(a, 1 - a)

  pct <- fmt.prc(a, 3)

  ci <- array(NA, dim = c(length(parm), 2L),
              dimnames = list(est_names, pct))

  nas <- anyNA(object[, parm])
  if (nas) chk::wrn("`NA` values present among the estimates")

  if (method == "wald") {
    cf <- coef(object)
    fac <- qnorm(a)
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + outer(ses, fac, "*")
  }
  else if (method == "quantile") {
    ci[] <- t(apply(object[, parm, drop = FALSE], 2, quantile, probs = a,
                    na.rm = nas))
  }

  ci
}

#' @exportS3Method coef clarify_est
coef.clarify_est <- function(object, ...) {
  attr(object, "original")
}

#' @exportS3Method vcov clarify_est
vcov.clarify_est <- function(object, ...) {
  cov(drop_sim_class(object))
}
