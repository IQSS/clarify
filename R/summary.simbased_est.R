#' Inference for `simbased_est` objects
#'
#' `summary()` tabulates the estimates and simulation-based confidence intervals and (optionally) p-values from a `simbased_est` object.
#'
#' @param object a `simbased_est` object; the output of a call to [sim_apply()] or its wrappers.
#' @param est a vector of the names or indices of the estimates to plot. If unspecified, all estimates will be plotted.
#' @param alpha the \eqn{\alpha} used to compute the confidence interval. Default is .05 for 95% confidence intervals.
#' @param normal `logical`; whether to compute p-values and confidence intervals using a normal approximation (`TRUE`) or the simulated sampling distribution (`FALSE`; default). See Details.
#' @param null the values of the parameters under the null hypothesis for the p-value calculations. Should have length equal to the number of quantities estimated, or one, in which case it will be recycled. Set values to `NA` to omit p-values for those quantities. When all values are `NA`, the default, no p-values are produced.
#' @param ... ignored.
#'
#' @return A `summary.simbased_est` object, which is a matrix containing the coefficient estimates, standard errors, test statistics, p-values, and confidence intervals. Not all columns will be present depending on the arguments supplied to `summary()`.
#'
#' @details `summary()` uses the estimates computed from the original model as its estimates and uses the simulated parameters for inference only. When `normal = TRUE`, the standard deviation of the simulation estimates is used as the standard error, which is used in the t- or z- statistic and the confidence interval. The p-values and confidence intervals are valid only when the sampling distribution of the resulting statistic is normal (which can be assessed using [plot.simbased_est()]. When `normal = FALSE`, the confidence interval is calculated using the `alpha/2` and `1 - alpha/2` quantiles of the simulation estimates, and the p-value is calculated as twice the proportion of simulation estimates less than or greater than `null`, whichever is smaller; this is equivalent to inverting the confidence interval but is only truly valid when the true sampling distribution is only a location shift from the sampling distribution under the null hypothesis and should therefore be interpreted with caution. Using `normal = FALSE` (the default) is recommended because the confidence intervals will be valid even if the sampling distribution is not normally distributed. The precision of the p-values and confidence intervals depends on the number of simulation requested (the value of `n` supplied to [sim()]).
#'
#' @seealso [plot.simbased_est()] for plotting the simulation distribution of the estimates.
#'
#' @examples
#' data("lalonde", package = "MatchIt")
#' fit <- glm(I(re78 > 0) ~ treat + age + race + nodegree + re74,
#'           data = lalonde)
#'
#' s <- sim(fit, n = 100)
#'
#' # Compute average marginal means for `treat`
#' est <- sim_ame(s, var = "treat")
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
#' @export
summary.simbased_est <- function(object, est, alpha = .05, normal = FALSE, null = NA, ...) {
  ans <- list()
  original_est <- coef(object)

  if (missing(est)) est <- seq_len(ncol(object))

  if (is.character(est)) {
    ind <- match(est, colnames(object))
    if (anyNA(ind)) {
      chk::err(sprintf("%s not the %s of any estimated quantities.",
                       word_list(est[is.na(ind)], is.are = TRUE, quotes = TRUE),
                       ngettext(sum(is.na(ind)), "name", "names")))
    }
    est <- ind
  }
  else if (is.numeric(est)) {
    chk::chk_whole_numeric(est)
    if (length(est) > 1 && ncol(object) == 1) {
      chk::wrn("ignoring `est` because only one estimate is available")
    }
    if (any(est < 1) || any(est > ncol(object))) {
      chk::err(sprintf("all values in `est` must be between 1 and %s", ncol(object)))
    }
  }
  else {
    chk::err("`est` must be a numeric or character vector identifiying the estimates to summarize")
  }

  est_names <- colnames(object)[est]

  pct <- fmt.prc(c(alpha/2, 1-alpha/2))
  ci <- matrix(nrow = length(est), ncol = 2,
               dimnames = list(est_names, paste("CI", pct)))

  chk::chk_number(alpha)
  if (alpha <= 0 || alpha >= .5) {
    chk::err("`alpha` must be between 0 and .5")
  }

  chk::chk_flag(normal)

  if (length(null) == 0 || (is.atomic(null) && all(is.na(null)))) {
    null <- NA_real_
  }
  chk::chk_numeric(null)

  if (length(null) == 1) null <- rep(null, length(est))
  test <- !all(is.na(null))

  if (test) {
    if (!chk::vld_length(null, length(est))) {
      chk::err(sprintf("`null` must have length 1 or length equal to the number of quantities estimated (%s)",
                       length(est)))
    }
  }

  nas <- anyNA(object[,est])
  if (nas) chk::wrn("NA values present among the estimates")

  se <- z <- p <- NULL
  if (normal) {
    se <- apply(object[,est, drop = FALSE], 2, sd, na.rm = TRUE)
    if (test) {
      z <- (original_est[est] - null)/se
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)
      p[p < .Machine$double.eps] <- 0
    }
    zcrit <- qnorm(c(alpha/2, 1-alpha/2))
    ci[,1] <- original_est[est] + se*zcrit[1]
    ci[,2] <- original_est[est] + se*zcrit[2]
  }
  else {
    if (test) {
      ns <- {
        if (nas) colSums(!is.na(object[,est, drop = FALSE]))
        else rep(nrow(object), length(est))
      }
      p <- vapply(seq_along(est), function(i) {
        if (is.na(null[i])) return(NA_real_)
        x <- object[, est[i]]
        2 * min(sum(x < null[i], na.rm = nas),
                sum(x > null[i], na.rm = nas))/ns[i]
      }, numeric(1L))
    }
    ci[] <- t(apply(object[,est, drop = FALSE], 2, quantile, probs = c(alpha/2, 1-alpha/2),
                    na.rm = nas))
  }

  ans <- cbind(Estimate = original_est[est],
               ci,
               `Std. Error` = if (test) se,
               `Z value` = if (test) z,
               `P-value` = if (test) p)

  class(ans) <- c("summary.simbased_est", class(ans))
  ans
}

#' @export
print.summary.simbased_est <- function(x, digits = 3, ...) {
  chk::chk_whole_number(digits)
  stats::printCoefmat(x, digits = digits,
                      cs.ind = c(1:3, (4)["Std. Error" %in% colnames(x)]),
                      tst.ind = which(colnames(x) == "Z value"),
                      has.Pvalue = "P-value" %in% colnames(x),
                      na.print = ".")
  invisible(x)
}
