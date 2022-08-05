#' Inference for `simbased_est` objects
#'
#' `summary()` tabulates the estimates and simulation-based confidence intervals and (optionally) p-values from a `simbased_est` object.
#'
#' @param object a `simbased_est` object; the output of a call to [sim_apply()].
#' @param alpha the \eqn{\alpha} used to compute the confidence interval. Default is .05 for 95% confidence intervals.
#' @param normal `logical`; whether to compute p-values and confidence intervals using a normal approximation (`TRUE`) or the simulated sampling distribution (`FALSE`; default). See Details.
#' @param null the values of the parameters under the null hypothesis for the p-value calculations. Should have length equal to the number of quantities estimated, or one, in which case it will be recycled. Set values to `NA` to omit p-values for those quantities. When all values are `NA`, the default, no p-values are produced.
#' @param ... ignored.
#'
#' @return A `summary.simbased_est` object, which contains the following components:
#' \item{coefficients}{a matrix containing the coefficient estimates, standard errors, test statistics, p-values, and confidence intervals. Not all columns will be present depending on the arguments supplied to `summary()`.}
#'
#'
#' @details `summary()` uses the estimates computed from the original model as its estimates and uses the simulated parameters for inference only. When `normal = TRUE`, the standard deviation of the simulation estimates is used as the standard error, which is used in the t- or z- statistic and the confidence interval. The p-values and confidence intervals are valid only when the sampling distribution of the resulting statistic is normal (which can be assessed using [sim_plot()]. When `normal = FALSE`, the confidence interval is calculated using the `alpha/2` and `1 - alpha/2` quantiles of the simulation estimates, and the p-value is calculated as twice the proportion of simulation estimates less than or greater than `null`, whichever is smaller. Using `normal = FALSE` (the default) is recommended because the confidence intervals and p-values will be valid even if the sampling distribution is not normally distributed. The precision of the p-values and confidence intervals depends on the number of simulation requested (the value of `n` supplied to [sim()]).
#'
# @examples
#'
#' @export
summary.simbased_est <- function(object, alpha = .05, normal = FALSE, null = NA, ...) {
  ans <- list()
  est <- coef(object)

  pct <- fmt.prc(c(alpha/2, 1-alpha/2))
  ci <- matrix(nrow = length(est), ncol = 2,
               dimnames = list(names(est), paste("CI", pct)))

  chk::chk_number(alpha)
  if (alpha <= 0 || alpha >= .5) {
    chk::err("`alpha` must be between 0 and .5")
  }
  chk::chk_flag(normal)

  if (length(null) == 0) null <- NA_real_
  if (length(null) == 1) null <- rep(null, length(est))

  test <- !all(is.na(null))
  if (!is.atomic(null) || test) {
    chk::chk_numeric(null)
    if (!chk::vld_length(null, length(est))) {
      chk::err(sprintf("`null` must have length 1 or length equal to the number of quantities estimated (%s)",
                       length(est)))
    }
  }

  nas <- anyNA(object)
  if (nas) chk::wrn("NA values present among the estimates")

  se <- z <- p <- NULL
  if (normal) {
    se <- apply(object, 2, sd, na.rm = TRUE)
    if (test) {
      z <- (est - null)/se
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)
      p[p < .Machine$double.eps] <- 0
    }
    zcrit <- qnorm(c(alpha/2, 1-alpha/2))
    ci[,1] <- est + se*zcrit[1]
    ci[,2] <- est + se*zcrit[2]
  }
  else {
    if (test) {
      ns <- if (nas) colSums(!is.na(object)) else rep(nrow(object), ncol(object))
      p <- vapply(seq_along(est), function(i) {
        if (is.na(null[i])) return(NA_real_)
        x <- object[,i]
        2 * min(sum(x < null[i], na.rm = nas),
                sum(x > null[i], na.rm = nas))/ns[i]
      }, numeric(1L))
    }
    ci[] <- t(apply(object, 2, quantile, probs = c(alpha/2, 1-alpha/2),
                    na.rm = nas))
  }

  coefficients <- cbind(Estimate = est,
                        ci,
                        `Std. Error` = if (test) se,
                        `Z value` = if (test) z,
                        `P-value` = if (test) p)

  ans <- list(coefficients = coefficients)
  class(ans) <- "summary.simbased_est"
  ans
}

#' @export
print.summary.simbased_est <- function(x, digits = 3, ...) {
  chk::chk_whole_number(digits)
  printCoefmat(x$coefficients, digits = digits,
               cs.ind = c(1:3, (4)["Std. Error" %in% colnames(x$coefficients)]),
               tst.ind = which(colnames(x$coefficients) == "Z value"),
               has.Pvalue = "P-value" %in% colnames(x$coefficients),
               na.print = ".")
}
