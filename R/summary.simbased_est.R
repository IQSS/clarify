#' Title
#'
#' @param object a `simbased_est` object; the output of a call to `sim_apply()`.
#' @param alpha the $\alpha$ used to compute the confidence interval. Default is .05 for 95% confidence intervals.
#' @param normal `logical`; whether to compute p-values and confidence intervals using a normal approximation (`TRUE`) or the simulated sampling distribution (`FALSE`; default).
#' @param null the value of the parameter under the null hypothesis for the p-value calculations.
#' @param ... ignored.
#'
#' @return
#' @export
#'
#' @examples
summary.simbased_est <- function(object, alpha = .05, normal = FALSE, test = FALSE, null = 0, transform = NULL, ...) {
  ans <- list()
  est <- coef(object)

  pct <- fmt.prc(c(alpha/2, 1-alpha/2))
  ci <- matrix(nrow = length(est), ncol = 2,
               dimnames = list(names(est), paste("CI", pct)))

  chk::chk_numeric(null)
  if (length(null) == 1) null <- rep(null, length(est))
  chk::chk_length(null, length(est))

  if (!is.null(transform)) {
    if (is.character(transform)) transform_name <- transform
    else transform_name <- "fn"
    transform <- try(match.fun(transform))
    chk::chk_function(transform)
    est <- transform(est)
    object$est <- transform(object$est)
  }

  se <- z <- p <- NULL
  if (normal) {
    se <- sqrt(diag(vcov(object)))
    if (test) {
      z <- (est - null)/se
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    }
    zcrit <- qnorm(c(alpha/2, 1-alpha/2))
    ci[,1] <- est - null + se*zcrit[1]
    ci[,2] <- est - null + se*zcrit[2]
  }
  else {
    if (test) {
      p <- vapply(seq_along(est), function(i) {
        x <- object$est[,i]
        if (est[i] < null) {
          return(mean(x > null | x < 2*est[i] - null))
        }
        else {
          return(mean(x < null | x > 2*est[i] - null))
        }
      }, numeric(1L))
    }
    ci[] <- t(apply(object$est, 2, quantile, probs = c(alpha/2, 1-alpha/2)))

  }

  coefficients <- cbind(Estimate = est, `Std. Error` = if (test) se,
                       `Z value` = if (test) z, `P-value` = if (test) p,
                       ci)
  if (!is.null(transform)) {
    rownames(coefficients) <- paste0(transform_name, "(", rownames(coefficients), ")")
  }

  ans <- list(coefficients = coefficients)
  class(ans) <- "summary.simbased_est"
  ans
}

#' @export
print.summary.simbased_est <- function(x, digits = 3, ...) {
  printCoefmat(x$coefficients, digits = digits)
}
