#' Apply a Function To Simulated Parameter Values
#'
#' @param sim
#' @param FUN
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sim_apply <- function(sim, FUN, ...) {
  if (missing(FUN)) {
    stop("FUN must be supplied.")
  }

  test <- try(FUN(sim$fit, ...))
  if (inherits(test, "try-error")) {
    stop("Initial check failed.")
  }

  if (is.null(names(test))) names(test) <- paste0("est", seq_along(test))

  ests <- matrix(NA_real_, nrow = nrow(sim$coefs), ncol = length(test),
                 dimnames = list(NULL, names(test)))

  pb <- txtProgressBar(0, nrow(ests), style = 3)
  for (i in seq_len(nrow(ests))) {
    sim$fit <- coef_assign(sim$fit, sim$coefs[i,])
    ests[i,] <- FUN(sim$fit, ...)
    setTxtProgressBar(pb, i)
  }
  close(pb)

  out <- list(est = ests)
  class(out) <- "simbased_est"

  out
}

summary.simbased_est <- function(object, alpha = .05, ...) {
  ans <- list()
  est <- coef(object)
  se <- sqrt(diag(vcov(object)))
  z <- est/se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)

  fmt.prc <- function(probs, digits = 3) {
    paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")
  }

  pct <- fmt.prc(c(alpha/2, 1-alpha/2))
  zcrit <- qnorm(c(alpha/2, 1-alpha/2))

  ci <- matrix(nrow = length(est), ncol = 2,
               dimnames = list(names(est), pct))
  ci[,1] <- est + se*zcrit[1]
  ci[,2] <- est + se*zcrit[2]

  ans <- list(coefficients = cbind(Estimate = est, `Std. Error` = se,
                                   `z value` = z, `Pr(>|z|)` = p,
                                   ci))
  class(ans) <- "summary.simbased_est"
  ans
}

coef.simbased_est <- function(object, ...) {
  colMeans(object$est)
}
vcov.simbased_est <- function(object, ...) {
  cov(object$est)
}
