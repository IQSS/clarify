#' Simulate Model Coefficients
#'
#' @param fit a model fit, such as the output of a call to `lm()` or `glm()`
#' @param n the number of simulations to run; default is 1000. More is better but resulting calculations will take longer.
#' @param vcov optional; a function to use to extract the asymptotic covariance matrix of the parameter estimates. By default, uses `stats::vcov()`.
#'
#' @return A `simbased_sim` object, which has the following components:
#' @export
#'
#' @examples
sim <- function(fit, n = 1e3, vcov = stats::vcov) {
  V <- vcov(fit)
  B <- coef(fit)

  sampler <- get_sampling_dist(fit)

  call <- insight::get_call(fit)

  out <- list(call = call,
              coefs = sampler(n, B, V),
              fit = fit)

  class(out) <- "simbased_sim"

  out
}
