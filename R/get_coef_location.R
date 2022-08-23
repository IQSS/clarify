# Reverse-engineering `insight::get_parameters()` to find location of
# coefficients so they can be inserted into model fit. Returns a vector
# that when supplied to `[[` gets the location of the coefficients.
get_coef_location <- function(fit, coefs) {
  loc <- NULL
  if (inherits(fit, "glm") || inherits(fit, "lm")) {
    loc <- "coefficients"
  }
  #Add more for known models

  if (is.null(loc)) {
    loc <- list.search(fit, coefs)
  }

  loc
}
