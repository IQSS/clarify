coef_assign <- function(fit, coef) {
  UseMethod("coef_assign")
}
coef_assign.default <- function(fit, coefs){
  fit$coefficients <- coefs
  return(fit)
}
