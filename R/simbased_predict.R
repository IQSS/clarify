clarify_predict <- function(x, newdata = NULL, group = NULL, type = NULL) {
  args <- list(x, newdata = newdata, vcov = FALSE, type = type)
  p <- try(do.call(marginaleffects::get_predict, args), silent = TRUE)

  if (length(p) == 0L || is_error(p)) {
    .err("predicted values could not be extracted from the model")
  }
  if ("group" %in% names(p) && !is.null(group)) {
    p <- subset_group(p, group)
  }
  p
}

subset_group <- function(pred, group = NULL) {
  if (is.null(group)) pred
  else pred[pred$group == group,]
}
