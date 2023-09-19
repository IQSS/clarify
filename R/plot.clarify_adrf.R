#' Plot marginal predictions from `sim_adrf()`
#'
#' `plot.clarify_adrf()` plots the output of [sim_adrf()]. For the average dose-response function (ADRF, requested with `contrast = "adrf"` in `sim_adrf()`), this is a plot of the average marginal mean of the outcome against the requested values of the focal predictor; for the average marginal effects function (AMEF, requested with `contrast = "amef"` in `sim_adrf()`), this is a plot of the instantaneous average marginal effect of the focal predictor on the outcome against the requested values of the focal predictor.
#'
#' @inheritParams plot.clarify_est
#' @param x a `clarify_adrf` object resulting from a call to [sim_adrf()].
#' @param ci `logical`; whether to display confidence bands for the estimates. Default is `TRUE`.
#' @param method the method used to compute confidence bands. Can be `"wald"` to use a Normal approximation or `"quantile"` to use the simulated sampling distribution (default). See [summary.clarify_est()] for details. Abbreviations allowed.
#' @param baseline `logical`; whether to include a horizontal line at `y = 0` on the plot. Default is `FALSE` for the ADRF (since 0 might not be in the range of the outcome) and `TRUE` for the AMEF.
#' @param color the color of the line and confidence band in the plot.
#'
#' @return A `ggplot` object.
#'
#' @details These plots are produced using [ggplot2::geom_line()] and [ggplot2::geom_ribbon()]. The confidence bands should be interpreted pointwise (i.e., they do not account for simultaneous inference).
#'
#' @seealso [summary.clarify_est()] for computing p-values and confidence intervals for the estimated quantities.
#'
#' @examples
#' ## See help("sim_adrf") for examples
#'
#' @exportS3Method plot clarify_adrf
plot.clarify_adrf <- function(x,
                              ci = TRUE,
                              level = .95,
                              method = "quantile",
                              baseline,
                              color = "black",
                              ...) {

  at <- attr(x, "at")
  var <- attr(x, "var")
  contrast <- attr(x, "contrast")
  by <- attr(x, "by")

  if (missing(baseline)) {
    baseline <- !is.null(contrast) && contrast == "amef"
  }
  else {
    chk::chk_flag(baseline)
  }

  s <- {
    if (ci)
      as.data.frame(summary.clarify_est(x, level = level, method = method))
    else
      data.frame(Estimate = coef(x))
  }

  if (!is.null(by)) {
    s$by_var <- factor(.extract_by_values(x))
    if (nlevels(s$by_var) == 1)
      by <- NULL
  }

  p <- ggplot(mapping = aes(x = at))

  if (baseline) {
    p <- p + geom_hline(yintercept = 0)
  }

  if (is.null(by)) {
    p <- p + geom_line(aes(y = s$Estimate),
                       color = color) +
      labs(x = var, y = "E[Y|X]")
  }
  else {
    p <- p + geom_line(aes(y = s$Estimate, color = s$by_var)) +
      labs(x = var, y = "E[Y|X]", color = paste(by, collapse = ", "))
  }

  if (ci) {
    if (is.null(by)) {
      p <- p +
        geom_ribbon(aes(ymin = s[[2]], ymax = s[[3]]),
                    alpha = .3, fill = color)
    }
    else {
      p <- p +
        geom_ribbon(aes(ymin = s[[2]], ymax = s[[3]],
                        fill = s$by_var),
                    alpha = .3) +
        labs(fill = paste(by, collapse = ", "))
    }
  }
  p + labs(x = var, y = if (!is.null(attr(x, "contrast"))) switch(attr(x, "contrast"), "adrf" = sprintf("E[Y(%s)]", var),
                                                                  "amef" = sprintf("E[dY/d(%s)]", var))) +
    theme_bw()
}

.extract_by_values <- function(obj) {
  x <- names(obj)

  if (identical(attr(obj, "contrast"), "amef"))
    pattern <- "\\,([^]]+)\\]"
  else
    pattern <- "\\|([^]]+)\\]"

  matches <- regexpr(pattern, x, perl = TRUE)
  out <- regmatches(x, matches)

  substr(out, 2, nchar(out) - 1)
}
