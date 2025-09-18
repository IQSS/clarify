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
#' @param simultaneous `logical`; whether confidence bands should be simultaneous or not (i.e., for nominal coverage of the whole effect curve); default is `FALSE`, but `TRUE` is recommended. See Details at [summary.clarify_est()] for details.
#'
#' @returns
#' A `ggplot` object.
#'
#' @details
#' These plots are produced using [ggplot2::geom_line()] and [ggplot2::geom_ribbon()]. The confidence bands should be interpreted pointwise (i.e., they do not account for simultaneous inference) unless `simultaneous = TRUE`.
#'
#' @seealso
#' [summary.clarify_est()] for computing p-values and confidence intervals for the estimated quantities.
#'
#' @examples
#' ## See help("sim_adrf") for examples

#' @exportS3Method plot clarify_adrf
plot.clarify_adrf <- function(x,
                              ci = TRUE,
                              level = .95,
                              method = "quantile",
                              baseline = NULL,
                              color = "black",
                              simultaneous = FALSE,
                              ...) {

  at <- .attr(x, "at")
  var <- .attr(x, "var")
  contrast <- .attr(x, "contrast")
  by <- .attr(x, "by")

  if (is_null(baseline)) {
    baseline <- identical(contrast, "amef")
  }
  else {
    chk::chk_flag(baseline)
  }

  if (ci) {
    s <- summary.clarify_est(x, level = level, method = method,
                             simultaneous = simultaneous) |>
      as.data.frame()
  }
  else {
    s <- data.frame(Estimate = coef(x))
  }

  if (is_not_null(by)) {
    s$by_var <- factor(.extract_by_values(x))
    if (nlevels(s$by_var) == 1L) {
      by <- NULL
    }
  }

  p <- ggplot(mapping = aes(x = at))

  if (baseline) {
    p <- p + geom_hline(yintercept = 0)
  }

  if (is_null(by)) {
    p <- p + geom_line(aes(y = s$Estimate),
                       color = color) +
      labs(x = var, y = "E[Y|X]")
  }
  else {
    p <- p + geom_line(aes(y = s$Estimate, color = s$by_var)) +
      labs(x = var, y = "E[Y|X]", color = toString(by))
  }

  if (ci) {
    if (is_null(by)) {
      p <- p +
        geom_ribbon(aes(ymin = s[[2L]], ymax = s[[3L]]),
                    alpha = .3, fill = color)
    }
    else {
      p <- p +
        geom_ribbon(aes(ymin = s[[2L]], ymax = s[[3L]],
                        fill = s$by_var),
                    alpha = .3) +
        labs(fill = toString(by))
    }
  }

  p + labs(x = var,
           y = if (is_not_null(.attr(x, "contrast"))) {
             switch(.attr(x, "contrast"),
                    adrf = sprintf("E[Y(%s)]", var),
                    amef = sprintf("E[dY/d(%s)]", var))
           }) +
    theme_bw()
}

.extract_by_values <- function(obj) {
  x <- names(obj)

  pattern <- {
    if (identical(.attr(obj, "contrast"), "amef")) "\\,([^]]+)\\]"
    else "\\|([^]]+)\\]"
  }

  matches <- regexpr(pattern, x, perl = TRUE)
  out <- regmatches(x, matches)

  substr(out, 2L, nchar(out) - 1L)
}
