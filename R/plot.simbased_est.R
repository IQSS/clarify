#' Plot distribution of simulated estimates
#'
#' `plot()` plots the distribution of the simulated estimates in a density plot.
#'
#' @inheritParams summary.simbased_est
#' @param x a `simbased_est` object; the output of a call to `sim_apply()`.
#' @param ci `logical`; whether to display confidence interval limits for the estimates. Default is `TRUE`.
#' @param normal `logical`; whether to compute confidence intervals using a normal approximation (`TRUE`) or the simulated sampling distribution (`FALSE`; default). See [summary.simbased_est()] for details.
#'
#' @details These plots are produced using [ggplot2::geom_density()].
#'
#' @return A `ggplot` object.
# @examples
#' @export
plot.simbased_est <- function(x, est, ci = TRUE, alpha = .05, normal = FALSE, ...) {

  #' @import ggplot2

  if (!inherits(x, "simbased_est")) {
    stop("'x' must be a simbased_est object.")
  }

  original_est <- coef(x)
  est_names <- colnames(x)

  if (missing(est)) est <- seq_len(ncol(x))

  if (is.character(est)) {
    ind <- match(est, colnames(x))
    if (anyNA(ind)) {
      chk::err(sprintf("%s not the %s of any estimated quantities.",
                   word_list(est[is.na(ind)], is.are = TRUE, quotes = TRUE),
                   ngettext(sum(is.na(ind)), "name", "names")))
    }
    est <- ind
  }
  else if (is.numeric(est)) {
    chk::chk_whole_numeric(est)
    if (length(est) > 1 && ncol(x) == 1) {
      chk::wrn("ignoring `est` because only one estimate is available to plot")
    }
    if (any(est < 1) || any(est > ncol(x))) {
      chk::err(sprintf("all values in `est` must be between 1 and %s", ncol(x)))
    }
  }
  else {
    chk::err("`est` must be a numeric or character vector identifiying the estimates to plot")
  }

  est_names <- colnames(x)[est]

  est_long <- setNames(utils::stack(as.data.frame(x[,est_names, drop = FALSE])),
                       c("val", "est"))
  original_est_long <- setNames(utils::stack(original_est[est_names]),
                                c("val", "est"))

  p <- ggplot() +
    geom_density(data = est_long, mapping = aes(x = .data$val),
                 color = "black", fill = "gray90", trim = TRUE) +
    geom_hline(yintercept = 0) +
    geom_vline(data = original_est_long, mapping = aes(xintercept = .data$val))

  chk::chk_flag(ci)
  if (ci) {
    chk::chk_number(alpha)
    if (alpha <= 0 || alpha >= .5) {
      chk::err("`alpha` must be between 0 and .5")
    }
    chk::chk_flag(normal)

    if (normal) {
      zcrit <- qnorm(c(alpha/2, 1-alpha/2))
      ci <- setNames(as.data.frame(lapply(est_names, function(e) {
        original_est[e] + sd(x[,e])*zcrit
      })), est_names)
    }
    else {
      ci <- setNames(as.data.frame(lapply(est_names, function(e) {
        quantile(x[,e], probs = c(alpha/2, 1-alpha/2))
      })), est_names)
    }

    ci_long <- setNames(utils::stack(ci), c("val", "est"))
    p <- p + geom_vline(data = ci_long, mapping = aes(xintercept = .data$val),
                        linetype = 2)
  }

  p <- p + facet_wrap(vars(.data$est), scales = "free") +
    labs(x = "Estimate", y = "Density") +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.border = element_rect(color = "black", fill = NA))

  p
}
