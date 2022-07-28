#' Title
#'
#' @param x a `simbased_est1 object`; the output of a call to `sim_apply()`.
#' @param est a vector of the names or indices of the estimates to plot. If unspecified, all estimates will be plotted.
#' @param ci
#' @param alpha
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
sim_plot <- function(x, est, ci = TRUE, alpha = .05, normal = FALSE, transform = NULL) {

  if (!inherits(x, "simbased_est")) {
    stop("'x' must be a simbased_est object.")
  }

  if (!is.null(transform)) {
    chk::chk_function(transform)
    x$est <- transform(x$est)
  }

  est_names <- colnames(x$est)
  if (missing(est)) est <- seq_len(ncol(x$est))

  if (is.character(est)) {
    ind <- match(est, est_names)
    if (anyNA(ind)) {
      stop(sprintf("%s not the %s of any estimated quantities.",
                   word_list(est[is.na(ind)], is.are = TRUE, quotes = TRUE),
                   ngettext(sum(is.na(ind)), "name", "names")))
    }
    est <- ind[!is.na(ind)]
  }

  est_long <- do.call("rbind", lapply(est, function(e) {
    data.frame(est = est_names[e], val = x$est[,e])
  }))

  est_long[[1]] <- factor(est_long[[1]], levels = est_names[est])

  p <- ggplot() +
    geom_density(data = est_long, mapping = aes(x = val),
                 color = "black", fill = "white", trim = TRUE) +
    geom_hline(yintercept = 0)

  if (ci) {
    if (normal) {
      zcrit <- qnorm(c(alpha/2, 1-alpha/2))
      ci <- do.call("rbind", lapply(est, function(e) {
        data.frame(est = est_names[e], val = mean(x$est[,e]) + sd(x$est[,e])*zcrit)
      }))
    }
    else {
      ci <- do.call("rbind", lapply(est, function(e) {
        data.frame(est = est_names[e], val = quantile(x$est[,e], probs = c(alpha/2, 1-alpha/2)))
      }))
    }

    p <- p + geom_vline(data = ci, mapping = aes(xintercept = val), color = "red",
                        linetype = 1)
  }

  p <- p + facet_wrap(vars(est), scales = "free") +
    labs(x = "Estimate")

  p
}
