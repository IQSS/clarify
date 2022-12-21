#' @exportS3Method plot clarify_est
#' @rdname summary.clarify_est
plot.clarify_est <- function(x, parm, ci = TRUE, level = .95, method = "quantile", ...) {

  original_est <- coef(x)
  est_names <- names(x)

  parm <- process_parm(x, parm)
  if (anyNA(parm)) {
    .err("`parm` must be a numeric or character vector identifiying the estimates to plot")
  }

  est_names <- est_names[parm]

  est_long <- setNames(utils::stack(as.data.frame(as.matrix(x))[est_names]),
                       c("val", "est"))
  original_est_long <- setNames(utils::stack(original_est[est_names]),
                                c("val", "est"))

  p <- ggplot() +
    geom_density(data = est_long, mapping = aes(x = .data$val),
                 color = "black", fill = "gray90",
                 ...) +
    geom_hline(yintercept = 0) +
    geom_vline(data = original_est_long, mapping = aes(xintercept = .data$val))

  chk::chk_flag(ci)
  if (ci) {
    ci <- confint(x, parm = parm, level = level,
                  method = method)

    ci_long <- setNames(utils::stack(as.data.frame(t(ci))), c("val", "est"))
    p <- p + geom_vline(data = ci_long, mapping = aes(xintercept = .data$val),
                        linetype = 2)
  }

  p <- p + facet_wrap(vars(.data$est), scales = "free") +
    labs(x = "Estimate", y = "Density") +
    theme_bw() +
    theme(panel.grid = element_blank())

  p
}
