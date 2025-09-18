#' @exportS3Method plot clarify_est
#' @rdname summary.clarify_est
plot.clarify_est <- function(x,
                             parm,
                             ci = TRUE,
                             level = .95,
                             method = "quantile",
                             reference = FALSE,
                             ncol = 3L,
                             simultaneous = FALSE,
                             ...) {

  chk::chk_flag(ci)
  chk::chk_flag(reference)

  original_est <- coef(x)
  est_names <- names(x)

  parm <- process_parm(x, parm)
  if (anyNA(parm)) {
    .err("`parm` must be a numeric or character vector identifiying the estimates to plot")
  }

  est_names <- est_names[parm]

  est_long <- as.data.frame(as.matrix(x))[est_names] |>
    stack() |>
    setNames(c("val", "est"))

  original_est_long <- original_est[est_names] |>
    stack() |>
    setNames(c("val", "est"))

  p <- ggplot() +
    geom_density(data = est_long, mapping = aes(x = .data$val),
                 color = "black", fill = "gray90",
                 ...) +
    geom_hline(yintercept = 0) +
    geom_vline(data = original_est_long, mapping = aes(xintercept = .data$val)) +
    facet_wrap(vars(.data$est), scales = "free", ncol = min(ncol, nlevels(original_est_long$est)))

  if (ci) {
    ci_long <- confint(x, parm = parm, level = level,
                       method = method, simultaneous = simultaneous) |>
      t() |>
      as.data.frame() |>
      stack() |>
      setNames(c("val", "est"))

    p <- p + geom_vline(data = ci_long, mapping = aes(xintercept = .data$val),
                        linetype = 2)
  }

  if (reference) {
    #Add normal density and mean line
    ref_means_and_medians <- data.frame(
      est = factor(levels(est_long$est), levels = levels(est_long$est)),
      mean = tapply(est_long$val, est_long$est, mean),
      height = dnorm(0, 0, tapply(est_long$val, est_long$est, sd)),
      median = tapply(est_long$val, est_long$est, median))

    p <- p +
      geom_density(data = est_long, mapping = aes(x = .data$val),
                   stat = StatNormal, color = "red") +
      geom_segment(aes(x = .data$mean, xend = .data$mean,
                       y = 0, yend = .data$height),
                   data = ref_means_and_medians, color = "red") +
      geom_segment(aes(x = .data$median, xend = .data$median,
                       y = 0, yend = .2 * .data$height),
                   data = ref_means_and_medians, color = "blue")
  }

  p + labs(x = "Estimate", y = "Density") +
    theme_bw() +
    theme(panel.grid = element_blank())
}

#Stat for normal reference density
StatNormal <- ggplot2::ggproto("StatNormal", ggplot2::Stat,
                               required_aes = "x|y",
                               default_aes = ggplot2::aes(x = ggplot2::after_stat(density),
                                                          y = ggplot2::after_stat(density),
                                                          fill = NA, weight = NULL),
                               setup_params = function(data, params) {
                                 params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

                                 has_x <- !(is.null(data$x) && is.null(params$x))
                                 has_y <- !(is.null(data$y) && is.null(params$y))
                                 if (!has_x && !has_y) {
                                   rlang::abort("stat_normal() requires an x or y aesthetic.")
                                 }

                                 params
                               },
                               extra_params = c("na.rm", "orientation"),
                               compute_group = function(data, scales, n = 512, trim = FALSE,
                                                        na.rm = FALSE, flipped_aes = FALSE) {
                                 data <- ggplot2::flip_data(data, flipped_aes)
                                 if (trim) {
                                   range <- range(data$x, na.rm = TRUE)
                                 }
                                 else {
                                   range <- scales[[flipped_names(flipped_aes)$x]]$dimension()
                                 }

                                 density <- compute_norm_dens(data$x, w = data$weight, from = range[1],
                                                              to = range[2], n = n)
                                 density$flipped_aes <- flipped_aes
                                 ggplot2::flip_data(density, flipped_aes)
                               }
)

compute_norm_dens <- function(x, w, from, to, n = 512) {
  nx <- length(x)
  if (is.null(w)) {
    w <- rep.int(1, nx)
  }

  nax <- is.na(x)
  naw <- is.na(w)

  x <- x[!nax & !naw]
  w <- w[!nax & !naw]

  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    rlang::warn("Groups with fewer than two data points have been dropped.")
    return(data.frame(
      x = NA_real_,
      density = NA_real_,
      scaled = NA_real_,
      ndensity = NA_real_,
      count = NA_real_,
      n = NA_integer_
    ))
  }

  covw <- cov.wt(as.matrix(x), w)
  s <- sqrt(covw$cov)
  m <- covw$center

  x <- seq(from, to, length.out = n)
  y <- dnorm(x, m, s)

  data.frame(
    x = x,
    density = y,
    scaled =  y / max(y, na.rm = TRUE),
    ndensity = y / max(y, na.rm = TRUE),
    count = y * nx,
    n = nx
  )
}
