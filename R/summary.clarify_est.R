#' Plotting and inference for `clarify_est` objects
#'
#' `summary()` tabulates the estimates and confidence intervals and (optionally) p-values from a `clarify_est` object. `confint()` computes confidence intervals. `plot()` plots the "posterior" distribution of estimates.
#'
#' @param object,x a `clarify_est` object; the output of a call to [sim_apply()] or its wrappers.
#' @param parm a vector of the names or indices of the estimates to plot. If unspecified, all estimates will be displayed.
#' @param level the confidence level desired. Default is .95 for 95% confidence intervals.
#' @param method the method used to compute p-values and confidence intervals. Can be `"wald"` to use a Normal approximation or `"quantile"` to use the simulated sampling distribution (default). See Details. Abbreviations allowed.
#' @param null the values of the parameters under the null hypothesis for the p-value calculations. Should have length equal to the number of quantities estimated, or one, in which case it will be recycled, or it can be a named vector with just the names of quantities for which null values are to be set. Set values to `NA` to omit p-values for those quantities. When all values are `NA`, the default, no p-values are produced.
#' @param simultaneous `logical`; whether confidence intervals and p-values should be simultaneous or not (i.e., adjusted for multiple comparisons); default is `FALSE`. See Details.
#' @param ci `logical`; whether to display confidence interval limits for the estimates. Default is `TRUE`.
#' @param reference `logical`; whether to overlay a normal density reference distribution over the plots. Default is `FALSE`.
#' @param ncol the number of columns used when wrapping multiple plots; default is 3.
#' @param ... for `plot()`, further arguments passed to [ggplot2::geom_density()].
#'
#' @returns
#' For `summary()`, a `summary.clarify_est` object, which is a matrix containing the coefficient estimates, standard errors, test statistics, p-values, and confidence intervals. Not all columns will be present depending on the arguments supplied to `summary()`.
#'
#' For `confint()`, a matrix containing the confidence intervals for the requested quantities.
#'
#' For `plot()`, a `ggplot` object.
#'
#' @details
#' `summary()` uses the estimates computed from the original model as its estimates and uses the simulated parameters for inference only, in line with the recommendations of Rainey (2023).
#'
#' When `method = "wald"`, the standard deviation of the simulation estimates is used as the standard error, which is used in the z-statistics and the confidence intervals. The p-values and confidence intervals are valid only when the sampling distribution of the resulting statistic is normal (which can be assessed using `plot()`). When `method = "quantile"`, the confidence interval is calculated using the quantiles of the simulation estimates corresponding to `level`, and the p-value is calculated as twice the proportion of simulation estimates less than or greater than `null`, whichever is smaller; this is equivalent to inverting the confidence interval but is only truly valid when the true sampling distribution is only a location shift from the sampling distribution under the null hypothesis and should therefore be interpreted with caution. Using `"method = "quantile"` (the default) is recommended because the confidence intervals will be valid even if the sampling distribution is not Normally distributed. The precision of the p-values and confidence intervals depends on the number of simulations requested (the value of `n` supplied to [sim()]).
#'
#' When `simultaneous = TRUE`, confidence intervals and p-values are adjusted to account for multiple comparisons using the "sup-t" confidence region and its inversion. The sup-t confidence region is the smallest rectangular region that ensures simultaneous coverage of all parameters at the desired confidence level. It is found by adjusting the nominal confidence level until a new level is found that, when simultaneously applied to all estimates, yields intervals that contain all estimates with rates equal to the original level. Unlike some other adjustments for multiple comparisons (e.g., Holm, Bonferroni, Benjamini-Hochberg), this method takes into account the joint distribution of the estimates, often yielding narrower regions that are less conservative (but still valid). P-values are found by finding the level of the narrowest band that guarantees simultaneous coverage while containing the null value of the given parameter. When `method = "quantile"`, the Bayesian algorithm described by Montiel Olea and Plagborg-Møller (2019) is used with the simulated estimates; when `method = "wald"`, a new simulation is performed treating the estimates as coming from a multivariate normal distribution.
#'
#' The plots are produced using [ggplot2::geom_density()] and can be customized with \pkg{ggplot2} functions. When `reference = TRUE`, a reference Normal distribution is produced using the empirical mean and standard deviation of the simulated values. A blue references line is plotted at the median of the simulated values. For Wald-based inference to be valid, the reference distribution should overlap with the empirical distribution, in which case the quantile-based and Wald-based intervals should be similar. For quantile-based inference to be valid, the median of the estimates should overlap with the estimated value; this is a necessary but not sufficient condition, though.
#'
#' @seealso
#' * [sim_apply()] for applying a function to each set of simulated coefficients
#'
#' @references
#' Montiel Olea, J. L., & Plagborg-Møller, M. (2019). Simultaneous confidence bands: Theory, implementation, and an application to SVARs. *Journal of Applied Econometrics*, 34(1), 1–17. \doi{10.1002/jae.2656}
#'
#' Rainey, C. (2023). A careful consideration of CLARIFY: Simulation-induced bias in point estimates of quantities of interest. *Political Science Research and Methods*, 1–10. \doi{10.1017/psrm.2023.8}
#'
#' @examplesIf rlang::is_installed("MatchIt")
#' data("lalonde", package = "MatchIt")
#' fit <- glm(I(re78 > 0) ~ treat + age + race + nodegree + re74,
#'           data = lalonde)
#'
#' s <- sim(fit, n = 100)
#'
#' # Compute average marginal means for `treat`
#' est <- sim_ame(s, var = "treat", verbose = FALSE)
#' coef(est)
#'
#' # Compute average marginal effects on risk difference
#' # (RD) and risk ratio (RR) scale
#' est <- transform(est,
#'                  RD = `E[Y(1)]` - `E[Y(0)]`,
#'                  RR = `E[Y(1)]` / `E[Y(0)]`)
#'
#' # Compute confidence intervals and p-values,
#' # using given null values for computing p-values
#' summary(est, null = c(`RD` = 0, `RR` = 1))
#'
#' # Same tests using normal approximation and alternate
#' # syntax for `null`
#' summary(est, null = c(NA, NA, 0, 1),
#'         normal = TRUE)
#'
#' # Plot the RD and RR with a reference distribution
#' plot(est, parm = c("RD", "RR"), reference = TRUE,
#'      ci = FALSE)
#'
#' # Plot the RD and RR with quantile confidence bounds
#' plot(est, parm = c("RD", "RR"), ci = TRUE)

#' @exportS3Method summary clarify_est
summary.clarify_est <- function(object,
                                parm,
                                level = .95,
                                method = "quantile",
                                null = NA,
                                simultaneous = FALSE,
                                ...) {

  if (is_null(coef(object)) ||
      is_null(ncol(object)) ||
      ncol(object) != length(coef(object)) ||
      !identical(names(object), names(coef(object)))) {
    .err("the `clarify_est` object is malformed, possibly due to tampering")
  }

  original_est <- coef(object)

  parm <- process_parm(object, parm)
  if (anyNA(parm)) {
    .err("`parm` must be a numeric or character vector identifying the estimates to summarize")
  }

  chk::chk_string(method)
  method <- match_arg(method, c("quantile", "wald"))
  # method <- match_arg(method, c("quantile", "wald", "optimal"))

  null <- process_null(null, object, parm)

  test <- !all(is.na(null))

  nas <- anyNA(object[parm])
  if (nas) {
    .wrn("`NA` values present among the estimates")
  }

  ans <- cbind(Estimate = original_est[parm],
               confint(object, parm = parm, level = level,
                       method = method, simultaneous = simultaneous))

  if (test) {
    se <- z <- p <- NULL
    object <- drop_sim_class(object)

    if (method == "quantile") {
      ns <- {
        if (nas) colSums(!is.na(object[, parm, drop = FALSE]))
        else rep.int(nrow(object), length(parm))
      }

      p <- vapply(seq_along(parm), function(i) {
        if (is.na(null[i])) {
          return(NA_real_)
        }

        x <- object[, parm[i]]
        2 * min(sum(x <= null[i], na.rm = nas),
                sum(x >= null[i], na.rm = nas)) / ns[i]
      }, numeric(1L))

      if (simultaneous && length(parm) > 1L) {
        p[] <- vapply(p, .pointwise_p_to_simul_p, numeric(1L),
                      object = object[, parm, drop = FALSE])
      }
    }
    else if (method == "wald") {
      se <- apply(object[, parm, drop = FALSE], 2L, sd, na.rm = TRUE)
      z <- (original_est[parm] - null) / se
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)

      if (simultaneous && length(parm) > 1L) {
        object2 <- rmvt(1e5,
                        mu = original_est[parm],
                        Sigma = vcov(object)[parm, parm, drop = FALSE])

        p[] <- vapply(p, .pointwise_p_to_simul_p, numeric(1L),
                      object = object2)
      }
    }
    else {
      .err('`null` cannot be specified when `method = "optimal"`')
    }

    p[p < .Machine$double.eps] <- 0

    ans <- cbind(ans,
                 `Std. Error` = se,
                 `Z value` = z,
                 `P-value` = p)
  }

  class(ans) <- c("summary.clarify_est", class(ans))
  ans
}

#' @exportS3Method print summary.clarify_est
print.summary.clarify_est <- function(x, digits = 3L, ...) {
  chk::chk_whole_number(digits)
  stats::printCoefmat(x, digits = digits,
                      cs.ind = c(1:3, (4)["Std. Error" %in% colnames(x)]),
                      tst.ind = which(colnames(x) == "Z value"),
                      has.Pvalue = "P-value" %in% colnames(x),
                      na.print = ".")
  invisible(x)
}

#' @exportS3Method confint clarify_est
#' @rdname summary.clarify_est
confint.clarify_est <- function(object,
                                parm,
                                level = .95,
                                method = "quantile",
                                simultaneous = FALSE,
                                ...) {

  chk::chk_number(level)
  chk::chk_range(level, c(.5, 1), inclusive = FALSE)

  chk::chk_string(method)
  method <- match_arg(method, c("quantile", "wald"))
  # method <- match_arg(method, c("quantile", "wald", "optimal"))

  parm <- process_parm(object, parm)
  if (anyNA(parm)) {
    .err("`parm` must be a numeric or character vector identifying the estimates for which to compute confidence intervals")
  }

  chk::chk_flag(simultaneous)
  if (simultaneous && method == "optimal") {
    .err('`method = "optimal"` cannot be used with `simultaneous = TRUE`')
  }

  est_names <- names(object)[parm]

  a <- (1 - level) / 2
  a <- c(a, 1 - a)

  pct <- fmt.prc(a, 3)

  ci <- array(NA_real_, dim = c(length(parm), 2L),
              dimnames = list(est_names, pct))

  nas <- anyNA(object[parm])
  if (nas) {
    .wrn("`NA` values present among the estimates")
  }

  if (method == "quantile") {
    object <- drop_sim_class(object)
    if (simultaneous && length(parm) > 1L) {
      alpha <- .pointwise_p_to_simul_p(2 * a[1L], object[, parm, drop = FALSE])

      a <- c(alpha / 2, 1 - alpha / 2)
    }

    ci[] <- t(apply(object[, parm, drop = FALSE], 2L, quantile, probs = a,
                    na.rm = nas, type = 8, names = FALSE))
  }
  else if (method == "wald") {
    cf <- coef(object)
    ses <- sqrt(diag(vcov(object)))[parm]

    if (simultaneous && length(parm) > 1L) {
      object2 <- rmvt(1e5,
                      mu = cf[parm],
                      Sigma = vcov(object)[parm, parm, drop = FALSE])

      alpha <- .pointwise_p_to_simul_p(2 * a[1L], object2)

      a <- c(alpha / 2, 1 - alpha / 2)
    }

    fac <- qnorm(a)
    ci[] <- cf[parm] + outer(ses, fac, "*")
  }
  else if (method == "optimal") {
    object <- drop_sim_class(object)
    qseq <- seq(0, 1 - level, length.out = nrow(object))[-c(1L, nrow(object))]
    rowDiff <- function(x) {x[,2L] - x[,1L]}
    ci[] <- t(apply(object[, parm, drop = FALSE], 2L, function(x) {
      ci.lengths <- rowDiff(matrix(quantile(x, probs = c(qseq, qseq + level),
                                            na.rm = nas, type = 8, names = FALSE),
                                   ncol = 2L))

      best.q <- qseq[which.min(ci.lengths)]
      unname(quantile(x, probs = c(best.q, best.q + level),
                      na.rm = nas, type = 8, names = FALSE))
    }))

    # ci[] <- t(apply(object[, parm, drop = FALSE], 2, function(x) {
    #   opt <- optimize(function(q) diff(quantile(x, probs = c(q, q + level),
    #                                             na.rm = nas, type = 8, names = FALSE)),
    #                   lower = 0, upper = 1 - level)
    #   best.q <- opt$minimum
    #   unname(quantile(x, probs = c(best.q, best.q + level),
    #                   na.rm = nas, type = 8, names = FALSE))
    # }))
  }

  ci
}

#' @exportS3Method coef clarify_est
coef.clarify_est <- function(object, ...) {
  .attr(object, "original")
}

#' @exportS3Method vcov clarify_est
vcov.clarify_est <- function(object, ...) {
  cov(drop_sim_class(object))
}

.pointwise_p_to_simul_p <- function(p, object) {
  level <- 1 - p

  nas <- anyNA(object)
  k <- ncol(object)

  fun <- function(q) {
    interval <- apply(object, 2L, quantile, probs = c(q, 1 - q),
                      na.rm = nas, type = 8, names = FALSE)

    all_est_above <- rowSums(sweep(object, 2L, interval[1L,]) >= 0) == k
    all_est_below <- rowSums(sweep(object, 2L, interval[2L,]) <= 0) == k

    coverage <- mean(all_est_above & all_est_below)

    coverage - level
  }

  a <- uniroot(fun, interval = c(p / (2.5 * k), max(p / 2, level / 2)),
               tol = 1e-10)$root

  2 * a
}
