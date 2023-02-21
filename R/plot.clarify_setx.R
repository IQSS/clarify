#' Plot marginal predictions from `sim_setx()`
#'
#' `plot.clarify_sext()` plots the output of [sim_setx()], providing graphics similar to those of [plot.clarify_est()] but with features specifically for plot marginal predictions. For continues predictors, this is a plot of the marginal predictions and their confidence bands across levels of the predictor. Otherwise, this is is a plot of simulated sampling distribution of the marginal predictions.
#'
#' @inheritParams plot.clarify_est
#' @param x a `clarify_est` object resulting from a call to [sim_setx()].
#' @param var the name of the focal varying predictor, i.e., the variable to be on the x-axis of the plot. All other variables with varying set values will be used to color the resulting plot. See Details. Ignored if no predictors vary or if only one predictor varies in the reference grid or if `x1` was specified in `sim_setx()`. If not set, will use the predictor with the greatest number of unique values specified in the reference grid.
#' @param ci `logical`; whether to display confidence intervals or bands for the estimates. Default is `TRUE`.
#' @param method the method used to compute confidence intervals or bands. Can be `"wald"` to use a Normal approximation or `"quantile"` to use the simulated sampling distribution (default). See [summary.clarify_est()] for details. Abbreviations allowed.
#' @param reference `logical`; whether to overlay a normal density reference distribution over the plots. Default is `FALSE`. Ignored when variables other than the focal varying predictor vary.
#'
#' @return A `ggplot` object.
#'
#' @details `plot()` creates one of two kinds of plots depending on how the reference grid was specified in the call to `sim_setx()` and what `var` is set to. When the focal varying predictor (i.e., the one set in `var`) is numeric and takes on three or more unique values in the reference grid, the produced plot is a line graph displaying the value of the marginal prediction (denoted as `E[Y|X]`) across values of the focal varying predictor, with confidence bands displayed when `ci = TRUE`. If other predictors also vary, lines for different values will be displayed in different colors. These plots are produced using [ggplot2::geom_line()] and [ggplot2::geom_ribbon()]
#'
#' When the focal varying predictor is a factor or character or only takes on two or fewer values in the reference grid, the produced plot is a density plot of the simulated predictions, similar to the plot resulting from [plot.clarify_est()]. When other variables vary, densities for different values will be displayed in different colors. These plots are produced using [ggplot2::geom_density()].
#'
#' Marginal predictions are identified by the corresponding levels of the predictors that vary. The user should keep track of whether the non-varying predictors are set at specified or automatically set "typical" levels.
#'
#' @seealso [summary.clarify_est()] for computing p-values and confidence intervals for the estimated quantities.
#'
#' @examples
#' ## See help("sim_setx") for examples
#'
#' @export
plot.clarify_setx <- function(x,
                              var = NULL,
                              ci = TRUE,
                              level = .95,
                              method = "quantile",
                              reference = FALSE,
                              ...) {

  newdata <- attr(x, "setx")

  if (nrow(newdata) == 1) {
    if (!is.null(var)) {
      chk::wrn("ignoring `var` because no variables vary over predictions")
    }
    return(plot.clarify_est(x, parm = 1, ci = ci, level = level,
                            method = method, reference = reference, ...))
  }
  else if (isTRUE(attr(x, "fd"))) {
    if (!is.null(var)) {
      chk::wrn("ignoring `var`")
    }
    return(plot.clarify_est(x, parm = 1:3, ci = ci, level = level,
                            method = method, reference = reference, ...))
  }

  len_unique_newdata <- vapply(newdata, function(v) length(unique(v)), integer(1L))
  varying <- names(newdata)[len_unique_newdata > 1]

  if (length(varying) == 1) {
    if (!is.null(var) && !identical(var, varying)) {
      chk::wrn("ignoring `var` because only one variable varies over predictions")
    }
    var <- varying
  }
  else if (is.null(var)) {
    if (any(len_unique_newdata[varying] > 2)) {
      var <- attr(newdata, "set_preds")[which.max(len_unique_newdata[attr(newdata, "set_preds")])]
    }
    else {
      var <- attr(newdata, "set_preds")[attr(newdata, "set_preds") %in% varying][1]
    }
  }
  else {
    chk::chk_string(var)
    if (!var %in% varying) {
      .err("`var` must be the name of a predictor set to be varying. Allowable options include ", word_list(varying, quotes = TRUE))
    }
  }

  non_var_varying <- setdiff(varying, var)

  if (len_unique_newdata[var] == 2 || chk::vld_character_or_factor(newdata[[var]])) {
    p <- setx_sim_plot(x, var, non_var_varying, ci = ci,
                       level = level, method = method, ...)
  }
  else {
    p <- setx_reg_plot(x, var, non_var_varying, ci = ci,
                       level = level, method = method)
  }

  p + theme_bw() + scale_fill_brewer(palette = "Set1")
}

#sim_plot, but with grouping by non_var_varying if present
setx_sim_plot <- function(x, var, non_var_varying = NULL, ci = TRUE, level = .95,
                          method = "quantile", ...) {

  chk::chk_flag(ci)

  newdata <- attr(x, "setx")
  original_est <- coef(x)
  est_names <- rownames(newdata)

  est_long <- setNames(utils::stack(as.data.frame(x)[est_names]),
                       c("val", "est"))
  est_long <- merge(est_long,
                    newdata[c(var, non_var_varying)],
                    by.x = "est", by.y = 0)
  est_long[[var]] <- paste0(var, " = ", add_quotes(est_long[[var]], chk::vld_character_or_factor(est_long[[var]])))

  original_est_long <- setNames(utils::stack(original_est[est_names]),
                                c("val", "est"))
  original_est_long <- merge(original_est_long,
                             newdata[c(var, non_var_varying)],
                             by.x = "est", by.y = 0)
  original_est_long[[var]] <- paste0(var, " = ", add_quotes(original_est_long[[var]], chk::vld_character_or_factor(original_est_long[[var]])))

  if (length(non_var_varying) > 0) {
    non_var_varying_f <- do.call("paste", c(lapply(non_var_varying, function(i) {
      paste0(i, " = ", add_quotes(est_long[[i]], chk::vld_character_or_factor(est_long[[i]])))
    }), list(sep = ", ")))
    non_var_varying_f <- factor(non_var_varying_f, levels = unique(non_var_varying_f))

    non_var_varying_f_o <- do.call("paste", c(lapply(non_var_varying, function(i) {
      paste0(i, " = ", add_quotes(original_est_long[[i]], chk::vld_character_or_factor(original_est_long[[i]])))
    }), list(sep = ", ")))
    non_var_varying_f_o <- factor(non_var_varying_f_o, levels = unique(non_var_varying_f_o))
  }
  else {
    non_var_varying_f <- non_var_varying_f_o <- NULL
  }

  p <- ggplot() +
    geom_density(data = est_long, mapping = aes(x = .data$val, color = non_var_varying_f,
                                                fill = non_var_varying_f),
                 alpha = .3, ...) +
    geom_hline(yintercept = 0) +
    geom_vline(data = original_est_long, mapping = aes(xintercept = .data$val,
                                                       color = non_var_varying_f_o)) +
    facet_wrap(vars(.data[[var]]), scales = "free")


  if (ci) {
    ci <- confint(x, level = level, method = method)
    ci_long <- setNames(utils::stack(as.data.frame(t(ci))), c("val", "est"))

    ci_long <- merge(ci_long,
                     newdata[c(var, non_var_varying)],
                     by.x = "est", by.y = 0)
    ci_long[[var]] <- paste0(var, " = ", add_quotes(ci_long[[var]], chk::vld_character_or_factor(ci_long[[var]])))

    if (length(non_var_varying) > 0) {
      non_var_varying_f_ci <- do.call("paste", c(lapply(non_var_varying, function(i) {
        paste0(i, " = ", add_quotes(ci_long[[i]], chk::vld_character_or_factor(ci_long[[i]])))
      }), list(sep = ", ")))
      non_var_varying_f_ci <- factor(non_var_varying_f_ci, levels = unique(non_var_varying_f_ci))
    }
    else {
      non_var_varying_f_ci <- NULL
    }

    p <- p + geom_vline(data = ci_long, mapping = aes(xintercept = .data$val,
                                                      color = non_var_varying_f_ci),
                        linetype = 2)
  }

  p <- p +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Estimate", y = "Density", color = NULL, fill = NULL) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.border = element_rect(color = "black", fill = NA))

  p

}

#Line plot with confidence bands
setx_reg_plot <- function(x, var, non_var_varying = NULL, ci = TRUE, level = .95, method = "quantile") {

  newdata <- attr(x, "setx")

  if (length(non_var_varying)) {
    non_var_varying_f <- do.call("paste", c(lapply(non_var_varying, function(i) {
      paste0(i, " = ", add_quotes(newdata[[i]], chk::vld_character_or_factor(newdata[[i]])))
    }), list(sep = ", ")))
    non_var_varying_f <- factor(non_var_varying_f, levels = unique(non_var_varying_f))
  }
  else {
    non_var_varying_f <- NULL
  }

  if (ci) {
    s <- summary.clarify_est(x, level = level, method = method)[rownames(newdata), , drop = FALSE]
  }
  else {
    s <- matrix(coef(x)[rownames(newdata)], ncol = 1,
                dimnames = list(rownames(newdata), "Estimate"))
  }

  s <- cbind(s, newdata)

  p <- ggplot(s, aes(x = .data[[var]], color = non_var_varying_f,
                     fill = non_var_varying_f)) +
    geom_line(aes(y = .data$Estimate)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = var, y = sprintf("E[Y|%s]", var), color = NULL, fill = NULL)

  if (ci) {
    p <- p +
      geom_ribbon(aes(ymin = .data[[colnames(s)[2]]], ymax = .data[[colnames(s)[3]]],
                      color = NULL),
                  alpha = .3)
  }
  p

}
