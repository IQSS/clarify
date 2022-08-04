#' @export
sim_comparisons <- function(sim, newdata = NULL, variables = NULL, type = "response",
                            transform_pre = "differenceavg", transform_post = NULL, interaction = NULL,
                            by = NULL, wts = NULL, eps = NULL,
                            verbose = TRUE, cl = NULL, ...) {

  args <- list(...)
  FUN <- function(fit) {
    comp <- do.call(marginaleffects::comparisons, c(list(fit, newdata = newdata, variables = variables, type = type,
                                                         transform_pre = transform_pre, transform_post = transform_post,
                                                         interaction = interaction, by = by, wts = wts, eps = eps,
                                                         vcov = FALSE),
                                                    args))
    comp$comparison
  }

  sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)
}

#' @export
sim_setx <- function(sim, x = NULL, x1 = NULL, verbose = TRUE, cl = NULL, ...) {

  dat <- insight::get_predictors(sim$fit)

  #Check `x` and note which vars are to be set by arguments (`set_preds`) and
  #which are set automatically (`auto_preds`)
  x_okay <- TRUE
  if (length(x) == 0) {
    set_preds <- NULL
    auto_preds <- names(dat)
  }
  else if (is.list(x)) {
    if (is.null(names(x)) || any(names(x) == "")) x_okay <- FALSE
    else if (any(!names(x) %in% names(dat))) {
      vars_not_in_model <- setdiff(names(x), names(dat))
      chk::err(sprintf("the %s %s named in `x` not present in the original model",
                       ngettext(length(vars_not_in_model), "variable", "variables"),
                       word_list(vars_not_in_model, quotes = TRUE),
                       ngettext(length(vars_not_in_model), "is", "are")))
    }
    else if (!chk::vld_all(x, is.atomic)) {
      x_okay <- FALSE
    }
    else {
      set_preds <- names(x)[lengths(x) > 0]
      auto_preds <- setdiff(names(dat), set_preds)
    }
  }
  else {
    x_okay <- FALSE
  }

  if (!x_okay) {
    chk::err("the argument to `x` must be a named list of values for variables to be set to")
  }

  #Create list of automatically set predictors
  if (length(auto_preds) > 0) {
    Mode <- function(v, na.rm = TRUE) {
      if (anyNA(v)) {
        if (na.rm) v <- v[!is.na(v)]
        else {
          #Return NA, keeping type of `v`
          v <- v[1]
          is.na(v) <- TRUE
          return(v)
        }
      }

      if (length(v) == 0) return(v)
      if (is.factor(v)) {
        if (nlevels(v) == 1) return(levels(v)[1])
        mode <- levels(v)[which.max(tabulate(v, nbins = nlevels(v)))]
        mode <- factor(mode, levels = levels(v))
      }
      else {
        uv <- unique(v)
        if (length(uv) == 1) return(uv)
        mode <- uv[which.max(tabulate(match(v, uv)))]
      }
      mode
    }
    x_auto <- setNames(lapply(dat[auto_preds], function(p) {
      if (is.numeric(p)) {
        if (length(unique(p)) == 2) Mode(p)
        else mean(p, na.rm = TRUE)
      }
      else if (is.ordered(p)) {
        median(p, na.rm = TRUE)
      }
      else {
        Mode(p)
      }
    }), auto_preds)
  }
  else {
    x_auto <- list()
  }

  #Create list of manually set predictors
  if (length(set_preds) > 0) {
    x <- x[set_preds]
    set_pred_lengths <- lengths(x)

    newdata <- do.call("expand.grid", c(x, x_auto, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))

    if (nrow(newdata) > 1) {
      rownames(newdata) <- do.call("paste", c(lapply(set_preds[set_pred_lengths > 1], function(i) {
        paste0(i, " = ", add_quotes(newdata[[i]], chk::vld_character_or_factor(newdata[[i]])))
      }), list(sep = ", ")))
    }
  }
  else {
    newdata <- as.data.frame(x_auto)
    rownames(newdata) <- "Typical"
  }

  newdata <- newdata[names(dat)]

  #Check to make sure inputs are the right type
  check_classes(dat, newdata)

  #make FUN for sim_apply()
  FUN <- function(fit) {
    predict(fit, newdata = newdata, type = "response")
  }

  out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)
  attr(out, "setx") <- newdata
  attr(out, "set_preds") <- set_preds
  out
}

#' @export
setx_plot <- function(x, var = NULL, ci = TRUE, alpha = .05, normal = FALSE) {
  if (is.null(attr(x, "setx"))) {
    chk::err("`x` must be the output of a call to `sim_setx()`")
  }

  chk::chk_number(alpha)
  if (alpha >= 1 || alpha <= 0) {
    chk::err("`alpha` must be between 0 and 1.")
  }

  newdata <- attr(x, "setx")

  if (nrow(newdata) == 1) {
    if (!is.null(var)) {
      chk::wrn("ignoring `var` because no variables vary over predictions")
    }
    return(sim_plot(x, est = 1, ci = length(alpha) > 0, alpha = alpha))
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
      var <- attr(x, "set_preds")[which.max(len_unique_newdata[attr(x, "set_preds")])]
    }
    else {
      var <- attr(x, "set_preds")[attr(x, "set_preds") %in% varying][1]
    }
  }
  else {
    chk::chk_string(var)
    if (!var %in% varying) {
      chk::err("`var` must be the name of a predictor set to be varying. Allowable options include ", word_list(varying, quote = TRUE))
    }
  }

  non_var_varying <- setdiff(varying, var)

  if (len_unique_newdata[var] == 2 || chk::vld_character_or_factor(newdata[[var]])) {
    p <- setx_sim_plot(x, var, non_var_varying, ci = ci,
                       alpha = alpha, normal = normal)
  }
  else {
    p <- setx_reg_plot(x, var, non_var_varying, ci = ci,
                       alpha = alpha, normal = normal)
  }

  p
}

#sim_plot, but with grouping by non_var_varying if present
setx_sim_plot <- function(x, var, non_var_varying = NULL, ci = TRUE, alpha = .05, normal = FALSE) {

  newdata <- attr(x, "setx")
  original_est <- coef(x)
  est_names <- rownames(newdata)

  est_long <- setNames(stack(as.data.frame(x[,est_names, drop = FALSE])),
                       c("val", "est"))
  est_long <- merge(est_long,
                    newdata[c(var, non_var_varying)],
                    by.x = "est", by.y = 0)
  est_long[[var]] <- paste(var, " = ", add_quotes(est_long[[var]], chk::vld_character_or_factor(est_long[[var]])))

  original_est_long <- setNames(stack(original_est[est_names]),
                                c("val", "est"))
  original_est_long <- merge(original_est_long,
                             newdata[c(var, non_var_varying)],
                             by.x = "est", by.y = 0)
  original_est_long[[var]] <- paste(var, " = ", add_quotes(original_est_long[[var]], chk::vld_character_or_factor(original_est_long[[var]])))

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
                 alpha = .3) +
    geom_hline(yintercept = 0) +
    geom_vline(data = original_est_long, mapping = aes(xintercept = .data$val,
                                                       color = non_var_varying_f_o))

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

    ci_long <- setNames(stack(ci), c("val", "est"))
    ci_long <- merge(ci_long,
                     newdata[c(var, non_var_varying)],
                     by.x = "est", by.y = 0)
    ci_long[[var]] <- paste(var, " = ", add_quotes(ci_long[[var]], chk::vld_character_or_factor(ci_long[[var]])))

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

  p <- p + facet_wrap(vars(.data[[var]]), scales = "free") +
    labs(x = "Estimate", y = "Density", color = NULL, fill = NULL) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.border = element_rect(color = "black", fill = NA))

  p

}

#Line plot with confidence bands
setx_reg_plot <- function(x, var, non_var_varying = NULL, ci = TRUE, alpha = .05, normal = FALSE) {

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
    s <- summary.simbased_est(x, alpha = alpha, normal = normal)$coefficients[rownames(newdata),, drop = FALSE]
  }
  else {
    s <- matrix(coef(x)[rownames(newdata)], ncol = 1,
                dimnames = list(rownames(newdata), "Estimate"))
  }

  s <- cbind(s, newdata)

  p <- ggplot(s, aes(x = .data[[var]], color = non_var_varying_f,
                     fill = non_var_varying_f)) +
    geom_line(aes(y = .data$Estimate)) +
    labs(x = var, y = "E[Y|X]", color = NULL, fill = NULL)

  if (ci) {
    p <- p +
      geom_ribbon(aes(ymin = .data[[colnames(s)[2]]], ymax = .data[[colnames(s)[3]]],
                      color = NULL),
                  alpha = .3)
  }
  p

}
