#' Compute average marginal effects
#'
#' @description
#' `sim_ame()` is a wrapper for [sim_apply()] that computes average marginal effects, the average effect of changing a single variable from one value to another (i.e., from one category to another for categorical variables or a tiny change for continuous variables).
#'
#' @inheritParams sim_apply
#' @param var either the name of a variable for which marginal effects are to be computed or a named list of length one containing the values the variable should take. If a list is supplied or the named variables is categorical (factor, character, or having two values), categorical calculations will be triggered. Otherwise, continuous calculations will be triggered. See Details.
#' @param subset optional; a vector used to subset the data used to compute the marginal effects. This will be evaluated within the original dataset used to fit the model using [subset()], so nonstandard evaluation is allowed.
#' @param contrast a string containing the name of a contrast between the average marginal means when the variable named in `var` is categorical and takes on two values. Allowed options include `"diff"` for the difference in means (also `"rd"`), `"rr"` for the risk ratio (also `"irr"`), `"log(rr):` for the log risk ratio (also `"log(irr)"`), `"or"` for the odds ratio, `"log(or)"` for the log odds ratio, and `"nnt"` for the number needed to treat. These options are not case sensitive, but the parentheses must be included if present.
#' @param outcome a string containing the name of the outcome or outcome level for multivariate (multiple outcomes) or multi-category outcomes. Ignored for univariate (single outcome) and binary outcomes.
#' @param type a string containing the type of predicted values (e.g., the link or the response). Passed to [marginaleffects::get_predict()] and eventually to `predict()` in most cases. The default and allowable option depend on the type of model supplied, but almost always corresponds to the response scale (e.g., predicted probabilities for binomial models).
#' @param eps when the variable named in `var` is continuous, the value by which to change the variable values to approximate the derivative. See Details.
#'
#' @details
#' `sim_ame()` operates differently depending on whether continuous or categorical calculations are triggered. To trigger categorical calculations, `var` should be a string naming a factor, character, or binary variable or a named list with specific values given (e.g., `var = list(x1 = c(1, 2 ,3))`). Otherwise, continuous calculations are triggered.
#'
#' Categorical calculations involve computing average marginal means at each level of `var`. The average marginal mean is the average predicted outcome value after setting all units' value of `var` to one level. (This quantity has several names, including the average potential outcome, average adjusted prediction, and standardized mean). When `var` only takes on two levels (or it is supplied as a list and only two values are specified), a contrast between the average marginal means can be computed by supplying an argument to `contrast`. Contrasts can be manually computed using [transform()] afterward as well.
#'
#' Continuous calculations involve computing the average of marginal effects of `var` across units. A marginal effect is the instantaneous rate of change corresponding to changing a unit's observed value of `var` by a tiny amount and considering to what degree the predicted outcome changes. The ratio of the change in the predicted outcome to the change in the value of `var` is the marginal effect; these are averaged across the sample to arrive at an average marginal effect. The "tiny amount" used is `eps` times the standard deviation of the focal variable.
#'
#' If unit-level weights are included in the model fit (and discoverable using [insight::get_weights()]), all means will be computed as weighted means.
#'
#' ## Effect measures
#'
#' The effect measures specified in `contrast` are defined below. Typically only `"diff"` is appropriate for continuous outcomes and `"diff"` or `"irr"` are appropriate for count outcomes; the rest are appropriate for binary outcomes. For a focal variable with two levels, `0` and `1`, and an outcome `Y`, the average marginal means will be denoted in the below formulas as `E[Y(0)]` and `E[Y(1)]`, respectively.
#' |`contrast`| Formula|
#' | --- | --- |
#' |`"diff"`| `E[Y(1)] - E[Y(0)]` |
#' |`"rr"`  | `E[Y(1)] / E[Y(0)]` |
#' |`"or"`  | `O[Y(1)] / O[Y(0)]`, where `O[Y(.)]` = `E[Y(.)] / (1 - E[Y(.)])` |
#' |`"nnt"` | `1 / (E[Y(1)] - E[Y(0)])` |
#'
#' The `log(.)` versions are defined by taking the [log()] (natural log) of the corresponding effect measure.
#'
#' @return
#' A `clarify_ame` object, which inherits from `clarify_est` and is similar to
#' the output of `sim_apply()`, with the additional attribute `"var"` containing
#' the variable named in `var`. The average marginal means will be named
#' `E[Y({v})]`, where `{v}` is replaced with the values the focal variable
#' (`var`) takes on. The average marginal effect for a continuous `var` will be
#' named `dY/d({x})` where `{x}` is replaced with `var`.
#'
#' @seealso [sim_apply()], which provides a general interface to computing any
#'   quantities for simulation-based inference; [plot.clarify_est()] for plotting the
#'   output of a call to `sim_ame()`; [summary.clarify_est()] for computing
#'   p-values and confidence intervals for the estimated quantities.
#'
#' `marginaleffects::marginaleffects()`, `marginaleffects::comparisons()`, and `margins::margins()` for delta method-based implementations of computing average marginal effects.
#'
#' @examples
#' data("lalonde", package = "MatchIt")
#'
#' # Fit the model
#' fit <- glm(I(re78 > 0) ~ treat + age + race + re74,
#'            data = lalonde, family = binomial)
#'
#' # Simulate coefficients
#' set.seed(123)
#' s <- sim(fit, n = 100)
#'
#' # Average marginal effect of `age`
#' est <- sim_ame(s, var = "age", verbose = FALSE)
#' summary(est)
#'
#' # Contrast between average marginal means for `treat`
#' est <- sim_ame(s, var = "treat", contrast = "rr",
#'                verbose = FALSE)
#' summary(est)
#'
#' # Average marginal means for `race`; need to follow up
#' # with contrasts for specific levels
#' est <- sim_ame(s, var = "race", verbose = FALSE)
#'
#' est <- transform(est,
#'                  `RR(h,b)` = `E[Y(hispan)]` / `E[Y(black)]`)
#'
#' summary(est)
#'
#' @export
sim_ame <- function(sim,
                    var,
                    subset = NULL,
                    contrast = NULL,
                    outcome = NULL,
                    type = NULL,
                    eps = 1e-5,
                    verbose = TRUE,
                    cl = NULL) {

  check_sim_apply_wrapper_ready(sim)

  chk::chk_flag(verbose)
  is_misim <- inherits(sim, "clarify_misim")

  vals <- NULL

  if (missing(var)) {
    .err("`var` must be supplied, identifying the focal variable")
  }
  if (is.list(var) && chk::vld_named(var) && length(var) == 1) {
    vals <- var[[1]]
    var <- names(var)
  }
  else if (!chk::vld_string(var)) {
    .err("`var` must be the name of the desired focal variable or a named list of length 1 with its values")
  }

  if (is_misim) {
    dat <- do.call("rbind", lapply(sim$fit, insight::get_predictors, verbose = FALSE))
  }
  else {
    dat <- insight::get_predictors(sim$fit, verbose = FALSE)
  }

  if (!var %in% names(dat)) {
    .err(sprintf("the variable \"%s\" named in `var` is not present in the original model",
                     var))
  }

  var_val <- dat[[var]]
  rm(dat)

  if (!is.null(vals) && !all(vals %in% var_val)) {
    .err(sprintf("the values mentioned in `var` must be values %s takes on", var))
  }

  ame_type <- {
    if (!is.null(vals) || chk::vld_character_or_factor(var_val) ||
        is.logical(var_val) || length(unique(var_val)) <= 2)
      "contrast"
    else
      "slope"
  }

  index.sub <- substitute(subset)
  sim$fit <- attach_pred_data_to_fit(sim$fit, index.sub = index.sub,
                                     is_fitlist = is_misim)

  #Test to make sure compatible
  if (is_misim) {
    test_dat <- get_pred_data_from_fit(sim$fit[[1]])
    test_predict <- clarify_predict(sim$fit[[1]], newdata = test_dat, group = NULL, type = type)
  }
  else {
    test_dat <- get_pred_data_from_fit(sim$fit)
    test_predict <- clarify_predict(sim$fit, newdata = test_dat, group = NULL, type = type)
  }

  if ("group" %in% names(test_predict) && length(unique_group <- unique(test_predict$group)) > 1) {
    if (is.null(outcome)) {
      .err("`outcome` must be supplied with multivariate models and models with multi-category outcomes")
    }
    chk::chk_string(outcome)
    if (!outcome %in% unique_group) {
      .err("only the following values of `outcome` are allowed: ", paste(add_quotes(unique_group), collapse = ", "))
    }
    test_predict <- subset_group(test_predict, outcome)
  }
  else {
    if (!is.null(outcome)) {
      chk::wrn("`outcome` is ignored for univariate models")
    }
    outcome <- NULL
  }

  if (nrow(test_predict) != nrow(test_dat)) {
    .err("not all units received a predicted value, suggesting a bug.")
  }

  if (ame_type == "contrast") {
    if (is.null(vals)) {
      vals <- if (is.factor(var_val)) levels(var_val) else sort(unique(var_val))
    }

    if (length(vals) == 2) {
      if (!is.null(contrast)) {
        chk::chk_string(contrast)
        contrast <- tolower(contrast)
        contrast <- match_arg(contrast, c("diff", "rd", "irr", "rr", "log(irr)",
                                          "log(rr)", "or", "log(or)", "nnt"))
      }
    }
    else if (length(vals) > 2) {
      if (!is.null(contrast)) {
        chk::wrn("`contrast` is ignored when the focal variable takes on more than two levels")
        contrast <- NULL
      }
    }
    else {
      contrast <- NULL
    }

    FUN <- function(fit) {
      dat <- get_pred_data_from_fit(fit)
      m <- nrow(dat)
      dat2 <- do.call("rbind", lapply(vals, function(v) {
        dat[[var]][] <- v
        dat
      }))
      pred <- clarify_predict(fit, newdata = dat2, group = outcome, type = type)
      p <- get_p(pred)
      vapply(seq_along(vals), function(i) {
        weighted.mean(p[seq_len(m) + (i - 1) * m], attr(fit, "weights"))
      }, numeric(1L))
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

    names(out) <- paste0("M", seq_along(vals))

    if (!is.null(contrast)) {
      out <- transform(out,
                       `.C` = switch(tolower(contrast),
                                     "diff" = , "rd" = M2 - M1,
                                     "nnt" = 1 / (M2 - M1),
                                     "irr" = , "rr" = M2 / M1,
                                     "log(irr)" = , "log(rr)" = log(M2 / M1),
                                     "or" = (M2 / (1 - M2)) / (M1 / (1 - M1)),
                                     "log(or)" = log((M2 / (1 - M2)) / (M1 / (1 - M1)))))
      names(out)[3] <- rename_contrast(contrast)
    }

    names(out)[seq_along(vals)] <- sprintf("E[Y(%s)]", vals)

  }
  else {
    chk::chk_number(eps)
    chk::chk_gt(eps)

    if (!is.null(contrast)) {
      chk::wrn("`contrast` is ignored when the focal variable is continuous")
      contrast <- NULL
    }

    eps <- eps * sd(var_val)

    FUN <- function(fit) {
      dat <- get_pred_data_from_fit(fit)
      ind <- seq_len(nrow(dat))
      dat2 <- rbind(dat, dat)

      dat2[[var]][ind] <- dat2[[var]][ind] - eps / 2
      dat2[[var]][-ind] <- dat2[[var]][-ind] + eps / 2

      pred <- clarify_predict(fit, newdata = dat2, group = outcome, type = type)
      p <- get_p(pred)

      m0 <- weighted.mean(p[ind], attr(fit, "weights"))
      m1 <- weighted.mean(p[-ind], attr(fit, "weights"))

      (m1 - m0) / eps
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

    names(out) <- sprintf("dY/d(%s)", var)
  }

  attr(out, "var") <- var
  class(out) <- c("clarify_ame", class(out))
  out
}

#' @exportS3Method print clarify_ame
#' @rdname sim_ame
#' @param x a `clarify_ame` object.
#' @param digits the minimum number of significant digits to be used; passed to [print.data.frame()].
#' @param max.ests the maximum number of estimates to display.
print.clarify_ame <- function(x, digits = NULL, max.ests = 6, ...) {
  chk::chk_count(max.ests)
  max.ests <- min(max.ests, length(attr(x, "original")))

  cat("A `clarify_est` object (from `sim_ame()`)\n")

  cat(sprintf(" - Average marginal effect of `%s`\n", attr(x, "var")))
  cat(sprintf(" - %s simulated values\n", nrow(x)))
  cat(sprintf(" - %s %s estimated:", length(attr(x, "original")),
              ngettext(length(attr(x, "original")), "quantity", "quantities")))

  print.data.frame(data.frame(names(attr(x, "original"))[seq_len(max.ests)],
                              attr(x, "original")[seq_len(max.ests)],
                              fix.empty.names	= FALSE),
                   row.names = FALSE, right = FALSE, digits = digits)
  if (max.ests != length(attr(x, "original"))) {
    cat(sprintf("# ... and %s more\n", length(attr(x, "original")) - max.ests))
  }
  invisible(x)
}

rename_contrast <- function(x) {
  if (length(x) == 0) return(character(0))
  vapply(tolower(x), switch, character(1L),
         "diff" = "Diff",
         "log(irr)" = "log(IRR)",
         "log(rr)" = "log(RR)",
         "log(or)" = "log(OR)",
         toupper(x))
}

attach_pred_data_to_fit <- function(fit, index.sub = NULL, is_fitlist = FALSE) {
  if (is_fitlist) {
    fit <- lapply(fit, attach_pred_data_to_fit, index.sub)
  }
  else {
    data <- insight::get_data(fit, verbose = FALSE)
    weights <- insight::get_weights(fit, null_as_ones = TRUE)
    vars <- insight::find_predictors(fit, effects = "fixed", component = "all",
                                     flatten = TRUE)
    if (!is.null(index.sub)) {
      subset <- eval(index.sub, data, parent.frame(2))

      if (!chk::vld_atomic(subset)) {
        .err("`subset` must evaluate to an atomic vector")
      }
      if (is.logical(subset) && length(subset) != nrow(data)) {
        .err("when `subset` is logical, it must have the same length as the original dataset")
      }
      if (length(subset) > 0) {
        data <- data[subset, ]
        weights <- weights[subset]
      }
    }

    attr(fit, "clarify_data") <- data[, intersect(vars, colnames(data)), drop = FALSE]
    attr(fit, "weights") <- weights
  }
  return(fit)
}

get_pred_data_from_fit <- function(fit) {
  attr(fit, "clarify_data")
}
