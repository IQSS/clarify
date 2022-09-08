#' Compute average marginal effects
#'
#' @description
#' `sim_ame()` is a wrapper for [sim_apply()] that computes average marginal effects, the average effect of changing a single variable from one value to another (i.e., from one category to another for categorical variables or a tiny change for continuous variables).
#'
#' @inheritParams sim_apply
#' @param var either the name of a variable for which marginal effects are to be computed or a named list of length one containing the values the variable should take. If a list is supplied or the named variables is categorical (factor, character, or having two values), categorical calculations will be triggered. Otherwise, continuous calculations will be triggered. See Details.
#' @param subset optional; a vector used to subset the data used to compute the marginal effects. This will be evaluated within the original dataset used to fit the model using [subset()], so nonstandard evaluation is allowed.
#' @param contrast a string containing the name of a contrast between the average marginal means when the variable named in `var` is categorical and takes on two values. Allowed options include `"diff"` for the difference in means (also `"rd"`), `"rr"` for the risk ratio (also `"irr"`), `"log(rr):` for the log risk ratio (also `"log(irr)"`), `"or"` for the odds ratio, `"log(or)"` for the log odds ratio, and `"nnt"` for the number needed to treat. These options are not case sensitive, but the parentheses must be included if present.
#' @param eps when the variable named in `var` is continuous, the value by which to change the variable values to approximate the derivative. See Details.
#'
#' @details
#' `sim_ame()` operates differently depending on whether continuous or categorical calculations are triggered. To trigger categorical calculations, `var` should be a string naming a factor, character, or binary variable or a named list with specific values given (e.g., `var = list(x1 = c(1, 2 ,3))`). Otherwise, continuous calculations are triggered.
#'
#' Categorical calculations involve computing average marginal means at each level of `var`. The average marginal mean is the average predicted outcome value after setting all units' value of `var` to one level. (This quantity has several names, including the average potential outcome, average adjusted prediction, and standardized mean). When `var` only takes on two levels (or it is supplied as a list and only two values are specified), a contrast between the average marginal means can be computed by supplying an argument to `contrast`. Contrasts can be manually computed using [transform()] afterward as well.
#'
#' Continuous calculations involve computing the average of marginal effects of `var` across units. A marginal effect is the instantaneous rate of change corresponding to changing a unit's observed value of `var` by a tiny amount and considering to what degree the predicted outcome changes. The ratio of the change in the predicted outcome to the change in the value of `var` is the marginal effect; these are averaged across the sample to arrive at an average marginal effect. The "tiny amount" used is `eps` times the standard deviation of the focal variable.
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
#' A `simbased_ame` object, which inherits from `simbased_est` and is similar to
#' the output of `sim_apply()`, with the additional attribute `"var"` containing
#' the variable named in `var`. The average marginal means will be named
#' `E[Y({v})]`, where `{v}` is replaced with the values the focal variable
#' (`var`) takes on. The average marginal effect for a continuous `var` will be
#' named `dY/d({x})` where `{x}` is replaced with `var`.
#'
#' @seealso [sim_apply()], which provides a general interface to computing any
#'   quantities for simulation-based inference; [plot.simbased_est()] for plotting the
#'   output of a call to `sim_ame()`; [summary.simbased_est()] for computing
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
                    eps = 1e-5,
                    verbose = TRUE,
                    cl = NULL) {
  chk::chk_is(sim, "simbased_sim")

  if (!isTRUE(attr(sim, "use_fit"))) {
    chk::err("`sim_ame()` can only be used when a model fit was supplied to the original call to `sim()`")
  }
  chk::chk_flag(verbose)
  is_simmi <- inherits(sim, "simbased_simmi")

  vals <- NULL

  if (missing(var)) {
    chk::err("`var` must be supplied, identifying the focal variable")
  }
  if (is.list(var) && chk::vld_named(var) && length(var) == 1) {
    vals <- var[[1]]
    var <- names(var)
  }
  else if (!chk::vld_string(var)) {
    chk::err("`var` must be the name of the desired focal variable or a named list of length 1 with its values")
  }

  if (is_simmi) {
    dat <- do.call("rbind", lapply(sim$fit, insight::get_predictors))
  }
  else {
    dat <- insight::get_predictors(sim$fit)
  }

  if (!var %in% names(dat)) {
    chk::err(sprintf("the variable \"%s\" named in `var` is not present in the original model",
                     var))
  }

  var_val <- dat[[var]]
  rm(dat)

  if (!is.null(vals) && !all(vals %in% var_val)) {
    chk::err(sprintf("the values mentioned in `var` must be values %s takes on", var))
  }

  type <- {
    if (!is.null(vals) || chk::vld_character_or_factor(var_val) ||
        is.logical(var_val) || length(unique(var_val)) <= 2)
      "contrast"
    else
      "slope"
  }

  index.sub <- substitute(subset)
  sim$fit <- attach_pred_data_to_fit(sim$fit, index.sub = index.sub,
                                     is_fitlist = is_simmi)

  if (type == "contrast") {
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
      vapply(vals, function(x) {
        dat[[var]][] <- x
        mean(simbased_predict(fit, newdata = dat))
      }, numeric(1L))
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

    names(out) <- paste0("M", seq_along(vals))

    if (!is.null(contrast)) {
      out <- transform(out,
                       `.C` = switch(tolower(contrast),
                                     "diff" =, "rd" = M2 - M1,
                                     "nnt" = 1/(M2 - M1),
                                     "irr" =, "rr" = M2 / M1,
                                     "log(irr)" =, "log(rr)" = log(M2 / M1),
                                     "or" = (M2/(1-M2))/(M1/(1-M1)),
                                     "log(or)" = log((M2/(1-M2))/(M1/(1-M1)))))
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
      dat[[var]] <- dat[[var]] - eps/2
      m0 <- mean(simbased_predict(fit, newdata = dat))
      dat[[var]] <- dat[[var]] + eps
      m1 <- mean(simbased_predict(fit, newdata = dat))

      (m1 - m0)/eps
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

    names(out) <- sprintf("dY/d(%s)", var)
  }

  attr(out, type) <- type
  attr(out, "var") <- var
  class(out) <- c("simbased_ame", class(out))
  out
}

#' @export
print.simbased_ame <- function(x, digits = NULL, ...) {
  cat("A `simbased_est` object (from `sim_ame()`)\n")

  cat(sprintf(" - Average marginal effect of `%s`\n", attr(x, "var")))
  cat(sprintf(" - %s simulated values\n", nrow(x)))
  cat(sprintf(" - %s %s estimated:\n", length(attr(x, "original")),
              ngettext(length(attr(x, "original")), "quantity", "quantities")))
  print(attr(x, "original"))

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
    data <- insight::get_data(fit)
    vars <- insight::find_predictors(fit, effects = "fixed", component = "all",
                                     flatten = TRUE)
    if (!is.null(index.sub)) {
      subset <- eval(index.sub, data, parent.frame(2))

      if (!chk::vld_atomic(subset)) {
        chk::err("`subset` must evaluate to an atomic vector")
      }
      if (is.logical(subset) && length(subset) != nrow(data)) {
        chk::err("when `subset` is logical, it must have the same length as the original dataset")
      }
      if (length(subset) > 0) {
        data <- data[subset,]
      }
    }

    attr(fit, "simbased_data") <- data[,intersect(vars, colnames(data)), drop = FALSE]
  }
  return(fit)
}

get_pred_data_from_fit <- function(fit) {
  attr(fit, "simbased_data")
}
