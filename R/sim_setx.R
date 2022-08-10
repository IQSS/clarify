#' Compute predictions and first differences at set values
#'
#' `sim_setx()` is a wrapper for [sim_apply()] that computes predicted values of
#' the outcome at specified values of the predictors, sometimes called marginal
#' predictions. One can also compute the difference between two marginal
#' predictions (the "first difference"). Although any function that accepted
#' `simbased_est` objects can be used with `sim_setx()` output objects, a
#' special plotting function, [setx_plot()], can be used to plot marginal
#' predictions.
#'
#' @inheritParams sim_apply
#' @param x a named list of values each predictor should take defining a
#'   reference grid of predictor values, e.g., `list(v1 = 1:4, v2 = c("A",
#'   "B"))`. Any omitted predictors are fixed at a "typical" value. See Details.
#'   When `x1` is specified, `x` should identify a single reference unit.
#' @param x1 a named list of the value each predictor should take to compute the
#'   first difference from the predictor combination specified in `x`. `x1` can
#'   only identify a single unit. See Details.
#'
#' @return a `simbased_est` object, similar to the output of `sim_apply()`, with
#'   the following additional attributes: * `"setx"` - a data frame containing
#'   the values at which predictions are to be made * `"fd"` - whether or not
#'   the first difference is to be computed; set to `TRUE` if `x1` is specified
#'   and `FALSE` otherwise
#'
#' @details `x` should be a named list of predictor values that will be crossed
#'   to form a reference grid for the marginal predictions. Any predictors not
#'   set in `x` are assigned their "typical" value, which, for factor,
#'   character, logical, and binary variables is the mode, for numeric variables
#'   is the mean, and for ordered variables is the median. These values can be
#'   seen in the `"setx"` attribute of the output object. If `x` is empty, a
#'   prediction will be made at a point corresponding to the typical value of
#'   every predictor. Estimates are identified (in `summary()`, etc.) only by
#'   the variables that differ across predictions.
#'
#'   When `x1` is supplied, the first difference is computed, which here is
#'   considered as the difference between two marginal predictions. One marginal
#'   prediction must be specified in `x` and another, ideally with a single
#'   predictor changed, specified in `x1`.
#'
#' @seealso [sim_apply()], which provides a general interface to computing any
#'   quantities for simulation-based inference; [setx_plot()] for plotting the
#'   output of a call to `sim_setx()`; [summary.simbased_est()] for computing
#'   p-values and confidence intervals for the estimated quantities.
#'
#' @examples
#' data("lalonde", package = "MatchIt")
#'
#' fit <- lm(re78 ~ treat + age + educ + married + race + re74,
#'           data = lalonde)
#'
#' # Simulate coefficients
#' set.seed(123)
#' s <- sim(fit, n = 100)
#'
#' # Predicted values at specified values of treat, typical
#' # values for other predictors
#' est <- sim_setx(s, x = list(treat = 0:1,
#'                             re74 = c(0, 10000)),
#'                 verbose = FALSE)
#' summary(est)
#' setx_plot(est)
#'
#' # Predicted values at specified grid of values, typical
#' # values for other predictors
#' est <- sim_setx(s, x = list(age = c(20, 25, 30, 35),
#'                             married = 0:1),
#'                 verbose = FALSE)
#' summary(est)
#' setx_plot(est)
#'
#' # First differences of treat at specified value of
#' # race, typical values for other predictors
#' est <- sim_setx(s, x = list(treat = 0, race = "hispan"),
#'                 x1 = list(treat = 1, race = "hispan"),
#'                 verbose = FALSE)
#' summary(est)
#' setx_plot(est)
#'
#' @export
sim_setx <- function(sim, x = list(), x1 = list(), verbose = TRUE, cl = NULL) {
  chk::chk_is(sim, "simbased_sim")
  chk::chk_flag(verbose)

  dat <- insight::get_predictors(sim$fit)

  x <- process_x(x, dat, "x")

  #Create list of manually set predictors
  newdata <- do.call("expand.grid", c(x, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))

  fd <- length(x1) > 0
  if (fd) {
    if (length(attr(x, "set_preds")) == 0) {
      chk::err("when `x1` is specified, `x` must be specified")
    }
    if (!all(lengths(x) == 1)) {
      chk::err("when `x1` is specified, `x` must identify a single reference unit")
    }

    x1 <- process_x(x1, dat, "x1")

    if (!all(lengths(x1) == 1)) {
      chk::err("`x1` must identify a single unit")
    }

    if (!setequal(attr(x, "set_preds"), attr(x1, "set_preds"))) {
      chk::err("when `x1` is specified, the same variables must be specified in `x` and `x1`")
    }

    newdata_x1 <- do.call("expand.grid", c(x1, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))

    newdata <- rbind(newdata, newdata_x1)
  }

  set_preds <- attr(x, "set_preds")
  if (length(set_preds) > 0) {
    set_pred_lengths <- vapply(newdata[set_preds], function(i) length(unique(i)), integer(1L))

    if (nrow(newdata) > 1) {
      rownames(newdata) <- do.call("paste", c(lapply(set_preds[set_pred_lengths > 1], function(i) {
        paste0(i, " = ", add_quotes(newdata[[i]], chk::vld_character_or_factor(newdata[[i]])))
      }), list(sep = ", ")))
    }
  }
  else {
    rownames(newdata) <- "Typical"
  }
  attr(newdata, "set_preds") <- set_preds

  #make FUN for sim_apply()
  if (fd) {
    FUN <- function(fit) {
      p <- simbased_predict(fit, newdata = newdata)
      c(p, "FD" = unname(diff(p)))
    }
  }
  else {
    FUN <- function(fit) {
      simbased_predict(fit, newdata = newdata)
    }
  }

  out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)
  attr(out, "setx") <- newdata
  attr(out, "fd") <- fd
  out
}

process_x <- function(x, dat, arg_name) {
  x_okay <- TRUE
  if (length(x) == 0) {
    set_preds <- NULL
    auto_preds <- names(dat)
  }
  else if (is.list(x)) {
    if (is.null(names(x)) || any(names(x) == "")) x_okay <- FALSE
    else if (any(!names(x) %in% names(dat))) {
      vars_not_in_model <- setdiff(names(x), names(dat))
      chk::err(sprintf("the %s %s named in `%s` %s not present in the original model",
                       ngettext(length(vars_not_in_model), "variable", "variables"),
                       word_list(vars_not_in_model, quotes = TRUE),
                       arg_name,
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
    chk::err(sprintf("the argument to `%s` must be a named list of values for variables to be set to",
                     arg_name))
  }

  if (length(auto_preds) > 0) {
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

    x <- c(x, x_auto)
  }

  x <- x[names(dat)]

  #Check to make sure inputs are the right type
  check_classes(dat, x)

  attr(x, "set_preds") <- set_preds
  attr(x, "auto_preds") <- auto_preds

  return(x)
}
