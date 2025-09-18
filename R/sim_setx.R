#' Compute predictions and first differences at set values
#'
#' `sim_setx()` is a wrapper for [sim_apply()] that computes predicted values of the outcome at specified values of the predictors, sometimes called marginal predictions. One can also compute the difference between two marginal predictions (the "first difference"). Although any function that accepted `clarify_est` objects can be used with `sim_setx()` output objects, a special plotting function, [plot.clarify_setx()], can be used to plot marginal predictions.
#'
#' @inheritParams sim_apply
#' @param x a data.frame containing a reference grid of predictor values or a named list of values each predictor should take defining such a reference grid, e.g., `list(v1 = 1:4, v2 = c("A", "B"))`. Any omitted predictors are fixed at a "typical" value. See Details. When `x1` is specified, `x` should identify a single reference unit.
#'
#'   For `print()`, a `clarify_setx` object.
#' @param x1 a data.frame or named list of the value each predictor should take to compute the
#'   first difference from the predictor combination specified in `x`. `x1` can
#'   only identify a single unit. See Details.
#' @param outcome a string containing the name of the outcome or outcome level for multivariate (multiple outcomes) or multi-category outcomes. Ignored for univariate (single outcome) and binary outcomes.
#' @param type a string containing the type of predicted values (e.g., the link or the response). Passed to [marginaleffects::get_predict()] and eventually to `predict()` in most cases. The default and allowable option depend on the type of model supplied, but almost always corresponds to the response scale (e.g., predicted probabilities for binomial models).
#' @param \dots for `sim_setx()`, additional arguments passed to [marginaleffects::get_predict()] (and eventually to `predict()`) to compute predictions. For `print()`, ignored.
#' @param digits the minimum number of significant digits to be used; passed to [print.data.frame()].
#' @param max.ests the maximum number of estimates to display.
#'
#' @returns
#' A `clarify_setx` object, which inherits from `clarify_est` and is similar to the output of `sim_apply()`, with the following additional attributes:
#' * `"setx"` - a data frame containing the values at which predictions are to be made
#' * `"fd"` - whether or not the first difference is to be computed; set to `TRUE` if `x1` is specified and `FALSE` otherwise
#'
#' @details
#' When `x` is a named list of predictor values, they will be crossed
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
#' @seealso
#' [sim_apply()], which provides a general interface to computing any
#'   quantities for simulation-based inference; [plot.clarify_setx()] for plotting the
#'   output of a call to `sim_setx()`; [summary.clarify_est()] for computing
#'   p-values and confidence intervals for the estimated quantities.
#'
#' @examplesIf rlang::is_installed("MatchIt")
#' data("lalonde", package = "MatchIt")
#'
#' fit <- lm(re78 ~ treat + age + educ + married + race + re74,
#'           data = lalonde)
#'
#' # Simulate coefficients
#' set.seed(123)
#' s <- sim(fit, n = 100)
#'
#' # Predicted values at specified values of values, typical
#' # values for other predictors
#' est <- sim_setx(s, x = list(treat = 0:1,
#'                             re74 = c(0, 10000)),
#'                 verbose = FALSE)
#' summary(est)
#' plot(est)
#'
#' # Predicted values at specified grid of values, typical
#' # values for other predictors
#' est <- sim_setx(s, x = list(age = c(20, 25, 30, 35),
#'                             married = 0:1),
#'                 verbose = FALSE)
#' summary(est)
#' plot(est)
#'
#' # First differences of treat at specified value of
#' # race, typical values for other predictors
#' est <- sim_setx(s, x = data.frame(treat = 0, race = "hispan"),
#'                 x1 = data.frame(treat = 1, race = "hispan"),
#'                 verbose = FALSE)
#' summary(est)
#' plot(est)

#' @export
sim_setx <- function(sim,
                     x = list(),
                     x1 = list(),
                     outcome = NULL,
                     type = NULL,
                     verbose = TRUE,
                     cl = NULL,
                     ...) {

  check_sim_apply_wrapper_ready(sim)

  chk::chk_flag(verbose)
  is_misim <- inherits(sim, "clarify_misim")

  dat <- {
    if (is_misim)
      do.call("rbind", lapply(sim$fit, insight::get_predictors, verbose = FALSE))
    else
      insight::get_predictors(sim$fit, verbose = FALSE)
  }

  sim$fit <- .attach_pred_data_to_fit(sim$fit, is_fitlist = is_misim)

  newdata <- process_x(x, dat, "x")

  #Test to make sure compatible
  if (is_misim) {
    test_dat <- .get_pred_data_from_fit(sim$fit[[1L]])[1L, , drop = FALSE]
    test_predict <- clarify_predict(sim$fit[[1L]], newdata = test_dat, group = NULL, type = type, ...)
  }
  else {
    test_dat <- .get_pred_data_from_fit(sim$fit)[1L, , drop = FALSE]
    test_predict <- clarify_predict(sim$fit, newdata = test_dat, group = NULL, type = type, ...)
  }

  if (hasName(test_predict, "group") && length(unique_group <- unique(test_predict$group)) > 1L) {
    if (is_null(outcome)) {
      .err("`outcome` must be supplied with multivariate models and models with multi-category outcomes")
    }

    chk::chk_string(outcome)
    if (!outcome %in% unique_group) {
      .err(sprintf("only the following values of `outcome` are allowed: %s",
                   toString(add_quotes(unique_group))))
    }
  }
  else {
    if (is_not_null(outcome)) {
      .wrn("`outcome` is ignored for univariate models")
    }
    outcome <- NULL
  }

  fd <- is_not_null(x1)
  if (fd) {
    if (is_null(.attr(newdata, "set_preds"))) {
      .err("when `x1` is specified, `x` must be specified")
    }

    if (nrow(newdata) != 1L) {
      .err("when `x1` is specified, `x` must identify a single reference unit")
    }

    newdata_x1 <- process_x(x1, dat, "x1")

    if (nrow(newdata_x1) != 1L) {
      .err("`x1` must identify a single unit")
    }

    if (!setequal(.attr(newdata, "set_preds"), .attr(newdata_x1, "set_preds"))) {
      .err("when `x1` is specified, the same variables must be specified in `x` and `x1`")
    }

    if (all_apply(names(newdata), function(i) identical(newdata[[i]], newdata_x1[[i]]))) {
      .err("`x` and `x1` must be different")
    }

    newdata <- rbind(newdata, newdata_x1)
  }

  set_preds <- .attr(newdata, "set_preds")
  if (is_not_null(set_preds)) {
    set_pred_lengths <- vapply(newdata[set_preds], function(i) length(unique(i)), integer(1L))

    if (nrow(newdata) > 1L) {
      rownames(newdata) <- do.call("paste", c(lapply(set_preds[set_pred_lengths > 1L], function(i) {
        paste0(i, " = ", add_quotes(newdata[[i]], chk::vld_character_or_factor(newdata[[i]])))
      }), list(sep = ", ")))
    }
  }
  else {
    rownames(newdata) <- "Typical"
  }

  attr(newdata, "set_preds") <- set_preds

  #make FUN for sim_apply()
  FUN <- {
    if (fd)
      function(fit) {
        p <- clarify_predict(fit, newdata = newdata, group = outcome, type = type, ...) |>
          .get_p() |>
          setNames(rownames(newdata))

        c(p, "FD" = unname(diff(p)))
      }
    else
      function(fit) {
        clarify_predict(fit, newdata = newdata, group = outcome, type = type, ...) |>
          .get_p() |>
          setNames(rownames(newdata))
      }
  }

  out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

  attr(out, "setx") <- newdata
  attr(out, "fd") <- fd
  class(out) <- c("clarify_setx", class(out))

  out
}

#' @exportS3Method print clarify_setx
#' @rdname sim_setx
print.clarify_setx <- function(x, digits = 4L, max.ests = 6L, ...) {
  chk::chk_whole_number(digits)
  chk::chk_count(max.ests)

  n.ests <- length(coef(x))
  max.ests <- min(max.ests, n.ests)

  cat("A `clarify_est` object (from `sim_setx()`)\n")
  if (isTRUE(.attr(x, "fd"))) {
    cat(" - First difference\n")
  }
  else {
    cat(" - Predicted outcomes at specified values\n")
  }

  set_preds <- .attr(.attr(x, "setx"), "set_preds")
  if (is_not_null(set_preds)) {
    cat(sprintf("   + Predictors set: %s\n",
                toString(set_preds)))
    if (length(set_preds) != nrow(.attr(x, "setx"))) {
      cat('   + All others set at typical values (see `help("sim_setx")` for definition)\n')
    }
  }
  else {
    cat('   + All predictors set at typical values (see `help("sim_setx")` for definition)')
  }

  cat(sprintf(" - %s simulated values\n", nrow(x)))
  cat(sprintf(" - %s %s estimated:", n.ests,
              ngettext(n.ests, "quantity", "quantities")))

  data.frame(names(coef(x)),
             coef(x),
             fix.empty.names = FALSE) |>
    .print_estimate_table(digits = digits,
                          topn = floor(max.ests / 2))

  invisible(x)
}

process_x <- function(x, dat, arg_name) {

  if (is_null(x)) {
    set_preds <- NULL
    auto_preds <- names(dat)
  }
  else if (is.data.frame(x)) {
    set_preds <- intersect(names(x), names(dat))
    auto_preds <- setdiff(names(dat), set_preds)

    if (is_null(set_preds)) {
      .wrn(sprintf("the data data.frame supplied to %s does not contain any variables that correspond to variables used in the original model", arg_name))
    }
  }
  else if (!is.list(x) || is_null(names(x)) || !all(nzchar(names(x))) ||
           !chk::vld_all(x, is.atomic)) {
    .err(sprintf("the argument to `%s` must be a grid data.frame or a named list of values for variables to be set to",
                 arg_name))
  }
  else if (all(names(x) %in% names(dat))) {
    set_preds <- names(x)[lengths(x) > 0L]
    auto_preds <- setdiff(names(dat), set_preds)
  }
  else {
    vars_not_in_model <- setdiff(names(x), names(dat))
    .err(sprintf("the variable%%s %s named in `%s` %%r not present in the original model",
                 word_list(vars_not_in_model, quotes = TRUE),
                 arg_name),
         n = length(vars_not_in_model))
  }

  #Check to make sure inputs are the right type
  check_classes(dat, x)

  if (!is.data.frame(x)) {
    x <- do.call("expand.grid", c(x, list(KEEP.OUT.ATTRS = FALSE,
                                          stringsAsFactors = FALSE)))
  }

  if (is_not_null(auto_preds)) {
    x_auto <- setNames(lapply(dat[auto_preds], function(p) {
      if (is.numeric(p)) {
        if (length(unique(p)) == 2L) Mode(p)
        else mean(p, na.rm = TRUE)
      }
      else if (is.ordered(p)) {
        median(p, na.rm = TRUE)
      }
      else {
        Mode(p)
      }
    }), auto_preds)

    x <- do.call("cbind", c(list(x), x_auto))
  }

  x <- x[names(dat)]

  attr(x, "set_preds") <- set_preds
  attr(x, "auto_preds") <- auto_preds

  x
}
