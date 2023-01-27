#' Compute an average dose-response function
#'
#' @description
#' `sim_adrf()` is a wrapper for [sim_apply()] that computes average dose-response functions (ADRFs) and average marginal effect functions (AMEFs). An ADRF describes the relationship between values a focal variable can take and the expected value of the outcome were all units to be given each value of the variable. An AMEF describes the relationship between values a focal variable can take and the derivative of ADRF at each value.
#'
#' @inheritParams sim_apply
#' @param var the name of a variable for which the ADRF or AMEF is to be computed. This variable must be present in the model supplied to `sim()` and must be a numeric variable taking on more than two unique values.
#' @param subset optional; a vector used to subset the data used to compute the ADRF or AMEF. This will be evaluated within the original dataset used to fit the model using [subset()], so nonstandard evaluation is allowed.
#' @param contrast a string naming the type of quantity to be produced: `"adrf"` for the ADRF (the default) or `"amef"` for the AMEF.
#' @param at the levels of the variable named in `var` at which to evaluate the ADRF or AMEF. Should be a vector of numeric values corresponding to possible levels of `var`. If `NULL`, will be set to a range from slightly below the lowest observed value of `var` to slightly above the largest value.
#' @param n when `at = NULL`, the number of points to evaluate the ADRF or AMEF. Default is 21. Ignored when `at` is not `NULL`.
#' @param outcome a string containing the name of the outcome or outcome level for multivariate (multiple outcomes) or multi-category outcomes. Ignored for univariate (single outcome) and binary outcomes.
#' @param type a string containing the type of predicted values (e.g., the link or the response). Passed to [marginaleffects::get_predict()] and eventually to `predict()` in most cases. The default and allowable option depend on the type of model supplied, but almost always corresponds to the response scale (e.g., predicted probabilities for binomial models).
#' @param eps when `contrast = "amef"`, the value by which to shift the value of `var` to approximate the derivative. See Details.
#'
#' @details
#' The ADRF is composed of average marginal means across levels of the focal predictor. For each level of the focal predictor, predicted values of the outcome are computed after setting the value of the predictor to that level, and those values of the outcome are averaged across all units in the sample to arrive at an average marginal mean. Thus, the ADRF represent the relationship between the "dose" (i.e., the level of the focal predictor) and the average "response" (i.e., the outcome variable). It is the continuous analog to the average marginal effect computed for a binary predictor, e.g., using [sim_ame()]. Although inference can be at each level of the predictor or between two levels of the predictor, typically a plot of the ADRF is the most useful relevant quantity. These can be requested using [plot.clarify_adrf()].
#'
#' The AMEF is the derivative of the ADRF; if we call the derivative of the ADRF at each point a "treatment effect" (i.e., the rate at which the outcome changes corresponding to a small change in the predictor, or "treatment"), the AMEF is a function that relates the size of the treatment effect to the level of the treatment. The shape of the AMEF is usually of less importance than the value of the AMEF at each level of the predictor, which corresponds to the size of the treatment effect at the corresponding level. The AMEF is computed by computing the ADRF at each level of the focal predictor specified in `at`, shifting the predictor value by a tiny amount (control by `eps`), and computing the ratio of the change in the outcome to the shift, then averaging this value across all units. This quantity is related the the average marginal effect of a continuous predictor as computed by [`sim_ame()`], but rather than average these treatment effects across all observed levels of the treatment, the AMEF is a function evaluated at each possible level of the treatment. The "tiny amount" used is `eps` times the standard deviation of `var`.
#'
#' If unit-level weights are included in the model fit (and discoverable using [insight::get_weights()]), all means will be computed as weighted means.
#'
#' @return
#' A `clarify_adrf` object, which inherits from `clarify_est` and is similar to
#' the output of `sim_apply()`, with the additional attributes `"var"` containing
#' the variable named in `var`, `"at"` containing values at which the ADRF or AMEF is evaluated, and `"contrast"` containing the argument supplied to `contrast`. For an ADRF, the average marginal means will be named
#' `E[Y({v})]`, where `{v}` is replaced with the values in `at`. For an AMEF, the average marginal effects will be
#' named `dY/d({x})|{a}` where `{x}` is replaced with `var` and `{a}` is replaced by the values in `at`.
#'
#' @seealso [plot.clarify_adrf()] for plotting the ADRF or AMEF; [sim_ame()] for computing average marginal effects; [sim_apply()], which provides a general interface to computing any
#'   quantities for simulation-based inference; [summary.clarify_est()] for computing
#'   p-values and confidence intervals for the estimated quantities.
#'
#' `marginaleffects::marginaleffects()` and `marginaleffects::predictions()` for delta method-based implementations of computing average marginal effects and average marginal means.
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
#' # ADRF for `age`
#' est <- sim_adrf(s, var = "age",
#'                 at = seq(15, 55, length.out = 6),
#'                 verbose = FALSE)
#' est
#' plot(est)
#'
#' # AMEF for `age`
#' est <- sim_adrf(s, var = "age", contrast = "amef",
#'                at = seq(15, 55, length.out = 6),
#'                verbose = FALSE)
#' est
#' summary(est)
#' plot(est)
#' @export
sim_adrf <- function(sim,
                     var,
                     subset = NULL,
                     contrast = "adrf",
                     at = NULL,
                     n = 21,
                     outcome = NULL,
                     type = NULL,
                     eps = 1e-5,
                     verbose = TRUE,
                     cl = NULL) {

  check_sim_apply_wrapper_ready(sim)

  chk::chk_flag(verbose)
  is_misim <- inherits(sim, "clarify_misim")

  if (missing(var)) {
    .err("`var` must be supplied, identifying the focal variable")
  }
  if (!chk::vld_string(var)) {
    .err("`var` must be the name of the desired focal variable or a named list of length 1 with its values")
  }

  chk::chk_string(contrast)
  contrast <- tolower(contrast)
  contrast <- match_arg(contrast, c("adrf", "amef"))

  if (is_misim) {
    dat <- do.call("rbind", lapply(sim$fit, insight::get_predictors))
  }
  else {
    dat <- insight::get_predictors(sim$fit)
  }

  if (!var %in% names(dat)) {
    .err(sprintf("the variable \"%s\" named in `var` is not present in the original model",
                 var))
  }

  var_val <- dat[[var]]
  rm(dat)

  if (chk::vld_character_or_factor(var_val) ||
      is.logical(var_val) || length(unique(var_val)) <= 2) {
    .err("the variable named in `var` must be a numeric variable taking on more than two values. Use `sim_ame()` instead")
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

  min_var <- min(var_val)
  max_var <- max(var_val)
  if (is.null(at)) {
    chk::chk_count(n)
    # lims <- c(min_var - .01 * (max_var - min_var),
    #           max_var + .01 * (max_var - min_var))
    lims <- c(min_var, max_var)
    at <- seq(lims[1], lims[2], length.out = n)
  }
  else {
    chk::chk_numeric(at)
    if (min(at) > max_var || max(at) < min_var) {
      chk::wrn("the values supplied to `at` are outside the range of ", var, "; proceed with caution")
    }
    at <- sort(at)
  }

  if (contrast == "adrf") {

    FUN <- function(fit) {
      dat <- get_pred_data_from_fit(fit)
      vapply(at, function(x) {
        dat[[var]] <- x
        pred <- clarify_predict(fit, newdata = dat, group = outcome, type = type)
        weighted.mean(get_p(pred), attr(fit, "weights"))
      }, numeric(1L))
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)
    names(out) <- sprintf("E[Y(%s)]", at)
  }
  else if (contrast == "amef") {
    chk::chk_number(eps)
    chk::chk_gt(eps)
    eps <- eps * sd(var_val)

    FUN <- function(fit) {
      dat <- get_pred_data_from_fit(fit)
      ind <- seq_len(nrow(dat))
      dat2 <- rbind(dat, dat)
      weights <- attr(fit, "weights")
      vapply(at, function(x) {
        dat2[[var]][ind] <- x - eps / 2
        dat2[[var]][-ind] <- x + eps / 2
        pred <- clarify_predict(fit, newdata = dat2, group = outcome, type = type)
        p <- get_p(pred)
        m0 <- weighted.mean(p[ind], weights)
        m1 <- weighted.mean(p[-ind], weights)
        (m1 - m0) / eps
      }, numeric(1L))
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)
    names(out) <- sprintf("dY/d(%s)|%s", var, at)
  }

  attr(out, "var") <- var
  attr(out, "at") <- at
  attr(out, "contrast") <- contrast
  class(out) <- c("clarify_adrf", class(out))
  out
}

#' @exportS3Method print clarify_adrf
#' @rdname sim_adrf
#' @param x a `clarify_adrf` object.
#' @param digits the minimum number of significant digits to be used; passed to [print.data.frame()].
#' @param max.ests the maximum number of estimates to display.
print.clarify_adrf <- function(x, digits = NULL, max.ests = 6, ...) {
  chk::chk_count(max.ests)
  n.ests <- length(attr(x, "original"))
  max.ests <- min(max.ests, n.ests)

  cat("A `clarify_est` object (from `sim_adrf()`)\n")

  cat(sprintf(" - %s of `%s`\n", switch(attr(x, "contrast"), "adrf" = "Average does-response function",
                                        "amef" = "Average marginal effect function"),
              attr(x, "var")))
  cat(sprintf(" - %s simulated values\n", nrow(x)))
  cat(sprintf(" - %s %s estimated:", n.ests,
              ngettext(n.ests, "quantity", "quantities")))

  if (max.ests == n.ests) {
    print.data.frame(data.frame(names(attr(x, "original")),
                                attr(x, "original"),
                                fix.empty.names	= FALSE),
                     row.names = FALSE, right = FALSE, digits = digits)
  }
  else {
    print.data.frame(data.frame(names(attr(x, "original"))[seq_len(floor(max.ests / 2))],
                                attr(x, "original")[seq_len(floor(max.ests / 2))],
                                fix.empty.names	= FALSE),
                     row.names = FALSE, right = FALSE, digits = digits)
    cat(sprintf("# ... and %s more", n.ests - floor(max.ests / 2) - ceiling(max.ests / 2)))
    print.data.frame(data.frame(names(attr(x, "original"))[seq(n.ests - ceiling(max.ests / 2), n.ests)],
                                attr(x, "original")[seq(n.ests - ceiling(max.ests / 2), n.ests)],
                                fix.empty.names	= FALSE),
                     row.names = FALSE, right = FALSE, digits = digits)
  }

  invisible(x)
}
