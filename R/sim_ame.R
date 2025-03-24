#' Compute average marginal effects
#'
#' @description
#' `sim_ame()` is a wrapper for [sim_apply()] that computes average
#' marginal effects, the average effect of changing a single variable from one
#' value to another (i.e., from one category to another for categorical
#' variables or a tiny change for continuous variables).
#'
#' @inheritParams sim_apply
#' @param var either the names of the variables for which marginal effects are
#'   to be computed or a named list containing the values the variables should
#'   take. See Details.
#' @param subset optional; a vector used to subset the data used to compute the
#'   marginal effects. This will be evaluated within the original dataset used
#'   to fit the model using [subset()], so nonstandard evaluation is allowed.
#' @param by a one-sided formula or character vector containing the names of
#'   variables for which to stratify the estimates. Each quantity will be
#'   computed within each level of the complete cross of the variables specified
#'   in `by`.
#' @param contrast a string containing the name of a contrast between the
#'   average marginal means when the variable named in `var` is categorical and
#'   takes on two values. Allowed options include `"diff"` for the difference in
#'   means (also `"rd"`), `"rr"` for the risk ratio (also `"irr"`), `"log(rr):`
#'   for the log risk ratio (also `"log(irr)"`), `"sr"` for the survival ratio,
#'   `"log(sr):` for the log survival ratio, `"srr"` for the switch relative
#'   risk (also `"grrr"`), `"or"` for the odds ratio, `"log(or)"` for the log
#'   odds ratio, and `"nnt"` for the number needed to treat. These options are
#'   not case sensitive, but the parentheses must be included if present.
#' @param outcome a string containing the name of the outcome or outcome level
#'   for multivariate (multiple outcomes) or multi-category outcomes. Ignored
#'   for univariate (single outcome) and binary outcomes.
#' @param type a string containing the type of predicted values (e.g., the link
#'   or the response). Passed to [marginaleffects::get_predict()] and eventually
#'   to `predict()` in most cases. The default and allowable option depend on
#'   the type of model supplied, but almost always corresponds to the response
#'   scale (e.g., predicted probabilities for binomial models).
#' @param eps when the variable named in `var` is continuous, the value by which
#'   to change the variable values to approximate the derivative. See Details.
#' @param \dots for `sim_ame()`, additional arguments passed to [marginaleffects::get_predict()] (and eventually to `predict()`) to compute predictions. For `print()`, ignored.
#'
#' @details
#' `sim_ame()` computes average adjusted predictions or average marginal effects depending on which variables are named in `var` and how they are specified. Canonically, `var` should be specified as a named list with the value(s) each variable should be set to. For example, specifying `var = list(x1 = 0:1)` computes average adjusted predictions setting `x1` to 0 and 1. Specifying a variable's values as `NULL`, e.g., `list(x1 = NULL)`, is equivalent to requesting average adjusted predictions at each unique value of the variable when that variable is binary or a factor or character and requests the average marginal effect of that variable otherwise. Specifying an unnamed entry in the list with a string containing the value of that variable, e.g., `list("x1")` is equivalent to specifying `list(x1 = NULL)`. Similarly, supplying a vector with the names of the variables is equivalent to specifying a list, e.g., `var = "x1"` is equivalent to `var = list(x1 = NULL)`.
#'
#' Multiple variables can be supplied to `var` at the same time to set the corresponding variables to those values. If all values are specified directly or the variables are categorical, e.g., `list(x1 = 0:1, x2 = c(5, 10))`, this computes average adjusted predictions at each combination of the supplied variables. If any one variable's values is specified as `NULL` and the variable is continuous, the average marginal effect of that variable will be computed with the other variables set to their corresponding combinations. For example, if `x2` is a continuous variable, specifying `var = list(x1 = 0:1, x2 = NULL)` requests the average marginal effect of `x2` computed first setting `x1` to 0 and then setting `x1` to 1. The average marginal effect can only be computed for one variable at a time.
#'
#' Below are some examples of specifications and what they request, assuming `x1` is a binary variable taking on values of 0 and 1 and `x2` is a continuous variable:
#'
#' * `list(x1 = 0:1)`, `list(x1 = NULL)`, `list("x1")`, `"x1"` -- the average adjusted predictions setting `x1` to 0 and to 1
#' * `list(x2 = NULL)`, `list("x2")`, `"x2"` -- the average marginal effect of `x2`
#' * `list(x2 = c(5, 10))` -- the average adjusted predictions setting `x2` to 5 and to 10
#' * `list(x1 = 0:1, x2 = c(5, 10))`, `list("x1", x2 = c(5, 10))` -- the average adjusted predictions setting `x1` and `x2` in a full cross of 0, 1 and 5, 10, respectively (e.g., (0, 5), (0, 10), (1, 5), and (1, 10))
#' * `list(x1 = 0:1, "x2")`, `list("x1", "x2")`, `c("x1", "x2")` -- the average marginal effects of `x2` setting `x1` to 0 and to 1
#'
#' The average adjusted prediction is the average predicted outcome
#' value after setting all units' value of a variable to a specified level. (This quantity
#' has several names, including the average potential outcome, average marginal mean, and standardized mean). When exactly two average adjusted predictions are requested, a contrast
#' between them can be requested by supplying an argument
#' to `contrast` (see Effect Measures section below). Contrasts can be manually computed using [transform()]
#' afterward as well; this is required when multiple average adjusted predictions are requested (i.e., because a single variable was supplied to `var` with more than two levels or a combination of multiple variables was supplied).
#'
#' A marginal effect is the instantaneous rate of change
#' corresponding to changing a unit's observed value of a variable by a tiny amount
#' and considering to what degree the predicted outcome changes. The ratio of
#' the change in the predicted outcome to the change in the value of the variable is
#' the marginal effect; these are averaged across the sample to arrive at an
#' average marginal effect. The "tiny amount" used is `eps` times the standard
#' deviation of the focal variable.
#'
#' The difference between using `by` or `subset` vs. `var` is that `by` and `subset` subset the data when computing the requested quantity, whereas `var` sets the corresponding variable to given a value for all units. For example, using `by = ~v` computes the quantity of interest separately for each subset of the data defined by `v`, whereas setting `var = list(., "v")` computes the quantity of interest for all units setting their value of `v` to its unique values. The resulting quantities have different interpretations. Both `by` and `var` can be used simultaneously.
#'
#' ## Effect measures
#'
#' The effect measures specified in `contrast` are defined below. Typically only
#' `"diff"` is appropriate for continuous outcomes and `"diff"` or `"irr"` are
#' appropriate for count outcomes; the rest are appropriate for binary outcomes.
#' For a focal variable with two levels, `0` and `1`, and an outcome `Y`, the
#' average marginal means will be denoted in the below formulas as `E[Y(0)]` and
#' `E[Y(1)]`, respectively.
#'
#' | `contrast`      | **Description**                      | **Formula**                                     |
#' | --------------- | -------------------------------- | ------------------------------------------- |
#' | `"diff"`/`"rd"` | Mean/risk difference             | `E[Y(1)] - E[Y(0)]`                         |
#' | `"rr"`/`"irr"`  | Risk ratio/incidence rate ratio  | `E[Y(1)] / E[Y(0)]`                         |
#' | `"sr"`          | Survival ratio                   | `(1 - E[Y(1)]) / (1 - E[Y(0)])`             |
#' | `"srr"`/`"grrr"`| Switch risk ratio                | `1 - sr` if `E[Y(1)] > E[Y(0)]`             |
#' |                 |                                  | `rr - 1` if `E[Y(1)] < E[Y(0)]`             |
#' |                 |                                  | `0` otherwise                               |
#' | `"or"`          | Odds ratio                       | `O[Y(1)] / O[Y(0)]`                         |
#' |                 |                                  | where `O[Y(.)]` = `E[Y(.)] / (1 - E[Y(.)])` |
#' | `"nnt"`         | Number needed to treat           | `1 / rd`                                    |
#'
#' The `log(.)` versions are defined by taking the [log()] (natural log) of the
#' corresponding effect measure.
#'
#' @return A `clarify_ame` object, which inherits from `clarify_est` and is
#' similar to the output of `sim_apply()`, with the additional attributes
#' `"var"` containing the variable values specified in `var` and `"by"` containing the
#' names of the variables specified in `by` (if any). The average adjusted
#' predictions will be named `E[Y({v})]`, where `{v}` is replaced with the
#' values the variables named in `var` take on. The average marginal effect for a
#' continuous `var` will be named `E[dY/d({x})]` where `{x}` is replaced with
#' `var`. When `by` is specified, the average adjusted predictions will be named
#' `E[Y({v})|{b}]` and the average marginal effect `E[dY/d({x})|{b}]` where
#' `{b}` is a comma-separated list of of values of the `by` variables at which
#' the quantity is computed. See examples.
#'
#' @seealso [sim_apply()], which provides a general interface to computing any
#'   quantities for simulation-based inference; [plot.clarify_est()] for plotting the
#'   output of a call to `sim_ame()`; [summary.clarify_est()] for computing
#'   p-values and confidence intervals for the estimated quantities.
#'
#'  [marginaleffects::avg_predictions()], [marginaleffects::avg_comparisons()] and [marginaleffects::avg_slopes()] for delta method-based implementations of computing average marginal effects.
#'
#' @examples
#' data("lalonde", package = "MatchIt")
#'
#' # Fit the model
#' fit <- glm(I(re78 > 0) ~ treat + age + race +
#'              married + re74,
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
#' # Contrast between average adjusted predictions
#' # for `treat`
#' est <- sim_ame(s, var = "treat", contrast = "rr",
#'                verbose = FALSE)
#' summary(est)
#'
#' # Average adjusted predictions for `race`; need to follow up
#' # with contrasts for specific levels
#' est <- sim_ame(s, var = "race", verbose = FALSE)
#'
#' est <- transform(est,
#'                  `RR(h,b)` = `E[Y(hispan)]` / `E[Y(black)]`)
#'
#' summary(est)
#'
#' # Average adjusted predictions for `treat` within levels of
#' # `married`, first using `subset` and then using `by`
#' est0 <- sim_ame(s, var = "treat", subset = married == 0,
#'                 contrast = "rd", verbose = FALSE)
#' names(est0) <- paste0(names(est0), "|married=0")
#' est1 <- sim_ame(s, var = "treat", subset = married == 1,
#'                 contrast = "rd", verbose = FALSE)
#' names(est1) <- paste0(names(est1), "|married=1")
#'
#' summary(cbind(est0, est1))
#'
#' est <- sim_ame(s, var = "treat", by = ~married,
#'                contrast = "rd", verbose = FALSE)
#'
#' est
#' summary(est)
#'
#' # Average marginal effect of `age` within levels of
#' # married*race
#' est <- sim_ame(s, var = "age", by = ~married + race,
#'                verbose = FALSE)
#' est
#' summary(est, null = 0)
#'
#' # Comparing AMEs between married and unmarried for
#' # each level of `race`
#' est_diff <- est[4:6] - est[1:3]
#' names(est_diff) <- paste0("AME_diff|", levels(lalonde$race))
#' summary(est_diff)
#'
#' # Average adjusted predictions at a combination of `treat`
#' # and `married`
#' est <- sim_ame(s, var = c("treat", "married"),
#'                verbose = FALSE)
#' est
#'
#' # Average marginal effect of `age` setting `married` to 1
#' est <- sim_ame(s, var = list("age", married = 1),
#'                verbose = FALSE)

#' @export
sim_ame <- function(sim,
                    var,
                    subset = NULL,
                    by = NULL,
                    contrast = NULL,
                    outcome = NULL,
                    type = NULL,
                    eps = 1e-5,
                    verbose = TRUE,
                    cl = NULL,
                    ...) {

  check_sim_apply_wrapper_ready(sim)

  chk::chk_flag(verbose)
  is_misim <- inherits(sim, "clarify_misim")

  vals <- NULL

  if (missing(var)) {
    .err("`var` must be supplied, identifying the focal variable")
  }

  if (is.character(var)) {
    var <- as.list(var)
  }

  if (!is.list(var)) {
    .err("`var` must be the name of the desired focal variable(s) or a named list with their values")
  }

  vals <- var
  if (is_null(names(vals))) {
    names(vals) <- character(length(vals))
  }

  vals[!nzchar(names(vals)) & lengths(vals) == 0L] <- NULL

  if (is_null(vals)) {
    .err("`var` must be the name of the desired focal variable or a named list with its values")
  }

  empty_names <- is.na(names(vals)) | !nzchar(names(vals))
  if (any(empty_names)) {
    for (v in vals[empty_names]) {
      if (!chk::vld_string(v)) {
        .err("`var` must be the name of the desired focal variable or a named list with its values")
      }
    }

    names(vals)[empty_names] <- unlist(vals[empty_names])
    vals[empty_names] <- vector("list", sum(empty_names))
  }

  vars <- names(vals)

  dat <- {
    if (is_misim)
      do.call("rbind", lapply(sim$fit, insight::get_predictors, verbose = FALSE))
    else
      insight::get_predictors(sim$fit, verbose = FALSE)
  }

  if (!all(vars %in% names(dat))) {
    .err(sprintf("the variable%%s %s named in `var` %%r not present in the original model",
                 word_list(vars[!vars %in% names(dat)], quotes = TRUE)),
         n = sum(!vars %in% names(dat)))
  }

  var_val <- dat[vars]
  rm(dat)

  var_types <- vapply(vars, function(v) {
    if (is_not_null(vals[[v]]) || chk::vld_character_or_factor(var_val[[v]]) ||
        is.logical(var_val[[v]]) || length(unique(var_val[[v]])) <= 2) "cat"
    else "cont"
  }, character(1L))

  for (v in vars[var_types == "cat"]) {
    if (is_null(vals[[v]])) {
      vals[[v]] <- {
        if (is.factor(var_val[[v]])) levels(var_val[[v]])
        else sort(unique(var_val[[v]]))
      }
    }
    else if (chk::vld_character_or_factor(var_val[[v]]) && !all(vals[[v]] %in% var_val[[v]])) {
      .err(sprintf("the values mentioned in `var[[%s]]` must be values `%s` takes on",
                   add_quotes(v), v))
    }
  }

  if (is_not_null(by)) {
    if (is.character(by)) {
      by <- reformulate(by)
    }
    else if (!inherits(by, "formula")) {
      .err("`by` must be a one-sided formula or character vector")
    }
  }

  if (sum(var_types == "cont") > 1) {
    .err("only one continuous variable can be supplied to `var`")
  }

  index.sub <- substitute(subset)
  sim$fit <- .attach_pred_data_to_fit(sim$fit, by = by, index.sub = index.sub,
                                      is_fitlist = is_misim)

  #Test to make sure compatible
  if (is_misim) {
    test_dat <- .get_pred_data_from_fit(sim$fit[[1L]])
    test_predict <- clarify_predict(sim$fit[[1L]], newdata = test_dat, group = NULL, type = type, ...)
  }
  else {
    test_dat <- .get_pred_data_from_fit(sim$fit)
    test_predict <- clarify_predict(sim$fit, newdata = test_dat, group = NULL, type = type, ...)
  }

  if (hasName(test_predict, "group") && length(unique_group <- unique(test_predict$group)) > 1L) {
    if (is_null(outcome)) {
      .err("`outcome` must be supplied with multivariate models and models with multi-category outcomes")
    }

    chk::chk_string(outcome)

    if (!outcome %in% unique_group) {
      .err("only the following values of `outcome` are allowed: ",
           toString(add_quotes(unique_group)))
    }

    test_predict <- .subset_group(test_predict, outcome)
  }
  else {
    if (is_not_null(outcome)) {
      .wrn("`outcome` is ignored for univariate models")
    }

    outcome <- NULL
  }

  if (nrow(test_predict) != nrow(test_dat)) {
    .err("not all units received a predicted value, suggesting a bug")
  }

  if (all(var_types == "cat")) {
    vars_grid <- do.call("expand.grid", vals)

    if (nrow(vars_grid) < 2) {
      contrast <- NULL
    }
    else if (nrow(vars_grid) == 2) {
      if (is_not_null(contrast)) {
        chk::chk_string(contrast)
        contrast <- tolower(contrast)
        contrast <- match_arg(contrast, c("diff", "rd", "irr", "rr", "sr", "srr", "grrr", "log(irr)",
                                          "log(rr)", "or", "log(or)", "nnt"))
      }
    }
    else if (is_not_null(contrast)) {
      if (sum(lengths(vals) >= 2) > 1)
        .wrn("`contrast` is ignored when multiple focal variables takes on two or more levels")
      else
        .wrn("`contrast` is ignored when any focal variable takes on more than two levels")

      contrast <- NULL
    }

    if (is_null(by)) {
      FUN <- function(fit) {
        dat <- .get_pred_data_from_fit(fit)
        m <- nrow(dat)

        # Extend dataset for combination of vars_grid
        dat <- dat[rep.int(seq_len(m), nrow(vars_grid)),, drop = FALSE]

        for (v in vars) {
          dat[[v]][] <- rep(vars_grid[[v]], each = m)
        }

        in_v <- lapply(seq_len(nrow(vars_grid)), function(i) {
          (i - 1) * m + seq_len(m)
        })

        p <- .get_p(clarify_predict(fit, newdata = dat, group = outcome, type = type, ...))

        vapply(in_v, function(in_v_i) {
          mean(p[in_v_i])
        }, numeric(1L))
      }

      out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

      if (is_not_null(contrast)) {
        out <- transform(out,
                         `.C` = switch(tolower(contrast),
                                       "diff" = , "rd" = .b2 - .b1,
                                       "nnt" = 1 / (.b2 - .b1),
                                       "irr" = , "rr" = .b2 / .b1,
                                       "log(irr)" = , "log(rr)" = log(.b2 / .b1),
                                       "sr" = (1 - .b2) / (1 - .b1),
                                       "log(sr)" = log((1 - .b2) / (1 - .b1)),
                                       "grrr" =, "srr" = (.b2 == .b1) * 0 +
                                         (.b2 > .b1) * (1 - (1 - .b2) / (1 - .b1)) + (.b2 < .b1) * (.b2 / .b1 - 1),
                                       "or" = (.b2 / (1 - .b2)) / (.b1 / (1 - .b1)),
                                       "log(or)" = log((.b2 / (1 - .b2)) / (.b1 / (1 - .b1)))))
        names(out)[3L] <- .rename_contrast(contrast)
      }

      names(out)[seq_len(nrow(vars_grid))] <- apply(as.matrix(vars_grid), 1L, function(g) {
        sprintf("E[Y(%s)]", paste(g, collapse = ","))
      })
    }
    else {
      FUN <- function(fit) {
        dat <- .get_pred_data_from_fit(fit)
        by_var <- .get_by_from_fit(fit)
        m <- nrow(dat)

        # Extend dataset for combination of vars_grid
        dat2 <- dat[rep.int(seq_len(m), nrow(vars_grid)),, drop = FALSE]

        for (v in vars) {
          dat2[[v]][] <- rep(vars_grid[[v]], each = m)
        }

        p <- .get_p(clarify_predict(fit, newdata = dat2, group = outcome, type = type, ...))

        in_v <- lapply(seq_len(nrow(vars_grid)), function(i) {
          (i - 1) * m + seq_len(m)
        })

        unlist(lapply(levels(by_var), function(b) {
          in_b <- which(by_var == b)
          vapply(in_v, function(in_v_i) {
            mean(p[in_v_i][in_b])
          }, numeric(1L))
        }))
      }

      out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

      by_levels <- levels(.get_by_from_fit(sim$fit))

      for (i in seq_along(by_levels)) {
        if (is_not_null(contrast)) {
          out_i <- out[(i - 1L) * nrow(vars_grid) + seq_len(nrow(vars_grid))]
          out_i <- transform(out_i,
                             `.C` = switch(tolower(contrast),
                                           "diff" = , "rd" = .b2 - .b1,
                                           "nnt" = 1 / (.b2 - .b1),
                                           "irr" = , "rr" = .b2 / .b1,
                                           "log(irr)" = , "log(rr)" = log(.b2 / .b1),
                                           "sr" = (1 - .b2) / (1 - .b1),
                                           "log(sr)" = log((1 - .b2) / (1 - .b1)),
                                           "grrr" =, "srr" = (.b2 > .b1) * (1 - (1 - .b2) / (1 - .b1)) +
                                             (.b2 < .b1) * (.b2 / .b1 - 1),
                                           "or" = (.b2 / (1 - .b2)) / (.b1 / (1 - .b1)),
                                           "log(or)" = log((.b2 / (1 - .b2)) / (.b1 / (1 - .b1)))))
          names(out_i)[3L] <- sprintf("%s[%s]", .rename_contrast(contrast), by_levels[i])
          out <- cbind(out, out_i[3L])
        }

        ind <- (i - 1L) * nrow(vars_grid) + seq_len(nrow(vars_grid))
        names(out)[ind] <- apply(as.matrix(vars_grid), 1L, function(g) {
          sprintf("E[Y(%s)|%s]", paste(g, collapse = ","), by_levels[i])
        })
      }

      if (is_not_null(contrast)) {
        #Re-order contrasts to be with by-levels
        out <- out[unlist(lapply(seq_along(by_levels), function(i) {
          c((i - 1L) * nrow(vars_grid) + seq_len(nrow(vars_grid)),
            length(by_levels) * nrow(vars_grid) + i)
        }))]
      }

      attr(out, "by") <- attr(sim$fit, "by_name")
    }
  }
  else {
    chk::chk_number(eps)
    chk::chk_gt(eps)

    if (is_not_null(contrast)) {
      .wrn("`contrast` is ignored when the focal variable is continuous")
      contrast <- NULL
    }

    cv <- which(var_types == "cont")

    vars_grid <- do.call("expand.grid", vals[-cv])

    eps <- eps * sd(var_val[[cv]])

    if (is_null(by)) {
      if (nrow(vars_grid) == 0L) {
        FUN <- function(fit) {
          dat <- .get_pred_data_from_fit(fit)
          ind <- seq_len(nrow(dat))

          # Double dataset for numeric derivative
          dat <- dat[rep.int(ind, 2L), , drop = FALSE]
          dat[[vars[cv]]][ind] <- dat[[vars[cv]]][ind] - eps / 2
          dat[[vars[cv]]][-ind] <- dat[[vars[cv]]][-ind] + eps / 2

          p <- .get_p(clarify_predict(fit, newdata = dat, group = outcome, type = type, ...))

          m0 <- mean(p[ind])
          m1 <- mean(p[-ind])

          (m1 - m0) / eps
        }

        out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

        names(out) <- sprintf("E[dY/d(%s)]", var)
      }
      else {
        FUN <- function(fit) {
          dat <- .get_pred_data_from_fit(fit)
          m <- nrow(dat)

          # Extend dataset for combination of vars_grid
          dat <- dat[rep.int(seq_len(m), nrow(vars_grid)),, drop = FALSE]
          ind2 <- seq_len(nrow(dat))

          for (v in vars[-cv]) {
            dat[[v]][] <- rep(vars_grid[[v]], each = m)
          }

          in_v <- lapply(seq_len(nrow(vars_grid)), function(i) {
            (i - 1L) * m + seq_len(m)
          })

          # Double dataset for numeric derivative
          dat <- dat[rep.int(ind2, 2L), , drop = FALSE]

          dat[[vars[cv]]][ind2] <- dat[[vars[cv]]][ind2] - eps / 2
          dat[[vars[cv]]][-ind2] <- dat[[vars[cv]]][-ind2] + eps / 2

          p <- .get_p(clarify_predict(fit, newdata = dat, group = outcome, type = type, ...))

          vapply(in_v, function(in_v_i) {
            m0 <- mean(p[ind2][in_v_i])
            m1 <- mean(p[-ind2][in_v_i])
            (m1 - m0) / eps
          }, numeric(1L))
        }

        out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

        names(out) <- apply(as.matrix(vars_grid), 1L, function(g) {
          sprintf("E[dY(%s)/d(%s)]", paste(g, collapse = ","), vars[cv])
        })
      }
    }
    else {
      if (nrow(vars_grid) == 0L) {
        FUN <- function(fit) {
          dat <- .get_pred_data_from_fit(fit)
          by_var <- .get_by_from_fit(fit)
          ind <- seq_len(nrow(dat))

          # Double dataset for numeric derivative
          dat <- dat[rep.int(ind, 2L), , drop = FALSE]
          dat[[vars[cv]]][ind] <- dat[[vars[cv]]][ind] - eps / 2
          dat[[vars[cv]]][-ind] <- dat[[vars[cv]]][-ind] + eps / 2

          p <- .get_p(clarify_predict(fit, newdata = dat, group = outcome, type = type, ...))

          vapply(levels(by_var), function(b) {
            in_b <- which(by_var == b)

            m0 <- mean(p[ind][in_b])
            m1 <- mean(p[-ind][in_b])
            (m1 - m0) / eps
          }, numeric(1L))

        }

        out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

        by_levels <- levels(.get_by_from_fit(sim$fit))

        names(out) <- sprintf("E[dY/d(%s)|%s]", var, by_levels)

      }
      else {
        FUN <- function(fit) {
          dat <- .get_pred_data_from_fit(fit)
          by_var <- .get_by_from_fit(fit)
          m <- nrow(dat)

          # Extend dataset for combination of vars_grid
          dat <- dat[rep.int(seq_len(m), nrow(vars_grid)),, drop = FALSE]
          ind2 <- seq_len(nrow(dat))

          for (v in vars[-cv]) {
            dat[[v]][] <- rep(vars_grid[[v]], each = m)
          }

          in_v <- lapply(seq_len(nrow(vars_grid)), function(i) {
            (i - 1L) * m + seq_len(m)
          })

          # Double dataset for numeric derivative
          dat <- dat[rep.int(ind2, 2L), , drop = FALSE]

          dat[[vars[cv]]][ind2] <- dat[[vars[cv]]][ind2] - eps / 2
          dat[[vars[cv]]][-ind2] <- dat[[vars[cv]]][-ind2] + eps / 2

          p <- .get_p(clarify_predict(fit, newdata = dat, group = outcome, type = type, ...))

          unlist(lapply(levels(by_var), function(b) {
            in_b <- which(by_var == b)
            vapply(in_v, function(in_v_i) {
              m0 <- mean(p[ind2][in_v_i][in_b])
              m1 <- mean(p[-ind2][in_v_i][in_b])
              (m1 - m0) / eps
            }, numeric(1L))
          }))
        }

        out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

        by_levels <- levels(.get_by_from_fit(sim$fit))

        for (i in seq_along(by_levels)) {
          names(out)[(i - 1L) * nrow(vars_grid) + seq_len(nrow(vars_grid))] <- apply(as.matrix(vars_grid), 1L, function(g) {
            sprintf("E[dY(%s)/d(%s)|%s]", paste(g, collapse = ","), vars[cv], by_levels[i])
          })
        }
      }

      attr(out, "by") <- attr(sim$fit, "by_name")
    }
  }

  attr(out, "var") <- vals
  class(out) <- c("clarify_ame", class(out))
  out
}

#' @exportS3Method print clarify_ame
#' @rdname sim_ame
#' @param x a `clarify_ame` object.
#' @param digits the minimum number of significant digits to be used; passed to [print.data.frame()].
#' @param max.ests the maximum number of estimates to display.
print.clarify_ame <- function(x, digits = NULL, max.ests = 6L, ...) {
  chk::chk_count(max.ests)
  max.ests <- min(max.ests, length(coef(x)))

  cat("A `clarify_est` object (from `sim_ame()`)\n")

  vals <- attr(x, "var", TRUE)
  cont_vals <- vals[lengths(vals) == 0L]

  if (is_not_null(cont_vals)) {
    set_vals <- vals[lengths(vals) > 0L]
    cat(sprintf(" - Average marginal effect of %s\n", word_list(names(cont_vals), quotes = "`")))
    if (is_not_null(set_vals)) {
      set_text <- vapply(names(set_vals), function(i) {
        sprintf("`%s` set to %s", i,
                word_list(set_vals[[i]], quotes = chk::vld_character_or_factor(set_vals[[i]])))
      }, character(1L))
      cat(sprintf("   - with %s\n", word_list(set_text)))
    }
  }
  else {
    set_vals <- vals[lengths(vals) == 1L]
    varying_vals <- vals[lengths(vals) > 1L]
    if (is_null(varying_vals)) {
      cat(" - Average adjusted predictions\n")
      set_text <- vapply(names(set_vals), function(i) {
        sprintf("`%s` set to %s", i,
                word_list(set_vals[[i]], quotes = chk::vld_character_or_factor(set_vals[[i]])))
      }, character(1L))
      cat(sprintf("   - with %s\n", word_list(set_text)))
    }
    else {
      cat(sprintf(" - Average adjusted predictions for %s\n",
                  word_list(names(varying_vals), quotes = "`")))
      if (is_not_null(set_vals)) {
        set_text <- vapply(names(set_vals), function(i) {
          sprintf("`%s` set to %s", i,
                  word_list(set_vals[[i]], quotes = chk::vld_character_or_factor(set_vals[[i]])))
        }, character(1L))
        cat(sprintf("   - with %s\n", word_list(set_text)))
      }
    }
  }

  if (is_not_null(attr(x, "by"))) {
    cat(sprintf("   - within levels of %s\n",
                word_list(attr(x, "by"), quotes = "`")))
  }

  cat(sprintf(" - %s simulated values\n", nrow(x)))

  cat(sprintf(" - %s %s estimated:", length(coef(x)),
              ngettext(length(coef(x)), "quantity", "quantities")))

  print.data.frame(data.frame(names(coef(x))[seq_len(max.ests)],
                              coef(x)[seq_len(max.ests)],
                              fix.empty.names	= FALSE),
                   row.names = FALSE, right = FALSE, digits = digits)

  if (max.ests != length(coef(x))) {
    cat(sprintf("# ... and %s more\n",
                length(coef(x)) - max.ests))
  }

  invisible(x)
}

.rename_contrast <- function(x) {
  if (is_null(x)) {
    return(character(0L))
  }

  vapply(tolower(x), switch, character(1L),
         "diff" = "Diff",
         "log(irr)" = "log(IRR)",
         "log(rr)" = "log(RR)",
         "log(or)" = "log(OR)",
         toupper(x))
}

.attach_pred_data_to_fit <- function(fit, by = NULL, index.sub = NULL, is_fitlist = FALSE) {
  if (is_fitlist) {
    fit <- lapply(fit, .attach_pred_data_to_fit, by = by, index.sub = index.sub)
    attr(fit, "is_fitlist") <- TRUE
    return(fit)
  }

  data <- insight::get_data(fit, verbose = FALSE)
  weights <- insight::get_weights(fit, null_as_ones = TRUE)
  vars <- insight::find_predictors(fit, effects = "fixed", component = "all",
                                   flatten = TRUE)
  if (is_not_null(index.sub)) {
    subset <- eval(index.sub, data, parent.frame(2L))

    if (!chk::vld_atomic(subset)) {
      .err("`subset` must evaluate to an atomic vector")
    }

    if (is.logical(subset) && length(subset) != nrow(data)) {
      .err("when `subset` is logical, it must have the same length as the original dataset")
    }

    if (is_not_null(subset)) {
      data <- data[subset, , drop = FALSE]
      weights <- weights[subset]
    }
  }

  attr(fit, "clarify_data") <- data[, intersect(vars, colnames(data)), drop = FALSE]
  attr(fit, "weights") <- weights

  if (is_not_null(by)) {
    by_mf <- model.frame(update(by, NULL ~ .), data = data)
    attr(fit, "by_var") <- factor(do.call("paste", c(as.list(by_mf), sep = ",")))
    attr(fit, "by_name") <- names(by_mf)
  }

  fit
}

.get_pred_data_from_fit <- function(fit) {
  attr(fit, "clarify_data", TRUE)
}

.get_by_from_fit <- function(fit) {
  if (isTRUE(attr(fit, "is_fitlist", TRUE))) {
    unlist(lapply(fit, .get_by_from_fit))
  }
  else {
    attr(fit, "by_var", TRUE)
  }
}
