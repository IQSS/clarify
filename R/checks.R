process_FUN <- function(FUN, use_fit = TRUE) {
  chk::chk_function(FUN)
  FUN_arg_names <- names(formals(FUN))

  if (is_null(FUN_arg_names)) {
    .err("`FUN` must accept one or more arguments")
  }

  attr(FUN, "use_coefs") <- any(FUN_arg_names == "coefs")

  if (!use_fit && !attr(FUN, "use_coefs", TRUE)) {
    .err("`FUN` must accept a `coefs` argument. See help(\"sim_apply\") for details")
  }

  attr(FUN, "use_fit") <- any(FUN_arg_names == "fit")

  if (!use_fit && attr(FUN, "use_fit", TRUE)) {
    .wrn("the `fit` argument to `FUN` will be ignored. See help(\"sim_apply\") for details")
    attr(FUN, "use_fit") <- FALSE
  }

  FUN
}

check_transform <- function(transform = NULL) {
  if (is_null(transform)) {
    return(NULL)
  }

  transform_name <- {
    if (is.character(transform)) transform
    else "fn"
  }

  transform <- try(match.fun(transform))
  chk::chk_function(transform)
  attr(transform, "transform_name") <- transform_name

  transform
}

check_valid_coef <- function(coef) {
  is_not_null(coef) &&
    is.numeric(coef) &&
    (is_null(dim(coef)) || (length(dim(coef)) == 2L && any(dim(coef) == 1L)))
}

check_valid_vcov <- function(vcov) {
  is_not_null(vcov) &&
    is.numeric(vcov) &&
    is.matrix(vcov) &&
    length(dim(vcov)) == 2L &&
    all(diag(vcov) > 0) &&
    check_symmetric_cov(vcov)
}

check_symmetric_cov <- function(x) {
  if (length(dim(x)) != 2L ||
      !identical(dim(x)[1L], dim(x)[2L])) {
    return(FALSE)
  }

  r <- cov2cor(x)

  check <- abs(r - t(r)) < sqrt(.Machine$double.eps)

  !anyNA(check) && all(check)
}

check_coefs_vcov_length <- function(vcov, coefs, vcov_supplied, coef_supplied) {
  if (!all(dim(vcov) == length(coefs))) {
    if (coef_supplied == "null") {
      if (vcov_supplied == "null") {
        .err("the covariance matrix extracted from the model has dimensions different from the number of coefficients extracted from the model. You may need to supply your own function to extract one or both of these")
      }
      if (vcov_supplied == "fun") {
        .err(sprintf("the output of the function supplied to `vcov` must have dimensions equal to the number of coefficients extracted from the model (%s)",
                     length(coefs)))
      }
      if (vcov_supplied == "num") {
        .err(sprintf("when supplied as a matrix, `vcov` must have dimensions equal to the number of coefficients extracted from the model (%s)",
                     length(coefs)))
      }
    }
    else if (coef_supplied == "fun") {
      if (vcov_supplied == "null") {
        .err(sprintf("the output of the function supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (%s)",
                     nrow(vcov)))
      }
      if (vcov_supplied == "fun") {
        .err(sprintf("the output of the function supplied to `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (%s)",
                     length(coefs)))
      }
      if (vcov_supplied == "num") {
        .err(sprintf("when supplied as a matrix, `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (%s)",
                     length(coefs)))
      }
    }
    else if (coef_supplied == "num") {
      if (vcov_supplied == "null") {
        .err(sprintf("the coefficient vector supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (%s)",
                     nrow(vcov)))
      }
      if (vcov_supplied == "fun") {
        .err(sprintf("the output of the function supplied to `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (%s)",
                     length(coefs)))
      }
      if (vcov_supplied == "num") {
        .err(sprintf("when supplied as a matrix, `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (%s)",
                     length(coefs)))
      }
    }
  }
}

check_coefs_vcov_length_mi <- function(vcov, coefs, vcov_supplied, coef_supplied) {

  if (!all_the_same(lengths(coefs))) {
    switch(
      coef_supplied,
      "null" = .err("the coefficient vectors extracted from the models must all have the same length"),
      "fun" = .err("the coefficient vectors returned by the function supplied to `coefs` must all have the same length"),
      "num" = .err("the coefficient vectors supplied to `coefs` must all have the same length")
    )
  }

  if (!all_the_same(lapply(vcov, dim))) {
    switch(
      coef_supplied,
      "null" = .err("the covariance matrices extracted from the models must all have the same dimensions"),
      "fun" = .err("the covariance matrices returned by the function supplied to `vcov` must all have the same dimensions"),
      "num" = .err("the covariance matrices supplied to `vcov` must all have the same dimensions")
    )
  }

  bad_imps <- which(vapply(seq_along(vcov), function(i) {
    !all(dim(vcov[[i]]) == length(coefs[[i]]))
  }, logical(1L)))

  in.imps <- {
    if (length(bad_imps) == length(vcov))
      "all imputations"
    else
      paste0("imputation%s ", word_list(as.character(bad_imps)))
  }

  if (is_not_null(bad_imps)) {
    if (coef_supplied == "null") {
      if (vcov_supplied == "null") {
        .err("in ", in.imps, ", the covariance matrix extracted from the model has dimensions different from the number of coefficients extracted from the model. You may need to supply your own function to extract one or both of these", n = length(bad_imps))
      }
      else if (vcov_supplied == "fun") {
        .err("in ", in.imps, ", the output of the function supplied to `vcov` must have dimensions equal to the number of coefficients extracted from the model (", length(coefs[[1]]), ")", n = length(bad_imps))
      }
      else if (vcov_supplied == "num") {
        .err("in ", in.imps, ", `vcov` must have dimensions equal to the number of coefficients extracted from the model (", length(coefs[[1]]), ") when supplied as a matrix or list of matrices", n = length(bad_imps))
      }
    }
    else if (coef_supplied == "fun") {
      if (vcov_supplied == "null") {
        .err("in ", in.imps, ", the output of the function supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (", nrow(vcov[[1]]), ")", n = length(bad_imps))
      }
      else if (vcov_supplied == "fun") {
        .err("in ", in.imps, ", the output of the function supplied to `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (", length(coefs[[1]]), ")", n = length(bad_imps))
      }
      else if (vcov_supplied == "num") {
        .err("in ", in.imps, ", `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (", length(coefs[[1]]), ") when supplied as a matrix or list of matrices", n = length(bad_imps))
      }
    }
    else if (coef_supplied == "num") {
      if (vcov_supplied == "null") {
        .err("in ", in.imps, ", the coefficient vector supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (", nrow(vcov[[1]]), ")", n = length(bad_imps))
      }
      else if (vcov_supplied == "fun") {
        .err("in ", in.imps, ", the output of the function supplied to `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (", length(coefs[[1]]), ")", n = length(bad_imps))
      }
      else if (vcov_supplied == "num") {
        .err("in ", in.imps, ", `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (", length(coefs[[1]]), ") when supplied as a matrix or list of matrices", n = length(bad_imps))
      }
    }
  }
}

check_fitlist <- function(fitlist) {
  if (!is.list(fitlist) ||
      any(vapply(fitlist, function(f) {
        b <- try(coef(f), silent = TRUE)
        is_error(b) || is_null(b) || all(is.na(b))
      }, logical(1L)))) {
    .err("`fitlist` must be a list of model fits or a `mira` object")
  }
}

check_ests.list <- function(est, test) {
  l <- lengths(est)

  if (any(l == 0)) {
    .wrn("some simulations produced no estimates; these estimates have been replaced by `NA`",
         immediate = FALSE)
    est[l == 0] <- lapply(which(l == 0), function(i) {
      rep.int(NA_real_, length(test))
    })
  }

  if (!all(l == length(test))) {
    .err("not all simulations produced estimates with the same length as the number of estimates computed from the original coefficients, indicating a problem in `FUN`")
  }
}

check_ests <- function(ests) {
  non_finites <- which(!is.finite(ests))
  if (length(non_finites) == length(ests)) {
    .err("no finite estimates were produced")
  }

  if (is_not_null(non_finites)) {
    .wrn("some non-finite values were found among the estimates, which can invalidate inferences",
         immediate = FALSE)
  }
}

process_parm <- function(object, parm) {
  #Returns numeric parm
  if (missing(parm)) {
    parm <- seq_len(ncol(object))
  }

  if (is.character(parm)) {
    ind <- match(parm, names(object))
    if (anyNA(ind)) {
      .err(sprintf("%s %%r not the name%%s of any estimated quantities.",
                   word_list(parm[is.na(ind)], quotes = TRUE)),
           n = sum(is.na(ind)))
    }
    parm <- ind
  }
  else if (is.numeric(parm)) {
    chk::chk_whole_numeric(parm)
    if (any(parm < 1) || any(parm > ncol(object))) {
      if (ncol(object) != 1L) {
        .err(sprintf("all values in `parm` must be between 1 and %s", ncol(object)))
      }

      .wrn("ignoring `parm` because only one estimate is available")
      parm <- 1L
    }
  }
  else {
    parm <- NA_integer_
  }

  parm
}

process_null <- function(null, object, parm) {
  if (is_null(null) || (is.atomic(null) && all(is.na(null)))) {
    null <- NA_real_
  }
  else {
    chk::chk_numeric(null)
  }

  if (is_not_null(names(null))) {
    null0 <- setNames(rep.int(NA_real_, length(parm)), names(object)[parm])

    if (!all(names(null) %in% names(null0))) {
      if (all(seq_len(ncol(object)) %in% parm)) {
        .err("if `null` is named, its names must correspond to estimates in the supplied `clarify_est` object")
      }
      else {
        .err("if `null` is named, its names must correspond to estimates specified in `parm`")
      }
    }

    null0[names(null)] <- null
  }
  else if (length(null) == 1L) {
    null0 <- setNames(rep.int(null, length(parm)), names(object)[parm])
  }
  else if (length(null) == length(parm)) {
    null0 <- setNames(null, names(object)[parm])
  }
  else {
    .err(sprintf("`null` must have length 1 or length equal to the number of quantities estimated (%s)",
                 length(parm)))
  }

  null0
}

#Edits to stats::.checkMFClasses
check_classes <- function(olddata, newdata) {
  new <- vapply(newdata, stats::.MFclass, character(1L))
  old <- vapply(olddata, stats::.MFclass, character(1L))
  new <- new[names(new) %in% names(old)]

  if (is_null(new)) {
    return(invisible(NULL))
  }

  old <- old[names(new)]
  old[old == "ordered"] <- "factor"
  new[new == "ordered"] <- "factor"
  new[old == "factor" & new == "character"] <- "factor"
  new[old == "character" & new == "factor"] <- "character"

  if (!identical(old, new)) {
    wrong <- old != new
    if (sum(wrong) == 1)
      .err(sprintf("variable '%s' was fit with type %s but type %s was supplied",
                   names(old)[wrong], add_quotes(old[wrong]), add_quotes(new[wrong])))
    else
      .err(sprintf("variables %s were specified with different types from the original model fit",
                   word_list(names(old)[wrong], quotes = TRUE)))
  }

  invisible(NULL)
}

check_sim_apply_wrapper_ready <- function(sim) {
  fun <- deparse1(pkg_caller_call()[[1L]])

  chk::chk_is(sim, "clarify_sim")

  if (!isTRUE(attr(sim, "use_fit", TRUE))) {
    .err(sprintf("`%s()` can only be used when a model fit was supplied to the original call to `sim()`",
                 fun))
  }

  if (inherits(sim, "clarify_misim")) {
    if (!all(vapply(sim$fit, insight::is_regression_model, logical(1L)))) {
      .err(sprintf("`%s()` can only be used with regression models",
                   fun))
    }
  }
  else if (!insight::is_regression_model(sim$fit)) {
    .err(sprintf("`%s()` can only be used with regression models",
                 fun))
  }

  invisible(NULL)
}
