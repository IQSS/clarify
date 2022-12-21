process_FUN <- function(FUN, use_fit = TRUE) {
  chk::chk_function(FUN)
  FUN_arg_names <- names(formals(FUN))

  if (length(FUN_arg_names) == 0) {
    .err("`FUN` must accept one or more arguments")
  }

  attr(FUN, "use_coefs") <- any(FUN_arg_names == "coefs")

  if (!use_fit && !attr(FUN, "use_coefs")) {
    .err("`FUN` must accept a `coefs` argument. See help(\"sim_apply\") for details")
  }

  attr(FUN, "use_fit") <- any(FUN_arg_names == "fit")

  if (!use_fit && attr(FUN, "use_fit")) {
    chk::wrn("the `fit` argument to `FUN` will be ignored. See help(\"sim_apply\") for details")
    attr(FUN, "use_fit") <- FALSE
  }

  return(FUN)
}

check_transform <- function(transform = NULL) {
  if (is.null(transform)) return(NULL)

  if (is.character(transform)) transform_name <- transform
  else transform_name <- "fn"

  transform <- try(match.fun(transform))
  chk::chk_function(transform)
  attr(transform, "transform_name") <- transform_name

  return(transform)
}

check_valid_coef <- function(coef) {
  length(coef) > 0 &&
    is.numeric(coef) &&
    (is.null(dim(coef)) || (length(dim(coef)) == 2 && any(dim(coef) == 1)))
}

check_valid_vcov <- function(vcov) {
  length(vcov) > 0 &&
    is.numeric(vcov) &&
    is.matrix(vcov) &&
    length(dim(vcov)) == 2L &&
    check_symmetric_cov(vcov)
}

check_symmetric_cov <- function(x) {
  if (length(dim(x)) != 2 ||
      !identical(dim(x)[1], dim(x)[2])) return(FALSE)

  r <- cov2cor(x)

  return(all(abs(r - t(r)) < sqrt(.Machine$double.eps)))
}

check_coefs_vcov_length <- function(vcov, coefs, vcov_supplied, coef_supplied) {
  if (!all(dim(vcov) == length(coefs))) {
    if (coef_supplied == "null") {
      if (vcov_supplied == "null") {
        .err("the covariance matrix extracted from the model has dimensions different from the number of coefficients extracted from the model. You may need to supply your own function to extract one or both of these")
      }
      else if (vcov_supplied == "fun") {
        .err("the output of the function supplied to `vcov` must have dimensions equal to the number of coefficients extracted from the model (", length(coefs), ")")
      }
      else if (vcov_supplied == "num") {
        .err("when supplied as a matrix, `vcov` must have dimensions equal to the number of coefficients extracted from the model (", length(coefs), ")")
      }
    }
    else if (coef_supplied == "fun") {
      if (vcov_supplied == "null") {
        .err("the output of the function supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (", nrow(vcov), ")")
      }
      else if (vcov_supplied == "fun") {
        .err("the output of the function supplied to `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (", length(coefs), ")")
      }
      else if (vcov_supplied == "num") {
        .err("when supplied as a matrix, `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (", length(coefs), ")")
      }
    }
    else if (coef_supplied == "num") {
      if (vcov_supplied == "null") {
        .err("the coefficient vector supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (", nrow(vcov), ")")
      }
      else if (vcov_supplied == "fun") {
        .err("the output of the function supplied to `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (", length(coefs), ")")
      }
      else if (vcov_supplied == "num") {
        .err("when supplied as a matrix, `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (", length(coefs), ")")
      }
    }
  }
}

check_coefs_vcov_length_mi <- function(vcov, coefs, vcov_supplied, coef_supplied) {

  if (!all_the_same(lengths(coefs))) {
    if (coef_supplied == "null") {
      .err("the coefficient vectors extracted from the models must all have the same length")
    }
    else if (coef_supplied == "fun") {
      .err("the coefficient vectors returned by the function supplied to `coefs` must all have the same length")
    }
    else if (coef_supplied == "num") {
      .err("the coefficient vectors supplied to `coefs` must all have the same length")
    }
  }

  if (!all_the_same(lapply(vcov, dim))) {
    if (vcov_supplied == "null") {
      .err("the covariance matrices extracted from the models must all have the same dimensions")
    }
    else if (vcov_supplied == "fun") {
      .err("the covariance matrices returned by the function supplied to `vcov` must all have the same dimensions")
    }
    else if (vcov_supplied == "num") {
      .err("the covariance matrices supplied to `vcov` must all have the same dimensions")
    }
  }

  bad_imps <- which(vapply(seq_along(vcov), function(i) {
    !all(dim(vcov[[i]]) == length(coefs[[i]]))
  }, logical(1L)))

  if (length(bad_imps) == length(vcov)) {
    in.imps <- "all imputations"
  }
  else {
    in.imps <- paste0("imputation%s ", word_list(as.character(bad_imps)))
  }

  if (length(bad_imps) > 0) {
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
        b <- try(coef(f))
        is_error(b) || is.null(b) || all(is.na(b))
      }, logical(1L)))) {
    .err("`fitlist` must be a list of model fits or a `mira` object")
  }
}

check_ests.list <- function(est, test) {
  l <- lengths(est)

  if (any(l == 0)) {
    chk::wrn("some simulations produced no estimates; these estimates have been replaced by `NA`")
    est[l == 0] <- lapply(which(l == 0), function(i) {
      rep(NA_real_, length(test))
    })
  }

  if (any(l != length(test))) {
    .err("not all simulations produced estimates with the same length as the number of estimates computed from the original coefficients, indicating a problem in `FUN`")
  }
}

check_ests <- function(ests) {
  non_finites <- which(!is.finite(ests))
  if (length(non_finites) == length(ests)) {
    .err("no finite estimates were produced")
  }
  if (length(non_finites) > 0) {
    chk::wrn("some non-finite values were found among the estimates, which can invalidate inferences")
  }
}

process_parm <- function(object, parm) {
  #Returns numeric parm
  if (missing(parm)) parm <- seq_len(ncol(object))

  if (is.character(parm)) {
    ind <- match(parm, names(object))
    if (anyNA(ind)) {
      .err(sprintf("%s not the %s of any estimated quantities.",
                   word_list(parm[is.na(ind)], is.are = TRUE, quotes = TRUE),
                   ngettext(sum(is.na(ind)), "name", "names")))
    }
    parm <- ind
  }
  else if (is.numeric(parm)) {
    chk::chk_whole_numeric(parm)
    if (any(parm < 1) || any(parm > ncol(object))) {
      if (ncol(object) == 1) {
        chk::wrn("ignoring `parm` because only one estimate is available")
        parm <- 1
      }
      else {
        .err(sprintf("all values in `parm` must be between 1 and %s", ncol(object)))
      }
    }
  }
  else {
    parm <- NA_integer_
  }

  parm
}

#Edits to stats::.checkMFClasses
check_classes <- function(olddata, newdata) {
  new <- vapply(newdata, stats::.MFclass, character(1L))
  old <- vapply(olddata, stats::.MFclass, character(1L))
  new <- new[names(new) %in% names(old)]
  if (length(new) == 0L) return(invisible(NULL))
  old <- old[names(new)]
  old[old == "ordered"] <- "factor"
  new[new == "ordered"] <- "factor"
  new[old == "factor" & new == "character"] <- "factor"
  new[old == "character" & new == "factor"] <- "character"
  if (!identical(old, new)) {
    wrong <- old != new
    if (sum(wrong) == 1)
      .err(sprintf("variable '%s' was fit with type \"%s\" but type \"%s\" was supplied",
                       names(old)[wrong], old[wrong], new[wrong]))
    else .err(sprintf("variables %s were specified with different types from the original model fit",
                          word_list(names(old)[wrong], quotes = TRUE)))
  }
  else invisible(NULL)
}

check_sim_apply_wrapper_ready <- function(sim) {
  fun <- deparse1(pkg_caller_call()[[1]])

  chk::chk_is(sim, "clarify_sim")

  if (!isTRUE(attr(sim, "use_fit"))) {
    .err(sprintf("`%s()` can only be used when a model fit was supplied to the original call to `sim()`",
                 fun))
  }
  if (inherits(sim, "clarify_misim")) {
    if (any(!vapply(sim$fit, insight::is_regression_model, logical(1L)))) {
      .err(sprintf("`%s()` can only be used with regression models",
                   fun))
    }
  }
  else {
    if (!insight::is_regression_model(sim$fit)) {
      .err(sprintf("`%s()` can only be used with regression models",
                   fun))
    }
  }
}
