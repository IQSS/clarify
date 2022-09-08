process_FUN <- function(FUN, use_fit = TRUE) {
  chk::chk_function(FUN)
  FUN_arg_names <- names(formals(FUN))

  if (length(FUN_arg_names) == 0) {
    chk::err("`FUN` must accept one or more arguments")
  }

  attr(FUN, "use_coefs") <- any(FUN_arg_names == "coefs")

  if (!use_fit && !attr(FUN, "use_coefs")) {
    chk::err("`FUN` must accept a `coefs` argument. See help(\"sim_apply\") for details")
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
        chk::err("the covariance matrix extracted from the model has dimensions different from the number of coefficients extracted from the model. You may need to supply your own function to extract one or both of these")
      }
      else if (vcov_supplied == "fun") {
        chk::err("the output of the function supplied to `vcov` must have dimensions equal to the number of coefficients extracted from the model (", length(coefs), ")")
      }
      else if (vcov_supplied == "num") {
        chk::err("when supplied as a matrix, `vcov` must have dimensions equal to the number of coefficients extracted from the model (", length(coefs), ")")
      }
    }
    else if (coef_supplied == "fun") {
      if (vcov_supplied == "null") {
        chk::err("the output of the function supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (", nrow(vcov), ")")
      }
      else if (vcov_supplied == "fun") {
        chk::err("the output of the function supplied to `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (", length(coefs), ")")
      }
      else if (vcov_supplied == "num") {
        chk::err("when supplied as a matrix, `vcov` must have dimensions equal to the length of the output of the function supplied to `coefs` (", length(coefs), ")")
      }
    }
    else if (coef_supplied == "num") {
      if (vcov_supplied == "null") {
        chk::err("the coefficient vector supplied to `coefs` must have length equal to the dimensions of the covariance matrix extracted from the model (", nrow(vcov), ")")
      }
      else if (vcov_supplied == "fun") {
        chk::err("the output of the function supplied to `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (", length(coefs), ")")
      }
      else if (vcov_supplied == "num") {
        chk::err("when supplied as a matrix, `vcov` must have dimensions equal to the length of the coefficient vector supplied to `coefs` (", length(coefs), ")")
      }
    }
  }
}

check_fitlist <- function(fitlist) {
  if (!is.list(fitlist) ||
      any(vapply(fitlist, function(f) {
        b <- coef(f)
        is.null(b) || all(is.na(b))
      }, logical(1L)))) {
    chk::err("`fitlist` must be a list of model fits or a `mira` object")
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
    chk::err("not all simulations produced estimates with the same length as the number of estimates computed from the original coefficients, indicating a problem in `FUN`")
  }
}

check_ests <- function(ests) {
  non_finites <- which(!is.finite(ests))
  if (length(non_finites) == length(ests)) {
    chk::err("no finite estimates were produced")
  }
  if (length(non_finites) > 0) {
    chk::wrn("some non-finite values were found among the estimates, which can invalidate inferences")
  }
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
      chk::err(sprintf("variable '%s' was fit with type \"%s\" but type \"%s\" was supplied",
                    names(old)[wrong], old[wrong], new[wrong]))
    else chk::err(sprintf("variables %s were specified with different types from the original model fit",
                          word_list(names(old)[wrong], quotes = TRUE)))
  }
  else invisible(NULL)
}
