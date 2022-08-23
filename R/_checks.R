process_FUN <- function(FUN, use.fit = TRUE) {
  chk::chk_function(FUN)
  FUN_arg_names <- names(formals(FUN))

  if (length(FUN_arg_names) == 0) {
    chk::err("`FUN` must accept one or more arguments")
  }

  attr(FUN, "use_coefs") <- any(FUN_arg_names == "coefs")

  if (!use.fit && !attr(FUN, "use_coefs")) {
    chk::err("`FUN` must accept a `coefs` argument. See help(\"sim_apply\") for details")
  }

  attr(FUN, "use_fit") <- any(FUN_arg_names == "fit")

  if (!use.fit && attr(FUN, "use_fit")) {
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
  is.numeric(coef) &&
    (is.null(dim(coef)) || (length(dim(coef)) == 2 && any(dim(coef) == 1)))
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
