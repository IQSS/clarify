#' Apply a Function To Simulated Parameter Values
#'
#' @param sim
#' @param FUN
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sim_apply <- function(sim, FUN, verbose = TRUE, ...) {
  if (missing(FUN)) {
    stop("FUN must be supplied.")
  }

  FUN <- process_FUN(FUN)

  apply_FUN <- make_apply_FUN(FUN)

  test <- try(apply_FUN(fit = sim$fit, coefs = coef(fit), ...), silent = TRUE)
  if (inherits(test, "try-error")) {
    chk::err("`FUN` failed to run on an initial check with the following error:\n",
             conditionMessage(attr(test, "condition")))
  }

  if (is.null(names(test))) names(test) <- paste0("est", seq_along(test))

  ests <- matrix(NA_real_, nrow = nrow(sim$coefs), ncol = length(test),
                 dimnames = list(NULL, names(test)))

  if (verbose) pb <- txtProgressBar(0, nrow(ests), style = 3)
  for (i in seq_len(nrow(ests))) {
    ests[i,] <- apply_FUN(fit = sim$fit, coefs = sim$coefs[i,], ...)
    if (verbose) setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)

  out <- list(est = ests,
              original = test)
  class(out) <- "simbased_est"

  out
}

coef.simbased_est <- function(object, ...) {
  object$original
}
vcov.simbased_est <- function(object, ...) {
  cov(object$est)
}

process_FUN <- function(FUN) {
  chk::chk_function(FUN)
  FUN_arg_names <- names(formals(FUN))

  if (length(FUN_arg_names) == 0) {
    chk::err("`FUN` must accept one or more arguments")
  }

  attr(FUN, "use_coefs") <- any(FUN_arg_names == "coefs")
  attr(FUN, "use_fit") <- any(FUN_arg_names == "fit")

  return(FUN)
}

make_apply_FUN <- function(FUN) {
  if (isTRUE(attr(FUN, "use_coefs")) && isTRUE(attr(FUN, "use_fit"))) {
    apply_FUN <- function(fit, coefs, ...) {
      fit <- coef_assign(fit, coefs)
      FUN(fit = fit, coefs = coefs, ...)
    }
  }
  else if (isTRUE(attr(FUN, "use_coefs"))) {
    apply_FUN <- function(fit, coefs, ...) {
      FUN(coefs = coefs, ...)
    }
  }
  else if (isTRUE(attr(FUN, "use_fit"))) {
    apply_FUN <- function(fit, coefs, ...) {
      fit <- coef_assign(fit, coefs)
      FUN(fit = fit, ...)
    }
  }
  else {
    apply_FUN <- function(fit, coefs, ...) {
      fit <- coef_assign(fit, coefs)
      FUN(fit, ...)
    }
  }

  return(apply_FUN)
}
