#' @exportS3Method cbind clarify_est
cbind.clarify_est <- function(..., deparse.level = 1) {
  if (...length() == 0) return(NULL)

  for (i in seq_len(...length())) {
    if (!inherits(...elt(i), "clarify_est")) {
      .err("all supplied objects must be `clarify_est` objects, the output of calls to `sim_apply()` or its wrappers")
    }
  }

  obj <- list(...)
  hashes <- lapply(obj, attr, "sim_hash")

  if (any(lengths(hashes) == 0) || any(!vapply(hashes, chk::vld_string, logical(1L)))) {
    .err("all supplied objects must be unmodified `clarify_est` objects")
  }
  if (!all_the_same(unlist(hashes)) || !all_the_same(unlist(lapply(obj, nrow)))) {
    .err("all supplied objects must be calls of `sim_apply()` or its wrappers on the same `clarify_sim` object")
  }

  out <- do.call("cbind", lapply(obj, drop_sim_class))

  attr(out, "original") <- do.call("c", lapply(obj, attr, "original"))
  attr(out, "sim_hash") <- hashes[[1]]
  class(out) <- c("clarify_est", class(out))

  return(out)
}

#' @exportS3Method transform clarify_est
transform.clarify_est <- function(`_data`, ...) {
  e <- eval(substitute(list(...)), as.data.frame(`_data`), parent.frame())

  n <- nrow(`_data`)
  if (!all(vapply(e, function(e.) length(e.) == 0 || (length(e.) == n && is.numeric(e.)), logical(1L)))) {
    .err("all transformations must be vector operations of the variables in the original `clarify_est` object")
  }

  e_original <- eval(substitute(list(...)), as.list(attr(`_data`, "original")), parent.frame())

  inx <- match(names(e), names(`_data`))
  matched <- !is.na(inx)

  if (any(matched)) {
    nulls <- lengths(e[matched]) == 0

    if (any(!nulls)) {
      for (i in seq_along(e)[matched][!nulls]) {
        `_data`[,inx[i]] <- e[[i]]
        attr(`_data`, "original")[inx[i]] <- as.numeric(e_original[i])
      }
    }

    if (any(nulls)) {
      `_data` <- `_data`[,-inx[matched][nulls]]
    }
  }
  if (!all(matched)) {
    nulls <- lengths(e[!matched]) == 0
    if (any(!nulls)) {
      new_e <- as.matrix(do.call("cbind", e[!matched][!nulls]))
      attr(new_e, "original") <- do.call("c", e_original[!matched][!nulls])
      attr(new_e, "sim_hash") <- attr(`_data`, "sim_hash")
      class(new_e) <- c("clarify_est", class(new_e))
      return(cbind.clarify_est(`_data`, new_e))
    }
  }
  return(`_data`)

}

#' @exportS3Method names clarify_est
names.clarify_est <- function(x) {
  names(attr(x, "original"))
}

#' @exportS3Method `names<-` clarify_est
`names<-.clarify_est` <- function(x, value) {
  original_names <- names(x)
  original_class <- class(x)
  x <- drop_sim_class(x)
  colnames(x) <- value
  names(attr(x, "original")) <- value
  for (i in names(attributes(x))) {
    if (identical(names(attr(x, i)), original_names)) {
      names(attr(x, i)) <- value
    }
    if (identical(rownames(attr(x, i)), original_names)) {
      rownames(attr(x, i)) <- value
    }
    if (identical(colnames(attr(x, i)), original_names)) {
      colnames(attr(x, i)) <- value
    }
  }
  class(x) <- original_class
  x
}

#' @export
Ops.clarify_est <- function(e1, e2 = NULL) {
  unary <- nargs() == 1L
  FUN <- get(.Generic, envir = parent.frame(), mode = "function")

  if (!.Generic %in% c("+", "-", "*", "^", "%%", "%/%", "/")) {
    .err("only mathematical operations can be applied to `clarify_est` objects")
  }

  if (unary) {
    f <- quote(FUN(left))
    left <- drop_sim_class(e1)
    e1[] <- eval(f)

    left <- attr(e1, "original")
    attr(e1, "original")[] <- eval(f)
    return(e1)
  }

  f <- quote(FUN(left, right))

  if (inherits(e1, "simabsed_est") && inherits(e2, "clarify_est")) {
    if (!identical(class(e1), class(e2))) {
      chk::wrn(sprintf("`%s` should only be used on `clarify_est` objects produced from the same function",
                       .Generic))
    }

    if (!identical(attr(e1, "hash"), attr(e2, "hash"))) {
      .err(sprintf("`%s` can only be used on `clarify_est` objects originating from calls applied to the same `clarify-sim` object",
                   .Generic))
    }

    if (any(dim(e2) != dim(e1))) {
      .err(sprintf("`%s` can only be used on `clarify_est` objects with an equal number of estimated quantities",
                   .Generic))
    }

    if (!identical(attr(e1, "at"), attr(e2, "at"))) {
      .err(sprintf("`%s` can only be used on `clarify_adrf` objects with the same values of `at`",
                   .Generic))
    }
  }

  left <- drop_sim_class(e1)
  right <- drop_sim_class(e2)

  if (inherits(e1, "clarify_est"))
    e1[] <- eval(f)
  else
    e2[] < eval(f)

  if (inherits(e1, "clarify_est"))
    left <- attr(e1, "original")
  if (inherits(e2, "clarify_est"))
    right <- attr(e2, "original")

  if (inherits(e1, "clarify_est")) {
    attr(e1, "original")[] <- eval(f)
    return(e1)
  }
  else {
    attr(e2, "original")[] <- eval(f)
    return(e2)
  }
}

#' @exportS3Method `[` clarify_est
`[.clarify_est` <- function(x, i, j, ...) {

  attrs <- attributes(x)

  y <- NextMethod("[", x, drop = FALSE)

  for (z in setdiff(names(attrs), c("names", "dimnames", "dim"))) {
    attr(y, z) <- attr(x, z)
  }
  attr(y, "original") <- attr(y, "original")[j]

  if ("at" %in% names(attrs)) {
    attr(y, "at") <- unname(setNames(attr(y, "at"), names(attr(x, "original")))[j])
  }
  if ("setx" %in% names(attrs)) {
    attr(y, "setx") <- attr(y, "setx")[j,,drop = FALSE]
  }

  y
}

#' @exportS3Method as.matrix clarify_est
as.matrix.clarify_est <- function(x, ...) {
  drop_sim_class(x)
  for (i in setdiff(names(attributes(x)), c("dimnames", "dim"))) {
    attr(x, i) <- NULL
  }
  x
}

#' @exportS3Method as.data.frame clarify_est
as.data.frame.clarify_est <- function(x, ...) {
  as.data.frame(as.matrix(x), ...)
}

#' @exportS3Method dimnames clarify_est
dimnames.clarify_est <- function(x) {
  .err("do not use `colnames()`, `rownames()`, or `dimnames()` with a `clarify_est` object. Use `names()` instead")
}

#' @exportS3Method `dimnames<-` clarify_est
`dimnames<-.clarify_est` <- function(x, value) {
  .err("do not use `colnames()`, `rownames()`, or `dimnames()` with a `clarify_est` object. Use `names()` instead")
}
