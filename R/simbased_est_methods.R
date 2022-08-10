#' @exportS3Method coef simbased_est
coef.simbased_est <- function(object, ...) {
  attr(object, "original")
}

#' @exportS3Method vcov simbased_est
vcov.simbased_est <- function(object, ...) {
  class(object) <- setdiff(class(object), "simbased_est")
  cov(object)
}

#' @export
cbind.simbased_est <- function(..., deparse.level = 1) {
  if (...length() == 0) return(NULL)

  for (i in seq_len(...length())) {
    if (!inherits(...elt(i), "simbased_est")) {
      chk::err("all supplied objects must be `simbased_est` objects, the output of calls to `sim_apply()` or its wrappers")
    }
  }

  obj <- list(...)
  hashes <- lapply(obj, attr, "sim_hash")

  if (any(lengths(hashes) == 0) || any(!vapply(hashes, chk::vld_string, logical(1L)))) {
    chk::err("all supplied objects must be unmodified `simbased_est` objects")
  }
  if (!all_the_same(unlist(hashes)) || !all_the_same(unlist(lapply(obj, nrow)))) {
    chk::err("all supplied objects must be calls of `sim_apply()` or its wrappers on the same `simbased_sim` object")
  }

  out <- do.call("cbind", lapply(obj, function(x) {
    class(x) <- setdiff(class(x), "simbased_est")
    x
  }))

  attr(out, "original") <- do.call("c", lapply(obj, attr, "original"))
  attr(out, "sim_hash") <- hashes[[1]]
  class(out) <- c("simbased_est", class(out))

  return(out)
}

#' @export
transform.simbased_est <- function(`_data`, ...) {
  e <- eval(substitute(list(...)), as.data.frame(`_data`), parent.frame())

  n <- nrow(`_data`)
  if (!all(vapply(e, function(e.) length(e.) == 0 || (length(e.) == n && is.numeric(e.)), logical(1L)))) {
    chk::err("all transformations must be vector operations of the variables in the original `simbased_est` object")
  }

  e_original <- eval(substitute(list(...)), as.list(attr(`_data`, "original")), parent.frame())

  inx <- match(names(e), colnames(`_data`))
  matched <- !is.na(inx)

  if (any(matched)) {
    nulls <- lengths(e[matched]) == 0

    if (any(!nulls)) {
      `_data`[,inx[matched][!nulls]] <- as.matrix(e[matched][!nulls])
      attr(`_data`, "original")[inx[matched][!nulls]] <- as.numeric(e_original[matched][!nulls])
    }

    if (any(nulls)) {
      attrs <- attributes(`_data`)
      `_data` <- `_data`[,-inx[matched][nulls], drop = FALSE]
      mostattributes(`_data`) <- attrs
      dim(`_data`) <- c(n, length(attrs$dimnames[[2]][-inx[matched][nulls]]))
      attr(`_data`, "original") <- attr(`_data`, "original")[-inx[matched][nulls]]
      colnames(`_data`) <- names(attr(`_data`, "original"))
    }
  }
  if (!all(matched)) {
    nulls <- lengths(e[!matched]) == 0
    if (any(!nulls)) {
      new_e <- as.matrix(do.call("cbind", e[!matched][!nulls]))
      attr(new_e, "original") <- do.call("c", e_original[!matched][!nulls])
      attr(new_e, "sim_hash") <- attr(`_data`, "sim_hash")
      class(new_e) <- c("simbased_est", class(new_e))
      return(cbind.simbased_est(`_data`, new_e))
    }
  }
  return(`_data`)

}

#' @export
names.simbased_est <- function(x) {
  colnames(x)
}

#' @export
`names<-.simbased_est` <- function(x, value) {
  original_names <- names(x)
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
  x
}
