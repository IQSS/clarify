#' #' Transform and combine `clarify_est` objects
#'
#' @description
#' `transform()` modifies a `clarify_est` object by allowing for the calculation of new quantities from the existing quantities without re-simulating them. `cbind()` binds two `clarify_est` objects together.
#'
#' @param _data the `clarify_est` object to be transformed.
#' @param ... for `transform()`, arguments in the form `name = value`, where `name` is the name of a new quantity to be computed and `value` is an expression that is a function of the existing quantities corresponding to the new quantity to be computed. See Details. For `cbind()`, `clarify_est` objects to be combined.
#' @param deparse.level ignored.
#'
#' @details
#' For `transform()`, the expression on the right side of the `=` should use the names of the existing quantities (e.g., `` `E[Y(1)]` - `E[Y(1)]` ``), with `` ` `` appropriately included when the quantity name include parentheses or brackets. Alternatively, it can use indexes prefixed by `.b`, e.g., `.b2 - .b1`, to refer to the corresponding quantity by position. This can aid in computing derived quantities of quantities with complicated names. (Note that if a quantity is named something like `.b1`, it will need to be referred to by position rather than name, as the position-based label takes precedence). See examples. Setting an existing value to `NULL` will remove that quantity from the object.
#'
#' `cbind()` does not rename the quanities or check for uniqueness of the names, so it is important to rename them yourself prior to combining the objects.
#'
#' @return
#' A `clarify_est` object, either with new columns added (when using `transform()`) or combining two `clarify_est` objects. Note that any type attributes corresponding to the `sim_apply()` wrapper used (e.g., `sim_ame()`) is lost when using either function. This can affect any helper functions (e.g., `plot()`) designed to work with the output of specific wrappers.
#'
#' @seealso [transform()], [cbind()], [sim()]
#'
#' @examples
#' data("lalonde", package = "MatchIt")
#'
#' # Fit the model
#' fit <- lm(re78 ~ treat * (age + educ + race +
#'              married + re74 + re75),
#'            data = lalonde)
#'
#' # Simulate coefficients
#' set.seed(123)
#' s <- sim(fit, n = 100)
#'
#' # Average adjusted predictions for `treat` within
#' # subsets of `race`
#' est_b <- sim_ame(s, var = "treat", verbose = FALSE,
#'                  subset = race == "black")
#' est_b
#'
#' est_h <- sim_ame(s, var = "treat", verbose = FALSE,
#'                  subset = race == "hispan")
#' est_h
#'
#' # Compute differences between adjusted predictions
#' est_b <- transform(est_b,
#'                    diff = `E[Y(1)]` - `E[Y(0)]`)
#' est_b
#'
#' est_h <- transform(est_h,
#'                    diff = `E[Y(1)]` - `E[Y(0)]`)
#' est_h
#'
#' # Bind estimates together after renaming
#' names(est_b) <- paste0(names(est_b), "_b")
#' names(est_h) <- paste0(names(est_h), "_h")
#'
#' est <- cbind(est_b, est_h)
#' est
#'
#' # Compute difference in race-specific differences
#' est <- transform(est,
#'                  `diff-diff` = .b6 - .b3)
#'
#' summary(est,
#'         parm = c("diff_b", "diff_h", "diff-diff"))
#'
#' # Remove last quantity by using `NULL`
#' transform(est, `diff-diff` = NULL)

#' @exportS3Method transform clarify_est
#' @name transform.clarify_est
transform.clarify_est <- function(`_data`, ...) {

  # Process dots to substitute .b{#} for corresponding value
  dots <- substitute(list(...))

  available_b <- sprintf(".b%s", seq_along(names(`_data`)))

  names_list <- setNames(lapply(add_quotes(names(`_data`), "`"), str2lang),
                         available_b)

  for (i in seq_along(dots)[-1]) {
    if (!is.null(dots[[i]]))
      dots[[i]] <- do.call("substitute", list(dots[[i]], names_list))
  }

  e <- try(eval(dots, as.data.frame(`_data`), parent.frame()), silent = TRUE)

  if (is_error(e)) .err(conditionMessage(attr(e, "condition")), tidy = FALSE)

  n <- nrow(`_data`)
  if (!all(vapply(e, function(e.) length(e.) == 0 || (length(e.) == n && is.numeric(e.)), logical(1L)))) {
    .err("all transformations must be vector operations of the variables in the original `clarify_est` object")
  }

  e_original <- eval(dots, as.list(attr(`_data`, "original")), parent.frame())

  inx <- match(names(e), names(`_data`))
  matched <- !is.na(inx)

  if (any(matched)) {
    nulls <- lengths(e[matched]) == 0

    if (any(!nulls)) {
      for (i in seq_along(e)[matched][!nulls]) {
        `_data`[, inx[i]] <- e[[i]]
        attr(`_data`, "original")[inx[i]] <- as.numeric(e_original[i])
      }
    }

    if (any(nulls)) {
      `_data` <- `_data`[-inx[matched][nulls]]
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

  `_data`
}

#' @exportS3Method cbind clarify_est
#' @rdname transform.clarify_est
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

  out
}
