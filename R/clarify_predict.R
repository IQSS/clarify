clarify_predict <- function(x, newdata = NULL, group = NULL, type = NULL, ...) {

  args <- list(model = x, vcov = FALSE, ...)
  args$type <- type
  args$newdata <- newdata

  p <- try(do.call(marginaleffects::get_predict, args), silent = TRUE)

  if (is_not_null(p) && !is_error(p)) {
    if (is_not_null(group) && "group" %in% names(p)) {
      p <- .subset_group(p, group)
    }

    return(p)
  }

  ord_mean <- identical(type, "mean") && isTRUE(insight::model_info(x)$is_ordinal)

  if (ord_mean) {
    args$type <- NULL

    p <- try(do.call(marginaleffects::get_predict, args), silent = TRUE)

    if (is_not_null(p) && !is_error(p)) {
      return(.get_ordinal_mean_preds(p))
    }
  }

  .err("predicted values could not be extracted from the model")
}

.subset_group <- function(pred, group = NULL) {
  if (is_null(group)) {
    return(pred)
  }

  pred[pred$group == group, , drop = FALSE]
}

.get_p <- function(pred) {
  if ("estimate" %in% names(pred)) pred[["estimate"]]
  else pred[["predicted"]]
}

.get_ordinal_mean_preds <- function(p) {
  ids <- unique(p$rowid)
  groups <- unique(p$group)
  m <- matrix(p$estimate, nrow = length(ids), ncol = length(groups))

  if (anyNA(groups)) {
    nas <- is.na(groups)
    gn <- rep.int(NA_real_, length(groups))

    if (!anyNA(suppressWarnings(g <- as.numeric(groups[!nas])))) {
      gn[!nas] <- g
    }
    else {
      gn[!nas] <- seq_along(g)
    }
  }
  else {
    if (!anyNA(suppressWarnings(g <- as.numeric(groups)))) {
      groups <- g
    }
    else {
      groups <- seq_along(g)
    }
  }

  data.frame(rowid = ids,
             estimate = drop(m %*% groups))
}
