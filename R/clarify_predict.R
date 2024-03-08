clarify_predict <- function(x, newdata = NULL, group = NULL, type = NULL) {
  ord_mean <- identical(type, "mean") && isTRUE(insight::model_info(x)$is_ordinal)

  if (ord_mean) {
    type <- NULL
    group <- NULL
  }

  args <- list(model = x, newdata = newdata, vcov = FALSE)
  args$type <- type

  p <- try(do.call(marginaleffects::get_predict, args), silent = TRUE)

  if (length(p) == 0L || is_error(p)) {
    .err("predicted values could not be extracted from the model")
  }

  if (ord_mean) {
    p <- .get_ordinal_mean_preds(p)
  }
  else if (!is.null(group) && "group" %in% names(p)) {
    p <- .subset_group(p, group)
  }

  p
}

.subset_group <- function(pred, group = NULL) {
  if (is.null(group)) pred
  else pred[pred$group == group, , drop = FALSE]
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
    gn <- rep(NA_real_, length(groups))

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
