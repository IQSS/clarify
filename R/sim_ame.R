#' @export
sim_ame <- function(sim,
                    var,
                    subset = NULL,
                    contrast = NULL,
                    verbose = TRUE,
                    cl = NULL) {
  chk::chk_is(sim, "simbased_sim")
  chk::chk_flag(verbose)

  dat <- insight::get_predictors(sim$fit)
  vals <- NULL

  if (missing(var)) {
    chk::err("`var` must be supplied, identifying the focal variable")
  }
  if (is.list(var) && chk::vld_named(var) && length(var) == 1) {
    vals <- var[[1]]
    var <- names(var)
  }
  else if (!chk::vld_string(var)) {
    chk::err("`var` must be the name of the desired focal variable or a named list of length 1 with its values")
  }

  if (!var %in% names(dat)) {
    chk::err(sprintf("the variable \"%s\" named in `var` is not present in the original model",
                     var))
  }

  var_val <- dat[[var]]

  index <- eval(substitute(subset), dat, parent.frame())
  dat <- dat[index,]

  if (!is.null(vals) && !all(vals %in% var_val)) {
    chk::err(sprintf("the values mentioned in `var` must be values %s takes on", var))
  }

  type <- {
    if (!is.null(vals) || chk::vld_character_or_factor(var_val) ||
        is.logical(var_val) || length(unique(dat[[var]])) <= 2)
      "contrast"
    else
      "slope"
  }

  if (type == "contrast") {
    if (is.null(vals)) {
      vals <- if (is.factor(var_val)) levels(var_val) else sort(unique(var_val))
    }

    if (length(vals) == 2) {
      if (!is.null(contrast)) {
        chk::chk_string(contrast)
        contrast <- tolower(contrast)
        contrast <- match_arg(contrast, c("diff", "rd", "irr", "rr", "log(irr)",
                                          "log(rr)", "or", "log(or)", "nnt"))
      }
    }
    else if (length(vals) > 2) {
      if (!is.null(contrast)) {
        chk::wrn("`contrast` is ignored when the focal variable takes on more than two levels")
        contrast <- NULL
      }
    }
    else {
      contrast <- NULL
    }

    FUN <- function(fit) {
      vapply(vals, function(x) {
        dat[[var]] <- x
        mean(insight::get_predicted(fit, data = dat, predict = "expectation",
                                    verbose = FALSE))
        # mean(predict(fit, newdata = dat, type = "response"))
      }, numeric(1L))
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

    names(out) <- paste0("M", seq_along(vals))

    if (!is.null(contrast)) {
      out <- transform(out,
                       `.C` = switch(tolower(contrast),
                                     "diff" =, "rd" = M2 - M1,
                                     "nnt" = 1/(M2 - M1),
                                     "irr" =, "rr" = M2 / M1,
                                     "log(irr)" =, "log(rr)" = log(M2 / M1),
                                     "or" = (M2/(1-M2))/(M1/(1-M1)),
                                     "log(or)" = log((M2/(1-M2))/(M1/(1-M1)))))
      names(out)[3] <- rename_contrast(contrast)
    }

    names(out)[seq_along(vals)] <- sprintf("E[Y(%s)]", vals)

  }
  else {
    eps <- 1e-6 * diff(range(var_val))

    FUN <- function(fit) {
      dat[[var]] <- dat[[var]] - eps/2
      m0 <- mean(insight::get_predicted(fit, data = dat, predict = "expectation"))
      dat[[var]] <- dat[[var]] + eps
      m1 <- mean(insight::get_predicted(fit, data = dat, predict = "expectation"))

      (m1 - m0)/eps
    }

    out <- sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)

    names(out) <- sprintf("dY/d(%s)", var)
  }


  attr(out, type) <- type
  attr(out, "var") <- var
  attr(out, "by") <- by
  out
}

rename_contrast <- function(x) {
  if (length(x) == 0) return(character(0))
  vapply(tolower(x), switch, character(1L),
         "diff" = "Diff",
         "log(irr)" = "log(IRR)",
         "log(rr)" = "log(RR)",
         "log(or)" = "log(OR)",
         toupper(x))
}
