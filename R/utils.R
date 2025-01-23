#Utilities

#Turn a vector into a string with "," and "and" or "or" for clean messages.
word_list <- function(word.list = NULL, and.or = "and", is.are = FALSE, quotes = FALSE) {
  #When given a vector of strings, creates a string of the form "a and b"
  #or "a, b, and c"
  #If is.are, adds "is" or "are" appropriately

  word.list <- setdiff(word.list, c(NA_character_, ""))

  if (is_null(word.list)) {
    out <- ""
    attr(out, "plural") <- FALSE
    return(out)
  }

  word.list <- add_quotes(word.list, quotes)

  L <- length(word.list)

  if (L == 1L) {
    out <- word.list
    if (is.are) out <- paste(out, "is")
    attr(out, "plural") <- FALSE
    return(out)
  }

  if (is_null(and.or) || isFALSE(and.or)) {
    out <- paste(word.list, collapse = ", ")
  }
  else {
    and.or <- match_arg(and.or, c("and", "or"))

    if (L == 2L) {
      out <- sprintf("%s %s %s",
                     word.list[1L],
                     and.or,
                     word.list[2L])
    }
    else {
      out <- sprintf("%s, %s %s",
                     paste(word.list[-L], collapse = ", "),
                     and.or,
                     word.list[L])
    }
  }

  if (is.are) out <- sprintf("%s are", out)

  attr(out, "plural") <- TRUE

  out
}

#Add quotes to a string
add_quotes <- function(x, quotes = 2L) {
  if (isFALSE(quotes)) {
    return(x)
  }

  if (isTRUE(quotes)) {
    quotes <- '"'
  }

  if (chk::vld_string(quotes)) {
    return(paste0(quotes, x, str_rev(quotes)))
  }

  if (!chk::vld_count(quotes) || quotes > 2) {
    stop("`quotes` must be boolean, 1, 2, or a string.")
  }

  if (quotes == 0) {
    return(x)
  }

  x <- {
    if (quotes == 1) sprintf("'%s'", x)
    else sprintf('"%s"', x)
  }

  x
}

#Reverse a string
str_rev <- function(x) {
  vapply(lapply(strsplit(x, NULL), rev), paste, character(1L), collapse = "")
}

#More informative and cleaner version of base::match.arg(). Uses chk.
match_arg <- function(arg, choices, several.ok = FALSE) {
  #Replaces match.arg() but gives cleaner error message and processing
  #of arg.
  if (missing(arg)) {
    stop("No argument was supplied to match_arg.")
  }

  arg.name <- deparse1(substitute(arg), width.cutoff = 500L)

  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }

  if (is_null(arg)) {
    return(choices[1L])
  }

  if (several.ok) {
    chk::chk_character(arg, x_name = add_quotes(arg.name, "`"))
  }
  else {
    chk::chk_string(arg, x_name = add_quotes(arg.name, "`"))
    if (identical(arg, choices)) {
      return(arg[1L])
    }
  }

  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L))
    .err(sprintf("the argument to `%s` should be %s%s",
                 arg.name,
                 ngettext(length(choices), "", if (several.ok) "at least one of " else "one of "),
                 word_list(choices, and.or = "or", quotes = 2)))
  i <- i[i > 0L]

  choices[i]
}

#Format percentage for CI labels
fmt.prc <- function(probs, digits = 3L) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")
}

#Check if all values are the same
all_the_same <- function(x) {
  if (is.list(x)) {
    for (i in x) {
      if (!identical(i, x[[1L]])) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  if (is.numeric(x)) {
    return(abs(max(x) - min(x)) < 1e-9)
  }

  length(unique(x)) == 1L
}

#Tidy tryCatching
try_chk <- function(expr) {
  tryCatch({
    withCallingHandlers({
      expr
    },
    warning = function(w) {
      .wrn(conditionMessage(w), tidy = FALSE)
      invokeRestart("muffleWarning")
    })},
    error = function(e) {
      .err(conditionMessage(e), tidy = FALSE)
    })
}

#mode
Mode <- function(v, na.rm = TRUE) {
  if (is_null(v)) {
    return(v)
  }

  if (anyNA(v)) {
    if (!na.rm) {
      #Return NA, keeping type of `v`
      v <- v[1L]
      is.na(v) <- TRUE
      return(v)
    }

    v <- v[!is.na(v)]
  }

  if (is.factor(v)) {
    if (nlevels(v) == 1L) {
      return(levels(v)[1])
    }

    mode <- levels(v)[which.max(tabulate(v, nbins = nlevels(v)))]
    return(factor(mode, levels = levels(v)))
  }

  uv <- unique(v)
  if (length(uv) == 1L) {
    return(uv)
  }

  uv[which.max(tabulate(match(v, uv)))]
}

#Checks if input is "try-error", i.e., failure of try()
is_error <- function(x) {
  inherits(x, "try-error")
}

is_null <- function(x) {length(x) == 0L}
is_not_null <- function(x) {!is_null(x)}

pkg_caller_call <- function() {
  pn <- utils::packageName()
  package.funs <- c(getNamespaceExports(pn),
                    .getNamespaceInfo(asNamespace(pn), "S3methods")[, 3])

  for (i in seq_len(sys.nframe())) {
    e <- sys.call(i)

    if (is_null(n <- rlang::call_name(e))) {
      next
    }

    if (n %in% package.funs) {
      return(e)
    }
  }

  NULL
}

.err <- function(..., n = NULL, tidy = TRUE) {
  m <- chk::message_chk(..., n = n, tidy = tidy)
  rlang::abort(paste(strwrap(m), collapse = "\n"),
               call = pkg_caller_call())
}
.wrn <- function(..., n = NULL, tidy = TRUE, immediate = TRUE) {
  if (immediate && isTRUE(all.equal(0, getOption("warn")))) {
    op <- options(warn = 1)
    on.exit(options(op))
  }
  m <- chk::message_chk(..., n = n, tidy = tidy)
  rlang::warn(paste(strwrap(m), collapse = "\n"))
}
.msg <- function(..., n = NULL, tidy = TRUE) {
  m <- chk::message_chk(..., n = n, tidy = tidy)
  rlang::inform(paste(strwrap(m), collapse = "\n"), tidy = FALSE)
}

drop_sim_class <- function(x) {
  class(x) <- class(x)[!startsWith(class(x), "clarify_")]
  x
}
