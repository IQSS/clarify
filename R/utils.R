#Utilities
word_list <- function(word.list = NULL, and.or = c("and", "or"), is.are = FALSE, quotes = FALSE) {
  #When given a vector of strings, creates a string of the form "a and b"
  #or "a, b, and c"
  #If is.are, adds "is" or "are" appropriately
  L <- length(word.list)
  word.list <- add_quotes(word.list, quotes)

  if (L == 0) {
    out <- ""
    attr(out, "plural") <- FALSE
  }
  else {
    word.list <- word.list[!word.list %in% c(NA_character_, "")]
    L <- length(word.list)
    if (L == 0) {
      out <- ""
      attr(out, "plural") <- FALSE
    }
    else if (L == 1) {
      out <- word.list
      if (is.are) out <- paste(out, "is")
      attr(out, "plural") <- FALSE
    }
    else {
      and.or <- match_arg(and.or)
      if (L == 2) {
        out <- paste(word.list, collapse = paste0(" ", and.or, " "))
      }
      else {
        out <- paste(paste(word.list[seq_len(L - 1)], collapse = ", "),
                     word.list[L], sep = paste0(", ", and.or, " "))

      }
      if (is.are) out <- paste(out, "are")
      attr(out, "plural") <- TRUE
    }

  }

  out
}

#Add quotation marks around a string.
add_quotes <- function(x, quotes = 2L) {
  if (!isFALSE(quotes)) {
    if (isTRUE(quotes)) quotes <- 2

    if (chk::vld_string(quotes)) x <- paste0(quotes, x, quotes)
    else if (chk::vld_whole_number(quotes)) {
      if (as.integer(quotes) == 0) return(x)
      else if (as.integer(quotes) == 1) x <- paste0("\'", x, "\'")
      else if (as.integer(quotes) == 2) x <- paste0("\"", x, "\"")
      else stop("`quotes` must be boolean, 1, 2, or a string.")
    }
    else {
      stop("'quotes' must be boolean, 1, 2, or a string.")
    }
  }
  x
}

#More informative and cleaner version of base::match.arg. From WeightIt with edits.
match_arg <- function(arg, choices, several.ok = FALSE) {
  #Replaces match.arg() but gives cleaner error message and processing
  #of arg.
  if (missing(arg))
    stop("No argument was supplied to match_arg().")
  arg.name <- deparse1(substitute(arg))

  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }

  if (is.null(arg)) return(choices[1L])
  else if (!is.character(arg))
    stop(sprintf("The argument to `%s` must be NULL or a character vector", arg.name), call. = FALSE)

  if (!several.ok) {
    if (identical(arg, choices)) return(arg[1L])
    if (length(arg) > 1L) {
      stop(sprintf("The argument to `%s` must be of length 1", arg.name), call. = FALSE)
    }
  }
  else if (length(arg) == 0) {
    stop(sprintf("The argument to `%s` must be of length >= 1", arg.name), call. = FALSE)
  }

  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L))
    stop(sprintf("The argument to `%s` should be %s%s.",
                 arg.name,
                 ngettext(length(choices), "", if (several.ok) "at least one of " else "one of "),
                 word_list(choices, and.or = "or", quotes = 2)),
         call. = FALSE)

  i <- i[i > 0L]

  choices[i]
}

#Format percentage for CI labels
fmt.prc <- function(probs, digits = 3) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")
}

#Check if all values are the same
all_the_same <- function(x) {
  if (is.list(x)) {
    for (i in x) if (!identical(i, x[[1]])) return(FALSE)
    return(TRUE)
  }

  if (is.numeric(x)) {
    return(abs(max(x) - min(x)) < 1e-9)
  }

  length(unique(x)) == 1
}

#Tidy tryCatching
try_chk <- function(expr) {
  tryCatch(expr,
           error = function(e) .err(conditionMessage(e)))
}

#mode
Mode <- function(v, na.rm = TRUE) {
  if (anyNA(v)) {
    if (na.rm) v <- v[!is.na(v)]
    else {
      #Return NA, keeping type of `v`
      v <- v[1]
      is.na(v) <- TRUE
      return(v)
    }
  }

  if (length(v) == 0) return(v)
  if (is.factor(v)) {
    if (nlevels(v) == 1) return(levels(v)[1])
    mode <- levels(v)[which.max(tabulate(v, nbins = nlevels(v)))]
    mode <- factor(mode, levels = levels(v))
  }
  else {
    uv <- unique(v)
    if (length(uv) == 1) return(uv)
    mode <- uv[which.max(tabulate(match(v, uv)))]
  }
  mode
}

#Recursively search a list for a value (key) and return location of value
list.search <- function(x, key) {
  for (i in seq_along(x)) {
    if (identical(x[[i]], key)) {
      return(i)
    }
    else if (is.list(x[[i]])) {
      l <- list.search(x[[i]], key)
      if (!is.null(l)) return(c(i, l))
    }
  }

  NULL
}

#Checks if object inherits from any of the supplied classes
inherits_any <- function(x, what) {
  chk::chk_character(what)

  for (i in what) {
    if (inherits(x, i)) return(TRUE)
  }

  FALSE
}

#Checks if input is "try-error", i.e., failure of try()
is_error <- function(x) {
  inherits(x, "try-error")
}

pkg_caller_call <- function(start = 1) {
  package.funs <- c(getNamespaceExports(utils::packageName()),
                    .getNamespaceInfo(asNamespace(utils::packageName()), "S3methods")[, 3])
  k <- start #skip checking pkg_caller_call()
  e_max <- start
  while (!is.null(e <- rlang::caller_call(k))) {
    if (!is.null(n <- rlang::call_name(e)) &&
        n %in% package.funs) e_max <- k
    k <- k + 1
  }
  rlang::caller_call(e_max)
}

.err <- function(...) {
  chk::err(..., call = pkg_caller_call(start = 2))
}

drop_sim_class <- function(x) {
  class(x) <- class(x)[!startsWith(class(x), "clarify_")]
  x
}
