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
        out <- paste(word.list, collapse = paste0(" ", and.or," "))
      }
      else {
        out <- paste(paste(word.list[seq_len(L-1)], collapse = ", "),
                     word.list[L], sep = paste0(", ", and.or," "))

      }
      if (is.are) out <- paste(out, "are")
      attr(out, "plural") <- TRUE
    }

  }
  return(out)
}

#Add quotation marks around a string.
add_quotes <- function(x, quotes = 2L) {
  if (!isFALSE(quotes)) {
    if (isTRUE(quotes) || as.integer(quotes) == 2L) x <- paste0("\"", x, "\"")
    else if (as.integer(quotes) == 1L) x <- paste0("\'", x, "\'")
    else stop("'quotes' must be boolean, 1, or 2.")
  }
  x
}

#More informative and cleaner version of base::match.arg. From WeightIt with edits.
match_arg <- function(arg, choices, several.ok = FALSE) {
  #Replaces match.arg() but gives cleaner error message and processing
  #of arg.
  if (missing(arg))
    stop("No argument was supplied to match_arg.", call. = FALSE)
  arg.name <- deparse1(substitute(arg))

  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }

  if (is.null(arg)) return(choices[1L])
  else if (!is.character(arg))
    stop(paste0("The argument to '", arg.name, "' must be NULL or a character vector"), call. = FALSE)

  if (!several.ok) {
    if (identical(arg, choices)) return(arg[1L])
    if (length(arg) > 1L) {
      stop(paste0("The argument to '", arg.name, "' must be of length 1"), call. = FALSE)
    }
  }
  else if (length(arg) == 0) {
    stop(paste0("The argument to '", arg.name, "' must be of length >= 1"), call. = FALSE)
  }

  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L))
    stop(sprintf("The argument to '%s' should be %s%s.",
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
