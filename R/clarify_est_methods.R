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

  e1_clarify_est <- inherits(e1, "clarify_est")
  e2_clarify_est <- inherits(e2, "clarify_est")

  if (e1_clarify_est && e2_clarify_est) {
    if (!identical(class(e1), class(e2))) {
      .wrn(sprintf("`%s` should only be used on `clarify_est` objects produced from the same function",
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

  if (e1_clarify_est)
    e1[] <- eval(f)
  else
    e2[] < eval(f)

  if (e1_clarify_est)
    left <- attr(e1, "original")
  if (e2_clarify_est)
    right <- attr(e2, "original")

  if (e1_clarify_est) {
    attr(e1, "original")[] <- eval(f)
    attr(e1, "contrast") <- NULL
    # class(e1) <- "clarify_est"
    return(e1)
  }

  attr(e2, "original")[] <- eval(f)
  attr(e1, "contrast") <- NULL
  # class(e2) <- "clarify_est"
  e2
}

#' @exportS3Method `[` clarify_est
`[.clarify_est` <- function(x, i, ...) {

  Narg <- nargs()

  if (Narg == 1L) {
    return(x)
  }

  if (Narg > 2L) {
    .err("`clarify_est` objects can only by subset as obj[.], not obj[., .]")
  }

  attrs <- attributes(x)
  cl <- class(x)

  x <- as.matrix(x)[, i, drop = FALSE]

  for (z in setdiff(names(attrs), c("names", "dimnames", "dim"))) {
    attr(x, z) <- attrs[[z]]
  }

  attr(x, "original") <- attr(x, "original")[i]

  if (hasName(attrs, "at")) {
    attr(x, "at") <- unname(setNames(attrs[["at"]], names(attrs[["original"]]))[i])
  }

  if (hasName(attrs, "setx")) {
    attr(x, "setx") <- attrs[["setx"]][i, , drop = FALSE]
  }

  class(x) <- cl

  x
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

#' @exportS3Method str clarify_est
str.clarify_est <- function(object,
                            max.level = NA, vec.len = getOption("str")$vec.len, digits.d = getOption("str")$digits.d,
                            nchar.max = 128, give.attr = TRUE, drop.deparse.attr = getOption("str")$drop.deparse.attr,
                            give.head = TRUE, give.length = give.head, width = getOption("width"),
                            nest.lev = 0, indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
                            comp.str = "$ ", no.list = FALSE,
                            envir = baseenv(), strict.width = getOption("str")$strict.width, formatNum = getOption("str")$formatNum,
                            list.len = getOption("str")$list.len, deparse.lines = getOption("str")$deparse.lines,
                            ...) {

  oDefs <- c("vec.len", "digits.d", "strict.width", "formatNum",
             "drop.deparse.attr", "list.len", "deparse.lines")
  strO <- getOption("str")
  if (!is.list(strO)) {
    warning("invalid options(\"str\") -- using defaults instead")
    strO <- utils::strOptions()
  }
  else {
    if (!all(names(strO) %in% oDefs))
      warning(gettextf("invalid components in options(\"str\"): %s",
                       paste(setdiff(names(strO), oDefs), collapse = ", ")),
              domain = NA)
    strO <- utils::modifyList(utils::strOptions(), strO)
  }

  oo <- options(digits = digits.d)
  on.exit(options(oo))
  le <- length(object)

  nchar.w <- function(x) nchar(x, type = "w", allowNA = TRUE)

  maybe_truncate <- function(x, nx = nchar.w(x), S = "\"",
                             ch = "| __truncated__") {
    ok <- {
      if (anyNA(nx)) !is.na(nx)
      else TRUE
    }

    if (any(lrg <- ok & nx > nchar.max)) {
      nc <- nchar(ch <- paste0(S, ch))
      if (nchar.max <= nc)
        stop(gettextf("'nchar.max = %d' is too small",
                      nchar.max), domain = NA)
      x.lrg <- x[lrg]
      tr.x <- strtrim(x.lrg, nchar.max - nc)
      if (any(ii <- tr.x != x.lrg & paste0(tr.x, S) !=
              x.lrg)) {
        x[lrg][ii] <- paste0(tr.x[ii], ch)
      }
    }
    x
  }

  nfS <- names(fStr <- formals())
  strSub <- function(obj, ...) {
    nf <- setdiff(nfS, c("object", "give.length", "comp.str",
                         "no.list", names(match.call())[-(1:2)], "..."))
    aList <- as.list(fStr)[nf]
    aList[] <- lapply(nf, function(n) eval(as.name(n)))
    do.call(function(...) str(obj, ...), c(aList, list(...)),
            quote = TRUE)
  }

  le.str <- {
    if (give.length) paste0("[1:", paste(le), "]")
    else ""
  }

  v.len <- vec.len
  std.attr <- "names"
  cl <- oldClass(object)

  if (give.attr)
    a <- attributes(object)
  dCtrl <- eval(formals(deparse)$control)

  if (drop.deparse.attr)
    dCtrl <- dCtrl[dCtrl != "showAttributes"]

  arrLenstr <- function(obj) {
    rnk <- length(di. <- dim(obj))
    di <- paste0(ifelse(di. > 1, "1:", ""), di., ifelse(di. >
                                                          0, "", " "))
    pDi <- function(...) paste(c("[", ..., "]"), collapse = "")
    if (rnk == 1)
      pDi(di[1L], "(1d)")
    else pDi(paste0(di[-rnk], ", "), di[rnk])
  }

  mod <- "num"

  le.str <- arrLenstr(object)
  if (m <- match("AsIs", cl, 0L))
    oldClass(object) <- cl[-m]
  std.attr <- "dim"

  cl <- cl[1L]
  if (cl != mod && substr(cl, 1L, nchar(mod)) != mod)
    mod <- paste0("'", cl, "' ", mod)
  std.attr <- c(std.attr, "class")

  str1 <- paste0(" ", mod, " ", le.str)

  iv.len <- round(2.5 * v.len)

  ob <- {
    if (le > iv.len)
      as.matrix(object)[seq_len(iv.len)]
    else as.matrix(object)
  }

  ao <- abs(ob <- unclass(ob[!is.na(ob)]))

  v.len <- {
    if ((all(ao > 1e-10 | ao == 0) && all(ao < 1e+10 | ao == 0) && all(abs(ob - signif(ob, digits.d)) <= 9e-16 * ao)))
      iv.len
    else
      round(1.25 * v.len)
  }

  format.fun <- formatNum

  if (!exists("format.fun")) {
    format.fun <- format
  }

  ile <- min(v.len, le)
  formObj <- function(x) maybe_truncate(paste(format.fun(x), collapse = " "), S = "")

  cat(if (give.head) paste0(str1, " "),
      formObj(
        if (ile >= 1 && mod != "...") as.matrix(object)[seq_len(ile)]
        else if (v.len > 0) object),
      if (le > v.len) " ...", "\n", sep = "")

  if (give.attr) {
    nam <- names(a)
    give.L <- give.length || identical(attr(give.length, "from"), "data.frame")
    for (i in seq_along(a)) if (all(nam[i] != std.attr)) {
      cat(indent.str, paste0("- attr(*, \"", nam[i], "\")="),
          sep = "")
      strSub(a[[i]], give.length = give.L,
             indent.str = paste(indent.str, ".."),
             nest.lev = nest.lev + 1)
    }
  }

  invisible()
}

