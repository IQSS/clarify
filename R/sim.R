sim <- function(fit, n = 1e3, vcov = NULL) {
  V <- vcov(fit)
  B <- coef(fit)

  sampler <- get_sampling_dist(fit)

  call <- insight::get_call(fit)

  out <- list(call = call,
              coefs = sampler(n, B, V),
              fit = fit)

  class(out) <- "simbased_sim"

  out
}

sim_apply <- function(sim, FUN, ...) {
  if (missing(FUN)) {
    stop("FUN must be supplied.")
  }

  test <- try(FUN(sim$fit, ...))
  if (inherits(test, "try-error")) {
    stop("Initial check failed.")
  }

  if (is.null(names(test))) names(test) <- paste0("est", seq_along(test))

  ests <- matrix(NA_real_, nrow = nrow(sim$coefs), ncol = length(test),
                dimnames = list(NULL, names(test)))

  pb <- txtProgressBar(0, nrow(ests), style = 3)
  for (i in seq_len(nrow(ests))) {
    sim$fit <- coef_assign(sim$fit, sim$coefs[i,])
    ests[i,] <- FUN(sim$fit, ...)
    setTxtProgressBar(pb, i)
  }
  close(pb)

  out <- list(est = ests)
  class(out) <- "simbased_est"

  out
}

summary.simbased_est <- function(object, alpha = .05, ...) {
  ans <- list()
  est <- coef(object)
  se <- sqrt(diag(vcov(object)))
  z <- est/se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)

  fmt.prc <- function(probs, digits = 3) {
    paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")
  }

  pct <- fmt.prc(c(alpha/2, 1-alpha/2))
  zcrit <- qnorm(c(alpha/2, 1-alpha/2))

  ci <- matrix(nrow = length(est), ncol = 2,
               dimnames = list(names(est), pct))
  ci[,1] <- est + se*zcrit[1]
  ci[,2] <- est + se*zcrit[2]

  ans <- list(coefficients = cbind(Estimate = est, `Std. Error` = se,
                                   `z value` = z, `Pr(>|z|)` = p,
                                   ci))
  class(ans) <- "summary.simbased_est"
  ans
}

sim_plot <- function(x, est, ci = TRUE, alpha = .05) {
  if (!inherits(x, "simbased_est")) {
    stop("'x' must be a simbased_est object.")
  }

  est_names <- colnames(x$est)
  if (missing(est)) est <- seq_len(ncol(x$est))

  if (is.character(est)) {
    ind <- match(est, est_names)
    if (anyNA(ind)) {
      stop(sprintf("%s not the %s of any estimated quantities.",
                   word_list(est[is.na(ind)], is.are = TRUE, quotes = TRUE),
                   ngettext(sum(is.na(ind)), "name", "names")))
    }
    est <- ind[!is.na(ind)]
  }

  est_long <- do.call("rbind", lapply(est, function(e) {
    data.frame(est = est_names[e], val = x$est[,e])
  }))

  est_long[[1]] <- factor(est_long[[1]], levels = est_names[est])
# browser()
  p <- ggplot() +
    geom_density(data = est_long, mapping = aes(x = val),
                 color = "black", fill = "white") +
    geom_hline(yintercept = 0)

  if (ci) {
    zcrit <- qnorm(c(alpha/2, 1-alpha/2))
    ci <- do.call("rbind", lapply(est, function(e) {
      data.frame(est = est_names[e], val = mean(x$est[,e]) + sd(x$est[,e])*zcrit)
    }))

    p <- p + geom_vline(data = ci, mapping = aes(xintercept = val), color = "red",
                        linetype = 1)
  }

  p <- p + facet_wrap(vars(est), scales = "free") +
    labs(x = "Estimate")

  p
}

get_sampling_dist <- function(fit) {
  df <- try(df.residual(fit), silent = TRUE)
  if (inherits(df, "try-error")) df <- NULL

  if (is.null(df)) df <- 0
  if (any(is.finite(df)) && all(df > 0)) {
    f <- function(n, mu, cov) {
      sigma <- cov*(df - 2)/df
      mvnfast::rmvt(n, mu = mu, sigma = sigma, df = df, kpnames = TRUE)
    }
  }
  else {
    f <- function(n, mu, cov) {
      mvnfast::rmvn(n, mu = mu, sigma = cov, kpnames = TRUE)
    }
  }

  return(f)
}

coef_assign <- function(fit, coef) {
  UseMethod("coef_assign")
}
coef_assign.default <- function(fit, coefs){
  fit$coefficients <- coefs
  return(fit)
}

coef.simbased_est <- function(object, ...) {
  colMeans(object$est)
}
vcov.simbased_est <- function(object, ...) {
  cov(object$est)
}
