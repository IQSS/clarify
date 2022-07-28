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
