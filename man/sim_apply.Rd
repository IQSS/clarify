% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_apply.R
\name{sim_apply}
\alias{sim_apply}
\title{Apply a function to simulated parameter values}
\usage{
sim_apply(sim, FUN, verbose = TRUE, cl = NULL, ...)
}
\arguments{
\item{sim}{a \code{clarify_sim} object; the output of a call to \code{\link[=sim]{sim()}} or
\code{\link[=misim]{misim()}}.}

\item{FUN}{a function to be applied to each set of simulated coefficients.
See Details.}

\item{verbose}{\code{logical}; whether to display a text progress bar indicating
progress and estimated time remaining for the procedure. Default is \code{TRUE}.}

\item{cl}{a cluster object created by \code{\link[parallel:makeCluster]{parallel::makeCluster()}}, or an
integer to indicate the number of child-processes (integer values are
ignored on Windows) for parallel evaluations. See \code{\link[pbapply:pbapply]{pbapply::pblapply()}} for
details. If \code{NULL}, no parallelization will take place.}

\item{...}{optional arguments passed to \code{FUN}.}
}
\value{
A \code{clarify_est} object, which is a matrix with a column for each
estimated quantity and a row for each simulation. The original estimates
(\code{FUN} applied to the original coefficients or model fit object) are stored
in the attribute \code{"original"}. The \code{"sim_hash"} attribute contains the
simulation hash produced by \code{sim()}.
}
\description{
\code{sim_apply()} applies a function that produces quantities of
interest to each set of simulated coefficients produced by \code{\link[=sim]{sim()}}; these
calculated quantities form the posterior sampling distribution for the
quantities of interest. Capabilities are available for parallelization.
}
\details{
\code{sim_apply()} applies a function, \code{FUN}, to each set of simulated
coefficients, similar to \code{\link[=apply]{apply()}}. This function should return a numeric
vector containing one or more estimated quantities. This should be a named
vector to more easily keep track of the meaning of each estimated quantity.
Care should be taken to ensure that the returned vector is the same length
each time \code{FUN} is called. \code{NA}s are allowed in the output but should be
avoided if possible.

The arguments to \code{FUN} can be specified in a few ways. If \code{FUN} has an
argument called \code{coefs}, a simulated set of coefficients will be passed to
this argument, and \code{FUN} should compute and return a quantity based on the
coefficients (e.g., the difference between two coefficients if one wants to
test whether two coefficients are equal). If \code{FUN} has an argument called
\code{fit}, a model fit object of the same type as the one originally supplied
to \code{sim()} (e.g., an \code{lm} or \code{glm} object) will be passed to this argument,
where the coefficients of the fit object have been replaced by the
simulated coefficients generated by \code{sim()}, and \code{FUN} should compute and
return a quantity based on the model fit (e.g., a computation based on the
output of \code{predict()}). If neither \code{coefs} nor \code{fit} are the names of
arguments to \code{FUN}, the model fit object with replaced coefficients will be
supplied to the first argument of \code{FUN}.

When custom coefficients are supplied to \code{sim()}, i.e., when the \code{coefs}
argument to \code{sim()} is not left at its default value, \code{FUN} must accept a
\code{coefs} argument and a warning will be thrown if it accepts a \code{fit}
argument. This is because \code{sim_apply()} does not know how to reconstruct
the original fit object with the new coefficients inserted. The quantities
computed by \code{sim_apply()} must therefore be computed directly from the
coefficients.

If \code{FUN} is not supplied at all, the simulated values of the coefficients will be returned in the output with a warning. Set \code{FUN} to \code{NULL} or \code{verbose} to \code{FALSE} to suppress this warning.
\subsection{\code{sim_apply()} with multiply imputed data}{

When using \code{\link[=misim]{misim()}} and \code{sim_apply()} with multiply imputed data, the
coefficients are supplied to the model fit corresponding to the imputation
identifier associated with each set of coefficients, which means if \code{FUN}
uses a dataset extracted from a model (e.g., using \code{\link[insight:get_data]{insight::get_data()}}), it will do so from the model fit in
the corresponding imputation.

The original estimates (see Value below) are computed as the mean of the
estimates across the imputations using the original coefficients averaged
across imputations. That is, first, the coefficients estimated in the
models in the imputed datasets are combined to form a single set of pooled
coefficients; then, for each imputation, the quantities of interest are
computed using the pooled coefficients; finally, the mean of the resulting
estimates across the imputations are taken as the "original" estimates.
Note this procedure is only valid for quantities with symmetric sampling
distributions, which excludes quantities like risk ratios and odds ratios,
but includes log risk ratios and log odds ratios. The desired quantities
can be transformed from their log versions using
\code{\link[=transform]{transform()}}.
}
}
\examples{

data("lalonde", package = "MatchIt")
fit <- lm(re78 ~ treat + age + race + nodegree + re74,
          data = lalonde)
coef(fit)

set.seed(123)
s <- sim(fit, n = 500)

# Function to compare predicted values for two units
# using `fit` argument
sim_fun <- function(fit) {
  pred1 <- unname(predict(fit, newdata = lalonde[1,]))
  pred2 <- unname(predict(fit, newdata = lalonde[2,]))
  c(pred1 = pred1, pred2 = pred2)
}

est <- sim_apply(s, sim_fun, verbose = FALSE)

# Add difference between predicted values as
# additional quantity
est <- transform(est, `diff 1-2` = pred1 - pred2)

# Examine estimates and confidence intervals
summary(est)

# Function to compare coefficients using `coefs`
# argument
sim_fun <- function(coefs) {
  setNames(coefs["racewhite"] - coefs["racehispan"],
           "wh - his")
}

est <- sim_apply(s, sim_fun, verbose = FALSE)

# Examine estimates and confidence intervals
summary(est)

# Another way to do the above:
est <- sim_apply(s, FUN = NULL)
est <- transform(est,
                 `wh - his` = `racewhite` - `racehispan`)

summary(est, parm = "wh - his")

}
\seealso{
\itemize{
\item \code{\link[=sim]{sim()}} for generating the simulated coefficients
\item \code{\link[=summary.clarify_est]{summary.clarify_est()}} for computing p-values and confidence intervals for
the estimated quantities
\item \code{\link[=plot.clarify_est]{plot.clarify_est()}} for plotting estimated
quantities and their simulated posterior sampling distribution.
}
}
