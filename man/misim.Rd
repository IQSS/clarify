% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misim.R
\name{misim}
\alias{misim}
\title{Simulate model coefficients after multiple imputation}
\usage{
misim(fitlist, n = 1000, vcov = NULL, coefs = NULL, dist = NULL)
}
\arguments{
\item{fitlist}{a list of model fits, one for each imputed dataset, or a \code{mira} object (the output of a call to \code{with()} applied to a \code{mids} object in \code{mice}).}

\item{n}{the number of simulations to run for each imputed dataset; default is 1000. More is always better but resulting calculations will take longer.}

\item{vcov}{a square covariance matrix of the coefficient covariance estimates, a function to use to extract it from \code{fit}, or a list thereof with an element for each imputed dataset. By default, uses \code{\link[stats:vcov]{stats::vcov()}} or \code{\link[insight:get_varcov]{insight::get_varcov()}} if that doesn't work.}

\item{coefs}{a vector of coefficient estimates, a function to use to extract it from \code{fit}, or a list thereof with an element for each imputed dataset. By default, uses \code{\link[stats:coef]{stats::coef()}} or \code{\link[insight:get_parameters]{insight::get_parameters()}} if that doesn't work.}

\item{dist}{a character vector containing the name of the multivariate distribution(s) to use to draw simulated coefficients. Should be one of \code{"normal"} (multivariate normal distribution) or \code{"t_{#}"} (multivariate t distribution), where \verb{\{#\}} corresponds to the desired degrees of freedom (e.g., \code{"t_100"}). If \code{NULL}, the right distributions to use will be determined based on heuristics; see \code{\link[=sim]{sim()}} for details.}
}
\value{
A \code{clarify_misim} object, which inherits from \code{clarify_sim} and has the following components:
\item{sim.coefs}{a matrix containing the simulated coefficients with a column for each coefficient and a row for each simulation for each imputation}
\item{coefs}{a matrix containing the original coefficients extracted from \code{fitlist} or supplied to \code{coefs}, with a row per imputation.}
\item{fit}{the list of model fits supplied to \code{fitlist}}
\item{imp}{a identifier of which imputed dataset each set of simulated coefficients corresponds to.}
The \code{"dist"} attribute contains \code{"normal"} if the coefficients were sampled from a multivariate normal distribution and \code{"t({df})"} if sampled from a multivariate t distribution. The \code{"clarify_hash"} attribute contains a unique hash generated by \code{\link[rlang:hash]{rlang::hash()}}.
}
\description{
\code{misim()} simulates model parameters from multivariate normal or t distributions after multiple imputation that are then used by \code{\link[=sim_apply]{sim_apply()}} to calculate quantities of interest.
}
\details{
\code{misim()} essentially combines multiple \code{sim()} calls applied to a list of model fits, each fit in an imputed dataset, into a single combined pool of simulated coefficients. When simulation-based inference is to be used with multiply imputed data, many imputations are required; see Zhou and Reiter (2010).
}
\examples{
\dontshow{if (requireNamespace("Amelia", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data("africa", package = "Amelia")

# Multiple imputation using Amelia
a.out <- Amelia::amelia(x = africa, m = 10,
                        cs = "country",
                        ts = "year", logs = "gdp_pc",
                        p2s = 0)

fits <- with(a.out, lm(gdp_pc ~ infl * trade))

# Simulate coefficients
s <- misim(fits)
s
\dontshow{\}) # examplesIf}
}
\references{
Zhou, X., & Reiter, J. P. (2010). A Note on Bayesian Inference After Multiple Imputation. \emph{The American Statistician}, 64(2), 159–163. \doi{10.1198/tast.2010.09109}
}
\seealso{
\itemize{
\item \code{\link[=sim]{sim()}} for simulating model coefficients for a single dataset
\item \code{\link[=sim_apply]{sim_apply()}} for applying a function to each set of simulated coefficients
\item \code{\link[=sim_ame]{sim_ame()}} for computing average marginal effects in each simulation draw
\item \code{\link[=sim_setx]{sim_setx()}} for computing marginal predictions and first differences at typical values in each simulation draw
}
}
