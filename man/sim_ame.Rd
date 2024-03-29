% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_ame.R
\name{sim_ame}
\alias{sim_ame}
\alias{print.clarify_ame}
\title{Compute average marginal effects}
\usage{
sim_ame(
  sim,
  var,
  subset = NULL,
  by = NULL,
  contrast = NULL,
  outcome = NULL,
  type = NULL,
  eps = 1e-05,
  verbose = TRUE,
  cl = NULL
)

\method{print}{clarify_ame}(x, digits = NULL, max.ests = 6, ...)
}
\arguments{
\item{sim}{a \code{clarify_sim} object; the output of a call to \code{\link[=sim]{sim()}} or
\code{\link[=misim]{misim()}}.}

\item{var}{either the name of a variable for which marginal effects are to be computed or a named list of length one containing the values the variable should take. If a list is supplied or the named variables is categorical (factor, character, or having two values), categorical calculations will be triggered. Otherwise, continuous calculations will be triggered. See Details.}

\item{subset}{optional; a vector used to subset the data used to compute the marginal effects. This will be evaluated within the original dataset used to fit the model using \code{\link[=subset]{subset()}}, so nonstandard evaluation is allowed.}

\item{by}{a one-sided formula or character vector containing the names of variables for which to stratify the estimates. Each quantity will be computed within each level of the complete cross of the variables specified in \code{by}.}

\item{contrast}{a string containing the name of a contrast between the average marginal means when the variable named in \code{var} is categorical and takes on two values. Allowed options include \code{"diff"} for the difference in means (also \code{"rd"}), \code{"rr"} for the risk ratio (also \code{"irr"}), \verb{"log(rr):} for the log risk ratio (also \code{"log(irr)"}), \code{"sr"} for the survival ratio, \verb{"log(sr):} for the log survival ratio, \code{"srr"} for the switch relative risk (also \code{"grrr"}), \code{"or"} for the odds ratio, \code{"log(or)"} for the log odds ratio, and \code{"nnt"} for the number needed to treat. These options are not case sensitive, but the parentheses must be included if present.}

\item{outcome}{a string containing the name of the outcome or outcome level for multivariate (multiple outcomes) or multi-category outcomes. Ignored for univariate (single outcome) and binary outcomes.}

\item{type}{a string containing the type of predicted values (e.g., the link or the response). Passed to \code{\link[marginaleffects:get_predict]{marginaleffects::get_predict()}} and eventually to \code{predict()} in most cases. The default and allowable option depend on the type of model supplied, but almost always corresponds to the response scale (e.g., predicted probabilities for binomial models).}

\item{eps}{when the variable named in \code{var} is continuous, the value by which to change the variable values to approximate the derivative. See Details.}

\item{verbose}{\code{logical}; whether to display a text progress bar indicating
progress and estimated time remaining for the procedure. Default is \code{TRUE}.}

\item{cl}{a cluster object created by \code{\link[parallel:makeCluster]{parallel::makeCluster()}}, or an
integer to indicate the number of child-processes (integer values are
ignored on Windows) for parallel evaluations. See \code{\link[pbapply:pbapply]{pbapply::pblapply()}} for
details. If \code{NULL}, no parallelization will take place.}

\item{x}{a \code{clarify_ame} object.}

\item{digits}{the minimum number of significant digits to be used; passed to \code{\link[=print.data.frame]{print.data.frame()}}.}

\item{max.ests}{the maximum number of estimates to display.}

\item{...}{optional arguments passed to \code{FUN}.}
}
\value{
A \code{clarify_ame} object, which inherits from \code{clarify_est} and is similar to
the output of \code{sim_apply()}, with the additional attributes \code{"var"} containing
the variable named in \code{var} and \code{"by"} containing the names of the variables specified in \code{by} (if any). The average adjusted predictions will be named
\code{E[Y({v})]}, where \code{{v}} is replaced with the values the focal variable
(\code{var}) takes on. The average marginal effect for a continuous \code{var} will be
named \code{E[dY/d({x})]} where \code{{x}} is replaced with \code{var}. When \code{by} is specified, the average adjusted predictions will be named \code{E[Y({v})|{b}]} and the average marginel effect \code{E[dY/d({x})|{b}]} where \code{{b}} is a comma-separated list of of values of the \code{by} variables at which the quantity is computed. See examples.
}
\description{
\code{sim_ame()} is a wrapper for \code{\link[=sim_apply]{sim_apply()}} that computes average marginal effects, the average effect of changing a single variable from one value to another (i.e., from one category to another for categorical variables or a tiny change for continuous variables).
}
\details{
\code{sim_ame()} operates differently depending on whether continuous or categorical calculations are triggered. To trigger categorical calculations, \code{var} should be a string naming a factor, character, or binary variable or a named list with specific values given (e.g., \code{var = list(x1 = c(1, 2 ,3))}). Otherwise, continuous calculations are triggered.

Categorical calculations involve computing average marginal means at each level of \code{var}. The average marginal mean is the average predicted outcome value after setting all units' value of \code{var} to one level. (This quantity has several names, including the average potential outcome, average adjusted prediction, and standardized mean). When \code{var} only takes on two levels (or it is supplied as a list and only two values are specified), a contrast between the average marginal means can be computed by supplying an argument to \code{contrast}. Contrasts can be manually computed using \code{\link[=transform]{transform()}} afterward as well.

Continuous calculations involve computing the average of marginal effects of \code{var} across units. A marginal effect is the instantaneous rate of change corresponding to changing a unit's observed value of \code{var} by a tiny amount and considering to what degree the predicted outcome changes. The ratio of the change in the predicted outcome to the change in the value of \code{var} is the marginal effect; these are averaged across the sample to arrive at an average marginal effect. The "tiny amount" used is \code{eps} times the standard deviation of the focal variable.

If unit-level weights are included in the model fit (and discoverable using \code{\link[insight:get_weights]{insight::get_weights()}}), all means will be computed as weighted means.
\subsection{Effect measures}{

The effect measures specified in \code{contrast} are defined below. Typically only \code{"diff"} is appropriate for continuous outcomes and \code{"diff"} or \code{"irr"} are appropriate for count outcomes; the rest are appropriate for binary outcomes. For a focal variable with two levels, \code{0} and \code{1}, and an outcome \code{Y}, the average marginal means will be denoted in the below formulas as \code{E[Y(0)]} and \code{E[Y(1)]}, respectively.\tabular{ll}{
   \code{contrast} \tab Formula \cr
   \code{"diff"} \tab \code{E[Y(1)] - E[Y(0)]} \cr
   \code{"rr"} \tab \code{E[Y(1)] / E[Y(0)]} \cr
   \code{"sr"} \tab \code{(1 - E[Y(1)]) / (1 - E[Y(0)])} \cr
   \code{"srr"} \tab \code{1 - sr} if  \code{E[Y(1)] > E[Y(0)]} \cr
    \tab \code{rr - 1} if \code{E[Y(1)] < E[Y(0)]} \cr
    \tab \code{0} otherwise \cr
   \code{"or"} \tab \code{O[Y(1)] / O[Y(0)]} \cr
    \tab where \code{O[Y(.)]} = \code{E[Y(.)] / (1 - E[Y(.)])} \cr
   \code{"nnt"} \tab \code{1 / (E[Y(1)] - E[Y(0)])} \cr
}


The \code{log(.)} versions are defined by taking the \code{\link[=log]{log()}} (natural log) of the corresponding effect measure.
}
}
\examples{
data("lalonde", package = "MatchIt")

# Fit the model
fit <- glm(I(re78 > 0) ~ treat + age + race +
             married + re74,
           data = lalonde, family = binomial)

# Simulate coefficients
set.seed(123)
s <- sim(fit, n = 100)

# Average marginal effect of `age`
est <- sim_ame(s, var = "age", verbose = FALSE)
summary(est)

# Contrast between average adjusted predictions
# for `treat`
est <- sim_ame(s, var = "treat", contrast = "rr",
               verbose = FALSE)
summary(est)

# Average adjusted predictions for `race`; need to follow up
# with contrasts for specific levels
est <- sim_ame(s, var = "race", verbose = FALSE)

est <- transform(est,
                 `RR(h,b)` = `E[Y(hispan)]` / `E[Y(black)]`)

summary(est)

# Average adjusted predictions for `treat` within levels of
# `married`, first using `subset` and then using `by`
est0 <- sim_ame(s, var = "treat", subset = married == 0,
                contrast = "rd", verbose = FALSE)
names(est0) <- paste0(names(est0), "|married=0")
est1 <- sim_ame(s, var = "treat", subset = married == 1,
                contrast = "rd", verbose = FALSE)
names(est1) <- paste0(names(est1), "|married=1")

summary(cbind(est0, est1))

est <- sim_ame(s, var = "treat", by = ~married,
               contrast = "rd", verbose = FALSE)

est
summary(est)

# Average marginal effect of `re74` within levels of
# married*race
est <- sim_ame(s, var = "age", by = ~married + race,
               verbose = FALSE)
est
summary(est, null = 0)

# Comparing AMEs between married and unmarried for
# each level of `race`
est_diff <- est[4:6] - est[1:3]
names(est_diff) <- paste0("AME_diff|", levels(lalonde$race))
summary(est_diff)

}
\seealso{
\code{\link[=sim_apply]{sim_apply()}}, which provides a general interface to computing any
quantities for simulation-based inference; \code{\link[=plot.clarify_est]{plot.clarify_est()}} for plotting the
output of a call to \code{sim_ame()}; \code{\link[=summary.clarify_est]{summary.clarify_est()}} for computing
p-values and confidence intervals for the estimated quantities.

\code{marginaleffects::marginaleffects()}, \code{marginaleffects::comparisons()}, and \code{margins::margins()} for delta method-based implementations of computing average marginal effects.
}
