
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simbased

<!-- badges: start -->
<!-- badges: end -->

`simbased` implements simulation-based inference as an alternative to
the delta method for computing functions of model parameters, such as
marginal effects.

## Installation

You can install the development version of simbased from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ngreifer/simbased")
```

## Example

Below is an example of performing g-computation for the ATT after
logistic regression to compute the marginal risk ratio and its
confidence interval:

``` r
library(simbased)

data("lalonde", package = "MatchIt")

fit <- glm(I(re78 == 0) ~ treat * (age + educ + race + married + nodegree + re74 + re75),
           data = lalonde, family = binomial)

set.seed(123)
sim_coefs <- sim(fit)

sim_est <- sim_apply(sim_coefs, function(fit) {
  d <- subset(lalonde, treat == 1)
  d$treat <- 1
  p1 <- mean(predict(fit, newdata = d, type = "response"))
  d$treat <- 0
  p0 <- mean(predict(fit, newdata = d, type = "response"))
  c(RR = p1 / p0)
}, verbose = FALSE)

summary(sim_est)
#>    Estimate CI 2.5 % CI 97.5 %
#> RR    0.826    0.641      1.41
sim_plot(sim_est)
```

<img src="man/figures/README-example-1.png" width="50%" />

We could have used `marginaleffects`, which uses the delta method
instead:

``` r
marginaleffects::comparisons(fit, variables = list(treat = 0:1),
                             newdata = subset(lalonde, treat == 1),
                             transform_pre = "lnratioavg") |>
  summary(transform_avg = exp)
#> Average contrasts 
#>                   treat Effect Pr(>|z|)  2.5 % 97.5 %
#> 1 ln(mean(1) / mean(0)) 0.8261  0.32119 0.5664  1.205
#> 
#> Model type:  glm 
#> Prediction type:  response 
#> Average-transformation:
```

The simulation-based confidence intervals are a bit wider, reflecting
that the delta method may be a poor approximation.
