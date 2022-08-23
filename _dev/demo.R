data("lalonde", package = "MatchIt")

fit <- lm(re78 ~ treat + age + educ + married + race + re74,
          data = lalonde)

# Simulate coefficients
set.seed(123)
s <- sim(fit, n = 100)

# Predicted values at specified values of treat, typical
# values for other predictors
est <- sim_setx(s, x = list(treat = 0:1,
                            re74 = c(0, 10000)),
                verbose = FALSE)
summary(est)
setx_plot(est)

# Predicted values at specified grid of values, typical
# values for other predictors
est <- sim_setx(s, x = list(age = c(20, 25, 30, 35),
                            married = 0:1),
                verbose = FALSE)
summary(est)
setx_plot(est)

# First differences of treat at specified value of
# race, typical values for other predictors
est <- sim_setx(s, x = list(treat = 0, race = "hispan"),
                x1 = list(treat = 1, race = "hispan"),
                verbose = FALSE)
summary(est)
setx_plot(est)
