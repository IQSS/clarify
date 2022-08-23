data("lalonde", package = "MatchIt")
library(simbased)

lalonde$re78_0 <- as.numeric(lalonde$re78 == 0)

fit <- glm(re78_0 ~ treat + age + educ + married + race + re74 + re75,
           data = lalonde, family = binomial)

#Simulate coefficients
set.seed(1234)
s <- sim(fit, n = 500)

s

#An arbitrary quantity: difference between two regression coefs
sim_fun <- function(fit) {
  b <- coef(fit)

  c(ORR = unname(exp(b["racehispan"]) / exp(b["racewhite"])))
}

#Apply this function to each set of simulated coefficients
sim_est <- sim_apply(s, sim_fun)

sim_est

#Plot the resulting sampling distribution
sim_plot(sim_est)

#View the estimate, confidence interval, and p-value
summary(sim_est, null = 1)

#Assess different quantities
summary(transform(sim_est,
                  logORR = log(ORR)),
        null = c(1, 0))

#Estimate average marginal effects using sim_ame(),
#a wrapper for sim_apply()
ame <- sim_ame(s, var = "treat", contrast = "RR",
               subset = treat == 1)

ame

summary(ame)

sim_plot(ame, est = "RR")

summary(transform(ame, RD = `E[Y(1)]` - `E[Y(0)]`),
        null = c(NA, NA, 1, 0))

#Estimate predictions at representative values using sim_setx()
repv <- sim_setx(s, x = list(treat = 0:1))
summary(repv)

#First differences
fd <- sim_setx(s, x = list(treat = 0), x1 = list(treat = 1))
summary(fd)

#Predictions across levels of a continuous variable
repv <- sim_setx(s, x = list(treat = 0:1, re75 = seq(0, 10000, by = 500)))
setx_plot(repv)
