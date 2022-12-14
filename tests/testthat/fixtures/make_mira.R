d <- cobalt::lalonde_mis
d$binY <- as.numeric(d$re78 > 0)

imp <- mice::mice(d, maxit = 5, m = 10, printFlag = FALSE,
                  seed = 1234567)

#mipo:
mira <- with(imp, glm(binY ~ treat + age + educ + race + re74, family = binomial))
saveRDS(mira, test_path("fixtures", "mira.rds"))

#list of models:
model_list <- lapply(mice::complete(imp, "all"), function(data) {
  glm(binY ~ treat + age + educ + race + re74, family = binomial,
      data = data)
})
saveRDS(model_list, test_path("fixtures", "model_list.rds"))

m <- MatchThem::matchthem(treat ~ age + educ + race + married + re74 + re75,
                          imp, estimand = "ATE", method = "full", link = "probit")

#mimipo
mimira <- with(m, glm(binY ~ treat + age + educ + race + re74,
                      family = "quasibinomial"))

saveRDS(mimira, test_path("fixtures", "mimira.rds"))
