test_that("sim() works with coefs and vcov", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- lm(re78 ~ treat + age + educ + race + re74, data = mdata,
            weights = weights)

  s <- sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
           n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")
  expect_false(attr(s, "use_fit"))

  s <- sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
           n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
                   n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
            n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
            n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
            n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  expect_error(sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit)[-2,-2],
               n = 5))
  expect_error(sim(coefs = coef(fit)[-1], vcov = sandwich::vcovHC(fit),
                   n = 5))
  expect_error(sim(coefs = c(NA, coef(fit)[-1]), vcov = sandwich::vcovHC(fit),
                   n = 5))
})

test_that("sim() works with lm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- lm(re78 ~ treat + age + educ + race + re74, data = mdata,
            weights = weights)

  p <- length(coef(fit))

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, p))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC3"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(p)))
  expect_error(sim(fit, n = 5, coefs = runif(p+1)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(p-1))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with glm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- glm(binY ~ treat + age + educ + race + re74, data = mdata,
             family = binomial)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC3"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with MASS::glm.nb()", {
  skip_if_not_installed("MASS")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- MASS::glm.nb(countY ~ treat + age + educ + race + re74, data = mdata,
                      weights = weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC3"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with betareg::betareg()", {
  skip_if_not_installed("betareg")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- betareg::betareg(propY ~ treat + age + educ + race + re74 | treat + age,
                          data = mdata, weights = weights)

  if (length(marginaleffects::get_coef(fit)) != 10L) {
    skip("disagreement between `marginaleffects` and `clarify` treatment of `betareg` obejcts")
  }

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 10L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite",
                 "re74", "(phi)_(Intercept)", "(phi)_treat", "(phi)_age"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(10)))
  expect_error(sim(fit, n = 5, coefs = runif(11)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(9))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with survey::svyglm()", {
  skip_if_not_installed("survey")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- survey::svyglm(binY ~ treat + age + educ + race + re74, family = quasibinomial,
                        design = survey::svydesign(ids = ~subclass, weights = ~weights, data = mdata))

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(92)")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC3"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = mdata$subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with estimatr::lm_robust()", {
  skip_if_not_installed("estimatr")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- estimatr::lm_robust(re78 ~ treat + age + educ + race + re74, data = mdata,
                             weights = weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov(fit)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with estimatr::iv_robust()", {
  skip_if_not_installed("estimatr")
  # skip("iv_robust() has errors; not yet fully supported")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- estimatr::iv_robust(re78 ~ treat + age + race | educ + age + race, data = mdata,
                             weights = weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 5L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "racehispan", "racewhite"))
  expect_equal(attr(s, "dist"), "t(573)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  p <- length(coef(fit))
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(p)))
  expect_error(sim(fit, n = 5, coefs = runif(p + 1)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(p - 1))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with fixest::feols()", {
  skip_if_not_installed("fixest")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- fixest::feols(re78 ~ treat + age + educ + race + re74, data = mdata,
                       weights = ~weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC1"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = mdata["subclass"])))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with fixest::feglm()", {
  skip_if_not_installed("fixest")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- fixest::feglm(binY ~ treat + age + educ + race + re74, data = mdata,
                       weights = ~weights, family = quasibinomial)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC1"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = mdata["subclass"])))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with logistf::logistf()", {
  skip_if_not_installed("logistf")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- logistf::logistf(binY ~ treat + age + educ + race + re74, data = mdata,
                           weights = weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov(fit)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with geepack::geeglm()", {
  skip_if_not_installed("geepack")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  suppressWarnings({
    fit <- geepack::geeglm(binY ~ treat + age + educ + race + re74,
                           data = mdata[order(mdata$subclass),],
                           weights = weights, family = binomial,
                           id = subclass)
  })
  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov(fit)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with rms::ols()", {
  skip_if_not_installed("rms")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  suppressMessages(library(rms))

  dd <<- suppressWarnings(datadist(mdata))
  op <- options(datadist = "dd")

  fit <- ols(re78 ~ treat + pol(age, 3) +
               educ + catg(race) + rcs(re74, 3), data = mdata,
             penalty = 3)
  k <- 10L #length(coef(fit))

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, k))
  expect_equal(colnames(s$sim.coefs),
               c("Intercept", "treat", "age", "age^2", "age^3", "educ", "race=hispan",
                 "race=white", "re74", "re74'"))
  expect_equal(attr(s, "dist"), "t(569.230711076445)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov))
  # expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(k)))
  expect_error(sim(fit, n = 5, coefs = runif(k + 1)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(k - 1))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))

  options(op); rm(dd, envir = globalenv())
  unloadNamespace("rms")
})

test_that("sim() works with rms::lrm()", {
  skip_if_not_installed("rms")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  suppressMessages(library(rms))

  dd <<- suppressWarnings(datadist(mdata))
  op <- options(datadist = "dd")

  fit <- lrm(binY ~ treat + pol(age, 3) +
               educ + catg(race) + rcs(re74, 3), data = mdata,
             penalty = 3)

  k <- 10L #length(coef(fit))

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, k))
  expect_equal(colnames(s$sim.coefs),
               c("Intercept", "treat", "age", "age^2", "age^3", "educ", "race=hispan",
                 "race=white", "re74", "re74'"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov(fit)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(k)))
  expect_error(sim(fit, n = 5, coefs = runif(k + 1)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(k - 1))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with robustbase::lmrob()", {
  skip_if_not_installed("robustbase")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robustbase::lmrob(re78 ~ treat + age + educ + race + re74, data = mdata,
                           weights = weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov))
  # expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with robustbase::glmrob()", {
  skip_if_not_installed("robustbase")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robustbase::glmrob(binY ~ treat + age + educ + race + re74, data = mdata,
                            # weights = weights, #Can't accept non-integer weights
                            family = binomial)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = vcov(fit)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with robust::lmRob()", {
  skip_if_not_installed("robust")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robust::lmRob(re78 ~ treat + age + educ + race + re74, data = mdata,
                           weights = weights)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = insight:::get_varcov))
  # expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = insight:::get_varcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = insight:::get_varcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with robust::glmRob()", {
  skip_if_not_installed("robust")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robust::glmRob(binY ~ treat + age + educ + I(educ^2) + I(race=="black") + re74, data = mdata,
                        weights = weights,
                        family = binomial)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "I(educ^2)", "I(race == \"black\")TRUE", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = insight:::get_varcov))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = insight:::get_varcov(fit)))
  expect_error(sim(fit, n = 5, vcov = insight:::get_varcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = insight:::get_varcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with AER::tobit()", {
  skip_if_not_installed("AER")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- AER::tobit(re78 ~ treat + age + educ + race + re74, data = mdata,
                    weights = weights, left = 0)

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  # expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)[-7,-7]))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-(2:3),-(2:3)]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(7)))
  expect_error(sim(fit, n = 5, coefs = runif(8)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(6))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit)[-7,-7])
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with ivreg::ivreg()", {
  skip_if_not_installed("ivreg")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- ivreg::ivreg(re78 ~ treat + age + race | educ + age + race, data = mdata,
                             weights = weights)

  p <- length(coef(fit))

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, p))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "racehispan", "racewhite"))
  expect_equal(attr(s, "dist"), "t(573)")

  s <- sim(fit, n = 5, dist = "norm")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "normal")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_sim(sim(fit, n = 5, vcov = "HC3"))
  expect_good_clarify_sim(sim(fit, n = 5, vcov = sandwich::vcovCL(fit, cluster = ~subclass)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs

  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(p)))
  expect_error(sim(fit, n = 5, coefs = runif(p + 1)))
  expect_error(sim(n = 5, coefs = stats::coef))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(p - 1))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim() works with mgcv::gam()", {
  skip_if_not_installed("mgcv")
  # skip("mgcv::gam() not ready yet")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- mgcv::gam(binY ~ treat + age + educ + race + re74, data = mdata,
             weights = weights, family = quasibinomial)

  p <- length(coef(fit))

  s <- sim(fit, n = 5)

  expect_good_clarify_sim(s)

  expect_equal(dim(s$sim.coefs), c(5L, p))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(571)")

  s <- sim(fit, n = 5, dist = "t(100)")
  expect_good_clarify_sim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(sim(fit, n = 5, dist = "t"))

  set.seed(987)
  s1 <- sim(fit, n = 5)
  expect_good_clarify_sim(s1)
  set.seed(987)
  s2 <- sim(fit, n = 5)
  expect_good_clarify_sim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- sim(fit, n = 5)
  expect_good_clarify_sim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  if (packageVersion("marginaleffects") > '0.8.1')
    expect_good_clarify_sim(sim(fit, n = 5, vcov = function(...) vcov(..., sandwich = TRUE)))
  expect_error(sim(fit, n = 5, vcov = vcov(fit)[-2,-2]))

  #Custom coefs
  expect_good_clarify_sim(sim(fit, n = 5, coefs = runif(p)))
  expect_error(sim(fit, n = 5, coefs = runif(p+1)))
  expect_error(sim(fit, n = 5, coefs = c(NA, runif(p-1))))

  s <- sim(n = 5, coefs = coef(fit), vcov = vcov(fit))
  expect_good_clarify_sim(s)
  expect_false(attr(s, "use_fit"))
})
