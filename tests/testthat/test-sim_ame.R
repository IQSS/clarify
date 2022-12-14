test_that("sim_ame() doesn't work with coefs and vcov", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- lm(re78 ~ treat + age + educ + race + re74, data = mdata,
            weights = weights)

  s <- sim(coefs = coef(fit), vcov = sandwich::vcovHC(fit),
           n = 5)

  expect_error(sim_ame(s, "treat", verbose = FALSE),
               "when a model fit was supplied")
})

test_that("sim_ame() works with lm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- lm(re78 ~ treat + age + educ + race + re74, data = mdata,
            weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with glm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- glm(binY ~ treat + age + educ + race + re74, data = mdata,
             weights = weights, family = quasibinomial)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with MASS::glm.nb()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- MASS::glm.nb(countY ~ treat + age + educ + race + re74, data = mdata,
                      weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with betareg::betareg()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- betareg::betareg(propY ~ treat + age + educ + race + re74 | treat + age,
                          data = mdata, weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with survey::svyglm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- survey::svyglm(binY ~ treat + age + educ + race + re74, family = quasibinomial,
                        design = survey::svydesign(ids = ~subclass, weights = ~weights, data = mdata))

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with estimatr::lm_robust()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- estimatr::lm_robust(re78 ~ treat + age + educ + race + re74, data = mdata,
                             weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with estimatr::iv_robust()", {
  skip("'estimatr' has problems with model.frame.iv_robust(), which causes an error with insight::get_data().")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- estimatr::iv_robust(re78 ~ treat + age + race | educ + age + race, data = mdata,
                             weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with fixest::feols()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- fixest::feols(re78 ~ treat + age + educ + race + re74, data = mdata,
                       weights = ~weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with fixest::feglm()", {
  # skip("'fixest' has problems with scoping. Can't test until fixed.")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- fixest::feglm(binY ~ treat + age + educ + race + re74, data = mdata,
                       weights = ~weights, family = quasibinomial)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with logistf::logistf()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- logistf::logistf(binY ~ treat + age + educ + race + re74, data = mdata,
                          weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with geepack::geeglm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  suppressWarnings({
    fit <- geepack::geeglm(binY ~ treat + age + educ + race + re74,
                           data = mdata[order(mdata$subclass),],
                           weights = weights, family = binomial,
                           id = subclass)
  })

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with rms::ols()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  suppressMessages(library(rms))

  dd <<- suppressWarnings(datadist(mdata))
  op <- options(datadist = "dd")

  fit <- ols(re78 ~ treat + pol(age, 3) +
               educ + catg(race) + rcs(re74, 4), data = mdata,
             penalty = 3)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

  options(op); rm(dd, envir = globalenv())
  unloadNamespace("rms")
})

test_that("sim_ame() works with rms::lrm()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  suppressMessages(library(rms))

  dd <<- suppressWarnings(datadist(mdata))
  op <- options(datadist = "dd")

  fit <- lrm(binY ~ treat + pol(age, 3) +
               educ + catg(race) + rcs(re74, 3), data = mdata,
             penalty = 3)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

  options(op); rm(dd, envir = globalenv())
  unloadNamespace("rms")
})

test_that("sim_ame() works with robustbase::lmrob()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robustbase::lmrob(re78 ~ treat + age + educ + race + re74, data = mdata,
                           weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with robustbase::glmrob()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robustbase::glmrob(binY ~ treat + age + educ + race + re74, data = mdata,
                            # weights = weights, #Can't accept non-integer weights
                            family = binomial)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with robust::lmRob()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robust::lmRob(re78 ~ treat + age + educ + race + re74, data = mdata,
                       weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with robust::glmRob()", {
  skip("predict() is bugged for glmRob()")
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- robust::glmRob(binY ~ treat + age + educ + I(educ^2) + I(race=="black") + re74,
                        data = mdata,
                        weights = weights,
                        family = binomial)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with AER::tobit()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- AER::tobit(re78 ~ treat + age + educ + race + re74, data = mdata,
                    weights = weights, left = 0)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})

test_that("sim_ame() works with ivreg::ivreg()", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- ivreg::ivreg(re78 ~ treat + age + race | educ + age + race, data = mdata,
                      weights = weights)

  s <- sim(fit, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "diff", verbose = FALSE)

  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "Diff"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "diff", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "Diff"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_simbased_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_simbased_est(e)

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  #Bad args
  expect_error(sim_ame(s, list(race = c("black", "whiteAAA")), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(race = 1:2), verbose = FALSE),
               "values mentioned in")
  expect_error(sim_ame(s, list(raceAAA = 1:2), verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, "raceAAA", verbose = FALSE),
               "not present in the original model")
  expect_error(sim_ame(s, c("race", "treat"), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(race = "black", treat = 0:1), verbose = FALSE),
               "desired focal variable or a named list")
  expect_error(sim_ame(s, list(0:1), verbose = FALSE),
               "desired focal variable or a named list")

})
