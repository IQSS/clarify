test_that("misim() works with lists of regressions", {
  m <- readRDS(test_path("fixtures", "model_list.rds"))

  s <- misim(m, n = 5)

  expect_good_clarify_misim(s)

  expect_equal(dim(s$sim.coefs), c(5L * length(s$fit), 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- misim(m, n = 5, dist = "t(100)")
  expect_good_clarify_misim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(misim(m, n = 5, dist = "t"))

  set.seed(987)
  s1 <- misim(m, n = 5)
  expect_good_clarify_misim(s1)
  set.seed(987)
  s2 <- misim(m, n = 5)
  expect_good_clarify_misim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- misim(m, n = 5)
  expect_good_clarify_misim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_misim(misim(m, n = 5, vcov = sandwich::vcovHC))
  expect_good_clarify_misim(misim(m, n = 5, vcov = sandwich::vcovHC(m[[1]])))
  expect_good_clarify_misim(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC)))
  expect_good_clarify_misim(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC(m[[i]]))))

  expect_error(misim(m, n = 5, vcov = coef))
  expect_error(misim(m, n = 5, vcov = sandwich::vcovHC(m[[1]])[-2,-2]))
  expect_error(misim(m, n = 5, vcov = lapply(1:9, function(i) sandwich::vcovHC)))
  expect_error(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC(m[[i]])[-2,-2])))
  expect_error(misim(m, n = 5, vcov = lapply(1:9, function(i) sandwich::vcovHC(m[[i]]))))

  #Custom coefs
  expect_good_clarify_misim(misim(m, n = 5, coefs = runif(7)))
  expect_good_clarify_misim(misim(m, n = 5, coefs = lapply(1:10, function(i) runif(7))))
  expect_error(misim(m, n = 5, coefs = runif(8)))
  expect_error(misim(m, n = 5, coefs = lapply(1:10, function(i) runif(8))))
  expect_error(misim(m, n = 5, coefs = lapply(1:9, function(i) runif(7))))
  expect_error(misim(n = 5, coefs = stats::coef))
  expect_error(misim(m, n = 5, coefs = c(NA, runif(6))))

  s <- misim(n = 5, coefs = lapply(m, coef), vcov = lapply(m, vcov))
  expect_good_clarify_misim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("misim() works with mira objects", {
  m <- readRDS(test_path("fixtures", "mira.rds"))

  s <- misim(m, n = 5)

  expect_good_clarify_misim(s)

  expect_equal(dim(s$sim.coefs), c(5L * length(s$fit), 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "normal")

  s <- misim(m, n = 5, dist = "t(100)")
  expect_good_clarify_misim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(misim(m, n = 5, dist = "t"))

  set.seed(987)
  s1 <- misim(m, n = 5)
  expect_good_clarify_misim(s1)
  set.seed(987)
  s2 <- misim(m, n = 5)
  expect_good_clarify_misim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- misim(m, n = 5)
  expect_good_clarify_misim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_misim(misim(m, n = 5, vcov = sandwich::vcovHC))
  expect_good_clarify_misim(misim(m, n = 5, vcov = sandwich::vcovHC(m$analyses[[1]])))
  expect_good_clarify_misim(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC)))
  expect_good_clarify_misim(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC(m$analyses[[i]]))))

  expect_error(misim(m, n = 5, vcov = coef))
  expect_error(misim(m, n = 5, vcov = sandwich::vcovHC(m$analyses[[1]])[-2,-2]))
  expect_error(misim(m, n = 5, vcov = lapply(1:9, function(i) sandwich::vcovHC)))
  expect_error(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC(m$analyses[[i]])[-2,-2])))
  expect_error(misim(m, n = 5, vcov = lapply(1:9, function(i) sandwich::vcovHC(m$analyses[[i]]))))

  #Custom coefs
  expect_good_clarify_misim(misim(m, n = 5, coefs = runif(7)))
  expect_good_clarify_misim(misim(m, n = 5, coefs = lapply(1:10, function(i) runif(7))))
  expect_error(misim(m, n = 5, coefs = runif(8)))
  expect_error(misim(m, n = 5, coefs = lapply(1:10, function(i) runif(8))))
  expect_error(misim(m, n = 5, coefs = lapply(1:9, function(i) runif(7))))
  expect_error(misim(n = 5, coefs = stats::coef))
  expect_error(misim(m, n = 5, coefs = c(NA, runif(6))))

  s <- misim(n = 5, coefs = lapply(m$analyses, coef), vcov = lapply(m$analyses, vcov))
  expect_good_clarify_misim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("misim() works with mimira objects", {
  m <- readRDS(test_path("fixtures", "mimira.rds"))

  s <- misim(m, n = 5)

  expect_good_clarify_misim(s)

  expect_equal(dim(s$sim.coefs), c(5L * length(s$fit), 7L))
  expect_equal(colnames(s$sim.coefs),
               c("(Intercept)", "treat", "age", "educ", "racehispan", "racewhite", "re74"))
  expect_equal(attr(s, "dist"), "t(607)")

  s <- misim(m, n = 5, dist = "t(100)")
  expect_good_clarify_misim(s)
  expect_equal(attr(s, "dist"), "t(100)")

  expect_error(misim(m, n = 5, dist = "t"))

  set.seed(987)
  s1 <- misim(m, n = 5)
  expect_good_clarify_misim(s1)
  set.seed(987)
  s2 <- misim(m, n = 5)
  expect_good_clarify_misim(s2)

  expect_identical(s1$sim.coefs, s2$sim.coefs)

  #Different seed
  set.seed(123)
  s3 <- misim(m, n = 5)
  expect_good_clarify_misim(s3)
  expect_false(identical(s1$sim.coefs, s3$sim.coefs))

  #Using custom variances
  expect_good_clarify_misim(misim(m, n = 5, vcov = sandwich::vcovHC))
  expect_good_clarify_misim(misim(m, n = 5, vcov = sandwich::vcovHC(m$analyses[[1]])))
  expect_good_clarify_misim(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC)))
  expect_good_clarify_misim(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC(m$analyses[[i]]))))

  expect_error(misim(m, n = 5, vcov = coef))
  expect_error(misim(m, n = 5, vcov = sandwich::vcovHC(m$analyses[[1]])[-2,-2]))
  expect_error(misim(m, n = 5, vcov = lapply(1:9, function(i) sandwich::vcovHC)))
  expect_error(misim(m, n = 5, vcov = lapply(1:10, function(i) sandwich::vcovHC(m$analyses[[i]])[-2,-2])))
  expect_error(misim(m, n = 5, vcov = lapply(1:9, function(i) sandwich::vcovHC(m$analyses[[i]]))))

  #Custom coefs
  expect_good_clarify_misim(misim(m, n = 5, coefs = runif(7)))
  expect_good_clarify_misim(misim(m, n = 5, coefs = lapply(1:10, function(i) runif(7))))
  expect_error(misim(m, n = 5, coefs = runif(8)))
  expect_error(misim(m, n = 5, coefs = lapply(1:10, function(i) runif(8))))
  expect_error(misim(m, n = 5, coefs = lapply(1:9, function(i) runif(7))))
  expect_error(misim(n = 5, coefs = stats::coef))
  expect_error(misim(m, n = 5, coefs = c(NA, runif(6))))

  s <- misim(n = 5, coefs = lapply(m$analyses, coef),
             vcov = lapply(m$analyses, sandwich::vcovHC))
  expect_good_clarify_misim(s)
  expect_false(attr(s, "use_fit"))
})

test_that("sim_ame() works with misim() and glm()", {
  m <- readRDS(test_path("fixtures", "model_list.rds"))

  s <- misim(m, n = 5)

  e <- sim_ame(s, "treat", verbose = FALSE)

  expect_good_clarify_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 2)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]"))
  expect_identical(attr(e, "var"), "treat")

  e <- sim_ame(s, "treat", contrast = "log(rr)", verbose = FALSE)

  expect_good_clarify_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(0)]", "E[Y(1)]", "log(RR)"))
  expect_identical(attr(e, "var"), "treat")

  expect_warning(sim_ame(s, "race", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "race", verbose = FALSE)
  expect_good_clarify_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(hispan)]", "E[Y(white)]"))
  expect_identical(attr(e, "var"), "race")

  e <- sim_ame(s, list(race = c("black", "white")), contrast = "nnt", verbose = FALSE)
  expect_good_clarify_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 3)
  expect_identical(names(e), c("E[Y(black)]", "E[Y(white)]", "NNT"))
  expect_identical(attr(e, "var"), "race")

  #Continuous variable
  e <- sim_ame(s, "age", verbose = FALSE)
  expect_good_clarify_est(e)
  expect_equal(nrow(e), nrow(s$sim.coefs))
  expect_equal(attr(e, "sim_hash"), attr(s, "sim_hash"))
  expect_equal(ncol(e), 1)
  expect_identical(names(e), "dY/d(age)")
  expect_identical(attr(e, "var"), "age")

  expect_warning(sim_ame(s, "age", contrast = "diff", verbose = FALSE),
                 "`contrast` is ignored")

  e <- sim_ame(s, "age", subset = treat == 1, verbose = FALSE)
  expect_good_clarify_est(e)

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
