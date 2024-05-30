test_that("transform() works", {
  mdata <- readRDS(test_path("fixtures", "mdata.rds"))

  fit <- lm(re78 ~ treat * age + educ + race + re74, data = mdata,
            weights = weights)

  s <- sim(fit, n = 5)

  e0 <- sim_ame(s, "treat", verbose = FALSE)

  e1 <- transform(e0, diff = `E[Y(1)]` - `E[Y(0)]`)

  expect_good_clarify_est(e1)
  expect_equal(length(names(e1)), 3)
  expect_equal(as.matrix(e1)[,2] - as.matrix(e1)[,1],
               as.matrix(e1)[,3])

  #Test positional matching
  e2 <- transform(e0, diff = .b2 - .b1)

  expect_good_clarify_est(e2)

  expect_equal(e1, e2)

  # test that positional matching is prioritized
  e3 <- e0; names(e3) <- c(".b2", ".b1")

  e3 <- transform(e3, diff = .b2 - .b1)
  expect_good_clarify_est(e3)
  expect_equal(e1[3], e3[3])

  #Test that NULL removes existing values but not new ones
  e4 <- transform(e2, diff = NULL)
  expect_good_clarify_est(e4)
  expect_equal(e1[-3], e4)

  e4 <- transform(e0,
                  diff2 = .b1 - .b2,
                  diff2 = NULL)
  expect_good_clarify_est(e4)
  expect_equal(length(names(e4)), 3)
})
