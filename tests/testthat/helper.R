#testthat helpers

expect_good_simbased_sim <- function(s) {
  expect_s3_class(s, "simbased_sim")
  expect_true(all(c("sim.coefs", "coefs", "vcov") %in% names(s)))

  expect_true(is.matrix(s$sim.coefs))
  expect_type(s$sim.coefs, "double")
  expect_type(s$coefs, "double")
  expect_true(is.matrix(s$vcov))
  expect_type(s$vcov, "double")

  expect_vector(attr(s, "dist"), character(), 1)
  expect_vector(attr(s, "sim_hash"), character(), 1)
  expect_vector(attr(s, "use_fit"), logical(), 1)

  expect_equal(isTRUE(!is.null(s$fit)), attr(s, "use_fit"))

  expect_equal(length(s$coefs), ncol(s$sim.coefs))
  expect_equal(ncol(s$vcov), nrow(s$vcov))
  expect_equal(length(s$coefs), nrow(s$vcov))

  expect_false(any(!is.finite(s$sim.coefs)))
  expect_false(any(!is.finite(s$coefs)))
  expect_false(any(!is.finite(s$vcov)))
}

expect_good_simbased_est <- function(e) {
  expect_s3_class(e, "simbased_est")
  expect_length(dim(e), 2L)
  expect_type(e, "double")

  expect_vector(attr(e, "original"), numeric(), ncol(e))
  expect_vector(attr(e, "sim_hash"), character(), 1)

  expect_identical(names(e), names(attr(e, "original")))

  expect_false(any(apply(e, 2, all_the_same)))
}

expect_good_simbased_misim <- function(s) {
  expect_s3_class(s, "simbased_misim")
  expect_s3_class(s, "simbased_sim")
  expect_true(all(c("sim.coefs", "coefs", "imp") %in% names(s)))

  expect_true(is.matrix(s$sim.coefs))
  expect_type(s$sim.coefs, "double")
  expect_true(is.matrix(s$coefs))
  expect_type(s$coefs, "double")
  if (!is.null(s$fit)) expect_equal(nrow(s$coefs), length(s$fit))
  expect_type(s$imp, "integer")
  expect_equal(max(s$imp), nrow(s$coefs))
  expect_equal(length(s$imp), nrow(s$sim.coefs))

  expect_vector(attr(s, "dist"), character(), 1)
  expect_vector(attr(s, "sim_hash"), character(), 1)
  expect_vector(attr(s, "use_fit"), logical(), 1)

  expect_equal(isTRUE(!is.null(s$fit)), attr(s, "use_fit"))

  expect_equal(ncol(s$coefs), ncol(s$sim.coefs))

  expect_false(any(!is.finite(s$sim.coefs)))
  expect_false(any(!is.finite(s$coefs)))
}
