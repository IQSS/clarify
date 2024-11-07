op <- options(width = 10000)

withr::defer(options(op), testthat::teardown_env())
