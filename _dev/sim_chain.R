# # Function to chain simulations, i.e., to simulate values within each simulation. Intended use is for
# # using outputs of first stage, which are estimated with uncertainty, in second stage. Turned out
# # not to give valid results when tested with propensity score weighting.
# sim_chain <- function(sim, FUN, n = 10, vcov = NULL, coefs = NULL, dist = NULL, verbose = TRUE,
#                       cl = NULL, ...) {
#   coef_template <- get_coef_template(sim$fit, sim$coefs)
#   coef_location <- get_coef_location(sim$fit, sim$coefs, coef_template)
#
#   opb <- pbapply::pboptions(type = if (verbose) "timer" else "none")
#   on.exit(pbapply::pboptions(opb))
#
#   apply_FUN <- make_apply_FUN(FUN, coef_location, coef_template)
#
#   # Test apply_FUN() on original model coefficients
#   test <- try(apply_FUN(fit = sim$fit, coefs = sim$coefs, ...), silent = TRUE)
#   if (is_error(test)) {
#     .err("`FUN` failed to run on an initial check with the following error:\n",
#          conditionMessage(attr(test, "condition")))
#   }
#   test_sim <- sim(test, n = 1, vcov = vcov, coefs = coefs, dist = dist)
#
#   if (is.null(names(test))) names(test) <- paste0("est", seq_along(test))
#
#   sim.list <- pbapply::pblapply(seq_len(nrow(sim$sim.coefs)), function(i) {
#     sim(apply_FUN(fit = sim$fit, coefs = sim$sim.coefs[i,], ...),
#         n = n, vcov = vcov, coefs = coefs, dist = dist)
#   }, cl = cl)
#
#   out <- list(sim.coefs = do.call("rbind", lapply(sim.list, `[[`, "sim.coefs")),
#               coefs = test_sim$coefs,
#               fit = test)
#
#   attr(out, "dist") <- attr(test_sim, "dist")
#   attr(out, "use_fit") <- TRUE
#   attr(out, "sim_hash") <- rlang::hash(out$sim.coefs)
#   class(out) <- "simbased_sim"
#
#   out
# }
