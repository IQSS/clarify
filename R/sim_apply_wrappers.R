# sim_comparisons <- function(sim, verbose = TRUE, cl = NULL, ...) {
#
#   args <- alist(...)
#   args[["vcov"]] <- FALSE
#
#   FUN <- function(fit) {
#     comp <- do.call(marginaleffects::comparisons, c(list(fit), args))
#     summ <- summary(comp)
#
#     estimate <- summ[["estimate"]]
#     contrast <- summ[[names(summ)[startsWith(names(summ), "contrast")][1]]]
#     by_vars <- setdiff(names(summ), c("estimate", names(summ)[startsWith(names(summ), "contrast")],
#                                       "type", "term"))
#
#     setNames(estimate, do.call("paste", c(lapply(by_vars, function(i) {
#       paste0(i, " = ", add_quotes(summ[[i]], chk::vld_character_or_factor(summ[[i]])))
#     }), list(contrast, sep = ", "))))
#   }
# # FUN(sim$fit)
#   sim_apply(sim, FUN = FUN, verbose = verbose, cl = cl)
# }
