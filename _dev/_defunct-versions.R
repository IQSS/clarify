# #Defunct functions that have been replaced by versions that rely on `marginaleffects`
# get_coefs <- function(fit) {
#
#   b <- try(coef(fit), silent = TRUE)
#
#   if (!is_error(b)) {
#     b <- try(transform_coefs(b), silent = TRUE)
#   }
#
#   if (!check_valid_coef(b)) {
#     if (insight::is_model_supported(fit)) {
#       b <- insight::get_parameters(fit, summary = TRUE, component = "all")
#       b <- try(transform_coefs(b, insight_used = TRUE), silent = TRUE)
#     }
#   }
#
#   if (!check_valid_coef(b)) {
#     .err("`sim()` was unable to extract a valid set of coefficients from the model fit; please supply coefficients to the `coefs` argument and a covariance matrix to the `vcov` argument")
#   }
#
#   return(b)
# }
#
# get_vcov <- function(fit) {
#   check <- TRUE
#   if (!inherits(fit, "tobit") &&
#       check_valid_vcov(v <- try(vcov(fit), silent = TRUE))) {
#     check <- FALSE
#     #v already extracted
#   }
#   else if (insight::is_model_supported(fit)) {
#     v <- try(insight::get_varcov(fit, component = "all"), silent = TRUE)
#   }
#
#   if (check && !check_valid_vcov(v)) {
#     .err("`sim()` was unable to extract a valid covariance matrix from the model fit; please supply a covariance matrix to the `vcov` argument")
#   }
#
#   return(v)
# }
#
# make_apply_FUN <- function(FUN, coef_location = NULL, template = NULL) {
#   if (is.null(coef_location) && isTRUE(attr(FUN, "use_fit"))) {
#     if (isTRUE(attr(FUN, "use_coefs"))) {
#       .err("the location of the coefficients in the model object cannot found, so `FUN` cannot have a `fit` argument")
#     }
#     else {
#       .err("the location of the coefficients in the model object cannot found, so `FUN` must have a `coefs` argument")
#     }
#   }
#
#   if (isTRUE(attr(FUN, "use_coefs")) && isTRUE(attr(FUN, "use_fit"))) {
#     apply_FUN <- function(fit, coefs, ...) {
#       fit <- coef_assign(fit, coefs, coef_location, template)
#       FUN(fit = fit, coefs = coefs, ...)
#     }
#   }
#   else if (isTRUE(attr(FUN, "use_coefs"))) {
#     apply_FUN <- function(fit, coefs, ...) {
#       FUN(coefs = coefs, ...)
#     }
#   }
#   else if (isTRUE(attr(FUN, "use_fit"))) {
#     apply_FUN <- function(fit, coefs, ...) {
#       fit <- coef_assign(fit, coefs, coef_location, template)
#       FUN(fit = fit, ...)
#     }
#   }
#   else {
#
#     apply_FUN <- function(fit, coefs, ...) {
#       fit <- coef_assign(fit, coefs, coef_location, template)
#       FUN(fit, ...)
#     }
#   }
#
#   return(apply_FUN)
# }
#
# coef_assign <- function(fit, coefs, coef_location, template) {
#
#   fit[[coef_location]] <- untransform_coefs(coefs, template)
#
#   return(fit)
# }
#
# untransform_coefs <- function(b, template = NULL) {
#
#   if (is.null(template)) return(b)
#
#   if (is.matrix(template)) {
#     coefs <- array(unname(b), dim = dim(template),
#                    dimnames = dimnames(template))
#   }
#   else if (is.list(template)) {
#     coefs <- template
#     for (i in seq_along(coefs)) {
#       coefs[[i]][] <- unname(b[template[[i]]])
#     }
#   }
#
#   return(coefs)
# }
#
# transform_coefs <- function(coefs, insight_used = FALSE) {
#
#   #Transform coefs
#   if (insight_used) {
#     if ("Response" %in% names(coefs)) {
#       b <- setNames(unname(coefs$Estimate), paste(coefs$Response, coefs$Paramater, sep = ":"))
#     }
#     else {
#       b <- setNames(unname(coefs$Estimate), coefs$Parameter)
#     }
#   }
#   else if (is.matrix(coefs)) {
#     b <- unlist(lapply(colnames(coefs), function(i) {
#       setNames(b[,i], paste(i, rownames(b), sep = ":"))
#     }))
#   }
#   else {
#     b <- coefs
#   }
#
#   return(b)
# }
#
# simbased_predict <- function(x, newdata = NULL, index = NULL) {
#   if (inherits_any(x, c("lm", "glm", "betareg", "lmrob", "lmRob", "glmRob", "ivreg")) &&
#       !inherits_any(x, c("svyglm", "rms"))) {
#     p <- setNames(predict(x, newdata = newdata, type = "response"),
#                   rownames(newdata))
#   }
#   else if (inherits(x, "ols") && inherits(x, "rms")) {
#     p <- setNames(predict(x, newdata = newdata, type = "lp"),
#                   rownames(newdata))
#   }
#   else if (inherits(x, "lrm") && inherits(x, "rms")) {
#     p <- setNames(predict(x, newdata = newdata, type = "fitted"),
#                   rownames(newdata))
#   }
#   else if (insight::is_model_supported(x)) {
#     p <- try(setNames(as.numeric(insight::get_predicted(x, data = newdata, predict = "expectation",
#                                                         verbose = FALSE)),
#                       rownames(newdata)), silent = TRUE)
#     if (is_error(p)) {
#       .err("predicted values could not be extracted from the model")
#     }
#   }
#   else {
#     p <- try(predict(x, newdata = newdata, type = "response"), silent = TRUE)
#     if (length(p) == 0L || is_error(p)) {
#       .err("predicted values could not be extracted from the model")
#     }
#   }
#
#   if (!is.null(dim(p))) {
#     if (length(dim(p) == 1L)) {
#       p <- setNames(as.numeric(p), dimnames(p)[[1]])
#     }
#     else if (any(dim(p) == 1L)) {
#       p <- setNames(as.numeric(drop(p)), dimnames(p)[[which(dim(p) == 1)]])
#     }
#   }
#
#   return(p)
# }
#
# is_linear_model <- function(model) {
#   identical(class(model), "lm") ||
#     (inherits(model, "glm") && !is.null(model$family) &&
#        identical(model$family$family, "gaussian") &&
#        identical(model$family$link, "identity")) ||
#     (inherits(model, "fixest") &&
#        (identical(model$method, "feols")) ||
#        (identical(model$method, "feglm") && !is.null(model$family) &&
#           (identical(model$family$family, "gaussian") &&
#              identical(model$family$link, "identity"))) ||
#        (identical(model$method, "femlm") && !is.null(model$family) &&
#           (identical(model$family, "gaussian")))) ||
#     inherits(model, "lm_robust") ||
#     inherits(model, "iv_robust") ||
#     (inherits(model, "rms") && inherits(model, "ols")) ||
#     inherits(model, "lmrob") ||
#     inherits(model, "lmRob") ||
#     inherits(model, "ivreg")
#
# }
#
# # Reverse-engineering `insight::get_parameters()` to find location of
# # coefficients so they can be inserted into model fit. Returns a vector
# # that when supplied to `[[` gets the location of the coefficients.
# # `coefs` is a vector from sim(); for multivariate models, it is transformed back
# # into a matrix
# get_coef_location <- function(fit = NULL, coefs, template = NULL) {
#
#   if (is.null(fit)) return(NULL)
#
#   loc <- NULL
#   if (inherits_any(fit, c("glm", "lm", "svyglm",
#                           "fixest",
#                           "lm_robust", "iv_robust",
#                           "betareg",
#                           "logistf", "flic", "flac",
#                           "systemfit",
#                           "lmrob",
#                           "lmRob", "glmRob",
#                           "survreg"))) {
#     loc <- "coefficients"
#   }
#   #Add more for known models
#
#   if (is.null(loc)) {
#     loc <- list.search(fit, untransform_coefs(coefs, template))
#   }
#
#   loc
# }
#
# # Create template of structure of coefficients in original model fit to make
# # transformation from simulated coefficients to coefficients the model expects
# get_coef_template <- function(fit = NULL, coefs) {
#
#   if (is.null(fit)) return(NULL)
#
#   template <- NULL
#
#   if (inherits(fit, "betareg")) {
#     template <- list(mean = which(!startsWith(names(coefs), "(phi)")),
#                      precision = which(startsWith(names(coefs), "(phi)")))
#   }
#   else if (inherits_any(fit, c("mlm")) ||
#            (inherits(fit, "lm_robust") && length(dim(coef(fit))) == 2)) {
#     template <- array(seq_len(coefs), dim = 2, dimnames = dimnames(coefs))
#   }
#   return(template)
# }
