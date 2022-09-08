#Shortcut for predicting from models to avoid slow insight::get_predicted()
#for simple models (GLMs)
simbased_predict <- function(x, newdata = NULL) {
  if (inherits_any(x, c("lm", "glm", "betareg")) && !inherits_any(x, c("svyglm", "rms"))) {
    p <- setNames(predict(x, newdata = newdata, type = "response"),
                  rownames(newdata))
  }
  else if (inherits(x, "ols") && inherits(x, "rms")) {
    p <- setNames(predict(x, newdata = newdata, type = "lp"),
                  rownames(newdata))
  }
  else if (inherits(x, "lrm") && inherits(x, "rms")) {
    p <- setNames(predict(x, newdata = newdata, type = "fitted"),
                  rownames(newdata))
  }
  else if (insight::is_model_supported(x)) {
    p <- try(setNames(as.numeric(insight::get_predicted(x, data = newdata, predict = "expectation",
                                               verbose = FALSE)),
             rownames(newdata)), silent = TRUE)
    if (is_error(p)) {
      chk::err("predcited values could not be extracted from the model")
    }
  }
  else {
    chk::err("predcited values could not be extracted from the model")
  }
  return(p)
}
