#Shortcut for predicting from models to avoid slow insight::get_predicted()
#for simple models (GLMs)
simbased_predict <- function(x, newdata = NULL) {
  if (inherits(x, "lm") || inherits(x, "glm")) {
    predict(x, newdata = newdata, type = "response")
  }
  else {
    setNames(as.numeric(insight::get_predicted(x, data = newdata, predict = "expectation",
                                               verbose = FALSE)),
             rownames(newdata))
  }
}
