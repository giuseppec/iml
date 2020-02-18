inferTaskFromModel <- function(model) {
  UseMethod("inferTaskFromModel")
}

inferTaskFromModel.NULL <- function(model) {
  "unknown"
}

inferTaskFromModel.WrappedModel <- function(model) {
  if (!requireNamespace("mlr")) {
    stop("Please install the mlr package.")
  }
  if (inherits(model, "WrappedModel")) {
    tsk <- mlr::getTaskType(model)
  }
  if (tsk == "classif") {
    if (model$learner$predict.type != "prob") {
      warning("Output seems to be class instead of probabilities. 
               Automatically transformed to 0 and 1 probabilities.
               You might want to set predict.type = 'prob' for Learner!")
    }
    return("classification")
  } else if (tsk == "regr") {
    return("regression")
  } else {
    stop(sprintf("mlr task type <%s> not supported", tsk))
  }
}

inferTaskFromModel.Learner <- function(model) {
  if (!requireNamespace("mlr3")) {
    stop("Please install the mlr package.")
  }
  tsk <- model$task_type
  if (tsk == "classif") {
    if (model$predict_type != "prob") {
      warning("Output seems to be class instead of probabilities. 
               Automatically transformed to 0 and 1 probabilities.
               You might want to set predict.type = 'prob' for Learner!")
    }
    return("classification")
  } else if (tsk == "regr") {
    return("regression")
  } else {
    stop(sprintf("mlr task type <%s> not supported", tsk))
  }
}

inferTaskFromModel.train <- function(model) {
  mtype <- model$modelType
  if (mtype == "Regression") {
    return("regression")
  } else if (mtype == "Classification") {
    return("classification")
  } else {
    stop(sprintf("caret model type %s not supported.", mtype))
  }
}

inferTaskFromModel.default <- function(model) {
  "unknown"
}

inferTaskFromPrediction <- function(prediction) {
  assert_true(any(class(prediction) %in%
    c("integer", "numeric", "data.frame", "matrix", "factor", "character")))
  if (inherits(prediction, c("data.frame", "matrix")) && dim(prediction)[2] > 1) {
    "classification"
  } else if (inherits(prediction, c("factor", "character"))) {
    "classification"
  } else {
    "regression"
  }
}
