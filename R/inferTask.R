inferTaskFromModel = function(object){
  UseMethod("inferTaskFromModel")
}

inferTaskFromModel.WrappedModel = function(object){
  if(inherits(object, "WrappedModel"))
    tsk = mlr::getTaskType(object)
  if (tsk == "classif") {
    if (object$learner$predict.type != "prob") {
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


inferTaskFromModel.train = function(object) {
  mtype = object$modelType
  if (mtype == "Regression") {
    return("regression")
  } else if (mtype == "Classification") {
    return("classification")
  } else {
    stop(sprintf("caret model type %s not supported.", mtype))
  }
}

inferTaskFromModel.default = function(object){
  "unknown"
}



inferTaskFromPrediction  = function(prediction){
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