createPredictionFunction = function(model, task, predict.fun = NULL, type = NULL){
  UseMethod("createPredictionFunction")
}

createPredictionFunction.WrappedModel = function(model, task, predict.fun = NULL, type = NULL){
  if (!requireNamespace("mlr")) {
    "Please install the mlr package."
  }
  if (task == "classification") {
    function(newdata){
      pred = predict(model, newdata = newdata)
      if (model$learner$predict.type == "response") {
        pred = mlr::getPredictionResponse(pred)
        factor.to.dataframe(pred)
      } else {
        mlr::getPredictionProbabilities(pred, cl = model$task.desc$class.levels)
      }
    }
  } else if (task == "regression") {      
    function(newdata){
      pred = predict(model, newdata = newdata)
      data.frame(.prediction = mlr::getPredictionResponse(pred))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}

createPredictionFunction.train = function(model, task, predict.fun = NULL, type = NULL){
  
  if (task == "classification") {
    function(newdata) {
      if (is.null(type)) {
        pred = predict(model, newdata = newdata)
      } else {
        pred = predict(model, newdata = newdata, type = type)
      }
      if (is.label.output(pred)) {
        pred = factor.to.dataframe(pred)
      }
      pred
    }
  } else if (task == "regression") {
    function(newdata) {
      if (is.null(type)) {
        prediction = predict(model, newdata = newdata)
      } else {
        prediction = predict(model, newdata = newdata, type = type)
      }
      data.frame(.prediction = prediction)
    }
  } else {
    stop(sprintf("task of type %s not allowed.", task))
  }
}



createPredictionFunction.NULL = function(model, task, predict.fun = NULL, type = NULL) {
  function(newdata) { 
    pred = predict.fun(newdata = newdata)
    if (is.label.output(pred)) {
      factor.to.dataframe(pred)
    }
    data.frame(pred)
  }
}

#' @importFrom stats model.matrix
createPredictionFunction.default = function(model, task, predict.fun = NULL, type = NULL){
  if (is.null(predict.fun)) {
    if (is.null(type)) {
      predict.fun = function(object, newdata) predict(object, newdata)
    } else {
      predict.fun = function(object, newdata) predict(object, newdata, type = type)
    }
  }
  function(newdata) {
    pred = do.call(predict.fun, list(model, newdata = newdata))
    if (is.label.output(pred)) {
      pred = factor.to.dataframe(pred)
    }
    data.frame(pred)
  }
}


factor.to.dataframe = function(fac) {
  check_vector(fac)
  res = data.frame(model.matrix(~fac-1, data.frame(fac = fac), sep = ":"))
  colnames(res) = substring(colnames(res), 4)
  res
}
