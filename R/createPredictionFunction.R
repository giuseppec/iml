createPredictionFunction = function(model, task, predict.fun = NULL){
  UseMethod("createPredictionFunction")
}

createPredictionFunction.WrappedModel = function(model, task, predict.fun = NULL){
  if (!requireNamespace("mlr")) {
    "Please install the mlr package."
  }
  if (task == "classification") {
    function(newdata){
      pred = predict(model, newdata = newdata)
      if (model$learner$predict.type == "response") {
        pred = mlr::getPredictionResponse(pred)
        data.frame(model.matrix(~prediction-1, data.frame(prediction = pred), sep = ":"))
      } else {
        mlr::getPredictionProbabilities(pred, cl = model$task.desc$class.levels)
      }
    }
  } else if (task == "regression") {      
    function(newdata){
      pred = predict(model, newdata = newdata)
      data.frame(..prediction = mlr::getPredictionResponse(pred))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}

createPredictionFunction.train = function(model, task, predict.fun = NULL){
  if (task == "classification") {
    function(newdata) {
      predict(model, newdata = newdata, type = "prob")
    }
  } else if (task == "regression") {
    function(newdata) {
      data.frame(..prediction = predict(model, newdata = newdata))
    }
  } else {
    stop(sprintf("task of type %s not allowed.", task))
  }
}


createPredictionFunction.NULL = function(model, task, predict.fun = NULL) {
  function(newdata) { 
    pred = predict.fun(newdata = newdata)
    if (is.label.output(pred)) {
      warning("Output seems to be class instead of probabilities. 
          Automatically transformed to 0 and 1 probabilities.
          Please use the predict.fun argument.")
      pred = data.frame(model.matrix(~prediction-1, data.frame(prediction = pred), sep = ":"))
    }
    data.frame(pred)
  }
}

#' @importFrom stats model.matrix
createPredictionFunction.default = function(model, task, predict.fun = NULL){
  if (is.null(predict.fun)) {
    predict.fun = function(object, newdata) predict(object, newdata)
  }
  function(newdata) {
    pred = do.call(predict.fun, list(model, newdata = newdata))
    if (is.label.output(pred)) {
      warning("Output seems to be class instead of probabilities. 
          Automatically transformed to 0 and 1 probabilities.
          Please use the predict.fun argument.")
      pred = data.frame(model.matrix(~prediction-1, data.frame(prediction = pred), sep = ":"))
    }
    data.frame(pred)
  }
}