createPredictionFunction = function(model, task, predict.args = NULL){
  UseMethod("createPredictionFunction")
  
}

createPredictionFunction.WrappedModel = function(model, task, predict.args = NULL){
  if (task == "classification") {
    function(newdata){
      pred = predict(model, newdata = newdata)
      if (model$learner$predict.type == "response") {
        pred = getPredictionResponse(pred)
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

createPredictionFunction.train = function(model, task, predict.args = NULL){
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


createPredictionFunction.function = function(model, task, predict.args = NULL){
  function(newdata) {
    predict.args = c(list(newdata), predict.args)
    res = do.call(model, predict.args)
    if (inherits(res, c("numeric", "integer"))) {
      res = data.frame(..prediction = res, fix.empty.names = FALSE)
    } else if (inherits(res, "matrix")) {
      cnames = colnames(res)
      res = data.frame(res)
      if (is.null(cnames)) {
        if (ncol(res) == 1) {
          colnames(res) = "..prediction"
        } else {
          colnames(res) = paste("..prediction", 1:ncol(res), sep = ".")
        }
      } else {
        colnames(res) = cnames
      }
    } else {
      res = data.frame(res)
    }
    res
  }
}


#' @importFrom stats model.matrix
createPredictionFunction.default = function(model, task, predict.args = NULL){
  function(newdata) {
    predict.args = c(list(object = model, newdata = newdata), predict.args)
    pred = do.call(predict, predict.args)
    if (is.label.output(pred)) {
      warning("Output seems to be class instead of probabilities. 
          Automatically transformed to 0 and 1 probabilities.
          Please use the predict.args argument.")
      pred =  data.frame(model.matrix(~prediction-1, data.frame(prediction = pred), sep = ":"))
    }
    data.frame(pred)
  }
}