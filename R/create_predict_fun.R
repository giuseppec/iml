create_predict_fun = function(model, task, predict.fun = NULL, type = NULL){
  UseMethod("create_predict_fun")
}

create_predict_fun.WrappedModel = function(model, task, predict.fun = NULL, type = NULL){
  if (!requireNamespace("mlr")) {
    "Please install the mlr package."
  }
  if (task == "classification") {
    function(newdata){
      pred = predict(model, newdata = newdata)
      if (model$learner$predict.type == "response") {
        pred = mlr::getPredictionResponse(pred)
        factor_to_dataframe(pred)
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

create_predict_fun.train = function(model, task, predict.fun = NULL, type = NULL){
  
  if (task == "classification") {
    function(newdata) {
      if (is.null(type)) {
        pred = predict(model, newdata = newdata)
      } else {
        pred = predict(model, newdata = newdata, type = type)
      }
      if (is_label(pred)) {
        pred = factor_to_dataframe(pred)
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



create_predict_fun.NULL = function(model, task, predict.fun = NULL, type = NULL) {
  function(newdata) { 
    pred = predict.fun(newdata = newdata)
    if (is_label(pred)) {
      factor_to_dataframe(pred)
    }
    data.frame(pred)
  }
}

#' @importFrom stats model.matrix
create_predict_fun.default = function(model, task, predict.fun = NULL, type = NULL){
  if (is.null(predict.fun)) {
    if (is.null(type)) {
      predict.fun = function(object, newdata) predict(object, newdata)
    } else {
      predict.fun = function(object, newdata) predict(object, newdata, type = type)
    }
  }
  function(newdata) {
    pred = do.call(predict.fun, list(model, newdata = newdata))
    if (is_label(pred)) {
      pred = factor_to_dataframe(pred)
    }
    data.frame(pred)
  }
}


factor_to_dataframe = function(fac) {
  check_vector(fac)
  res = data.frame(model.matrix(~fac-1, data.frame(fac = fac), sep = ":"))
  colnames(res) = substring(colnames(res), 4)
  res
}
