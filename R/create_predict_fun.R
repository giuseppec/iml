create_predict_fun <- function(model, task, predict.fun = NULL, type = NULL) {
  # Use user-specified predict.fun if user has passed one
  # TODO: this might be useful also for all the other create_predict_fun methods as users might want to do specific things
  if (is.null(model)) {
    return(function(newdata) {
      pred <- predict.fun(newdata = newdata)
      if (is_label(pred)) {
        factor_to_dataframe(pred)
      }
      data.frame(pred, check.names = FALSE)
    })
  }
  if (!is.null(predict.fun)) {
    return(function(newdata) sanitizePrediction(predict.fun(model, newdata = newdata)))
  }
  create_predict_fun2(model, task, predict.fun, type)
}

create_predict_fun2 <- function(model, task, predict.fun = NULL, type = NULL) {
  UseMethod("create_predict_fun2")
}



create_predict_fun2.WrappedModel <- function(model, task, predict.fun = NULL, type = NULL) {
  if (!requireNamespace("mlr")) {
    "Please install the mlr package."
  }
  if (task == "classification") {
    function(newdata) {
      pred <- predict(model, newdata = newdata)
      if (model$learner$predict.type == "response") {
        pred <- mlr::getPredictionResponse(pred)
        factor_to_dataframe(pred)
      } else {
        mlr::getPredictionProbabilities(pred, cl = model$task.desc$class.levels)
      }
    }
  } else if (task == "regression") {
    function(newdata) {
      pred <- predict(model, newdata = newdata)
      data.frame(.prediction = mlr::getPredictionResponse(pred))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}


create_predict_fun2.Learner <- function(model, task, predict.fun = NULL, type = NULL) {
  if (!requireNamespace("mlr3")) {
    "Please install the mlr3 package."
  }
  if (task == "classification") {
    function(newdata) {
      if (model$predict_type == "response") {
        pred <- predict(model, newdata = newdata)
        factor_to_dataframe(pred)
      } else {
        data.frame(predict(model, newdata = newdata, predict_type = "prob"), check.names = FALSE)
      }
    }
  } else if (task == "regression") {
    function(newdata) {
      data.frame(.prediction = predict(model, newdata = newdata))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}


create_predict_fun2.train <- function(model, task, predict.fun = NULL, type = NULL) {
  if (task == "classification") {
    function(newdata) {
      if (is.null(type)) {
        pred <- predict(model, newdata = newdata)
      } else {
        pred <- predict(model, newdata = newdata, type = type)
      }
      if (is_label(pred)) {
        pred <- factor_to_dataframe(pred)
      }
      pred
    }
  } else if (task == "regression") {
    function(newdata) {
      if (is.null(type)) {
        prediction <- predict(model, newdata = newdata)
      } else {
        prediction <- predict(model, newdata = newdata, type = type)
      }
      data.frame(.prediction = prediction, check.names = FALSE)
    }
  } else {
    stop(sprintf("task of type %s not allowed.", task))
  }
}

#' @importFrom stats model.matrix
create_predict_fun2.default <- function(model, task, predict.fun = NULL, type = NULL) {
  if (is.null(predict.fun)) {
    if (is.null(type)) {
      predict.fun <- function(object, newdata) predict(object, newdata)
    } else {
      predict.fun <- function(object, newdata) predict(object, newdata, type = type)
    }
  }
  function(newdata) {
    pred <- do.call(predict.fun, list(model, newdata = newdata))
    if (is_label(pred)) {
      pred <- factor_to_dataframe(pred)
    }
    data.frame(pred, check.names = FALSE)
  }
}

create_predict_fun2.keras.engine.training.Model <- function(model, task, predict.fun = NULL, type = NULL) {
  if (is.null(predict.fun)) {
    predict.fun <- function(object, newdata) predict(object, newdata)
  }
  function(newdata) {
    pred <- do.call(predict.fun, list(model, newdata = as.matrix(newdata)))
    data.frame(pred, check.names = FALSE)
  }
}

create_predict_fun2.H2ORegressionModel <- function(model, task, predict.fun = NULL, type = NULL) {
  function(newdata) {
    newdata2 <- h2o::as.h2o(newdata)
    as.data.frame(h2o::h2o.predict(model, newdata = newdata2))
  }
}


create_predict_fun2.H2OBinomialModel <- function(model, task, predict.fun = NULL, type = NULL) {
  function(newdata) {
    # TODO: Include predict.fun and type
    newdata2 <- h2o::as.h2o(newdata)
    as.data.frame(h2o::h2o.predict(model, newdata = newdata2))[, -1] 
  }
}

create_predict_fun2.H2OMultinomialModel <- function(model, task, predict.fun = NULL, type = NULL) {
  function(newdata) {
    # TODO: Include predict.fun and type
    newdata2 <- h2o::as.h2o(newdata)
    # Removes first column with classification
    # Following columns contain the probabilities
    as.data.frame(h2o::h2o.predict(model, newdata = newdata2))[, -1]
  }
}



factor_to_dataframe <- function(fac) {
  check_vector(fac)
  res <- data.frame(model.matrix(~ fac - 1, data.frame(fac = fac), sep = ":"), check.names = FALSE)
  colnames(res) <- substring(colnames(res), 4) #make.names(levels(fac))#
  res
}
