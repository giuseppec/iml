


## TODO
# - Think about subclasses for mlr/caret/predict/function


#' Get a prediction object
#' 
#' @param object function, mlr WrappedObject, S3 class with predict function, or caret train object
#' @param multi.class TRUE if object prediction is multi.class and 
#' you want to get the probabilities for all classes, not only the first one. Defaults to FALSE. 
#' The multi.class parameter is ignored when the class parameter is set. 
#' @param class In case of classification, class specifies the class for which to predict the probability. 
#' By default the first class in the prediction (first column) is chosen. 
#' @return object of type Prediction
prediction.model = function(object, class = NULL, multi.class=FALSE, predict.args = NULL){
  assert_logical(multi.class, len=1)
  assert_vector(class, len=1, null.ok=TRUE)
  
  # Ignore the multi.class flag if class parameter is set
  if(!is.null(class)) multi.class = FALSE
  
  
  if(inherits(object, "function")) {
    Prediction.f$new(object = object, class = class, multi.class = multi.class)
  } else if(inherits(object, "WrappedModel")){
    Prediction.mlr$new(object = object, class = class, multi.class = multi.class)
  } else if(inherits(object, 'train')){
    Prediction.caret$new(object = object, class = class, multi.class = multi.class)
  } else if(has.predict(object)) {
    Prediction.S3$new(object = object, class = class, multi.class = multi.class, predict.args = predict.args)
  } else {
    stop(sprintf('Object of type [%s] not supported', paste(class(object), collapse = ", ")))
  }
}



Prediction = R6Class("Prediction", 
  public = list(
    predict = function(newdata){
      newdata = data.frame(newdata)
      prediction = private$predict.function(newdata)
      if(!self$multi.class){
        prediction = private$predict.function(newdata)
        if(self$type == 'classification'){
          prediction = prediction[,self$class]
        } 
      }
      data.frame(prediction)
    },
    predict.class = function(newdata){
      stopifnot(self$multi.class)
      pred = self$predict(newdata)
      classes = colnames(pred)
      classes[apply(pred, 1, which.max)]
    },
    multi.class = NULL,
    class = NULL,
    type = NULL,
    initialize = function(object, class=NULL, multi.class=FALSE){
      # if object has predict function, but not from caret or mlr, then 
      # it is difficult to know if it's classification or regression model
      # if class or multi.class was set, then at least we can make a guess
      if(!is.null(class) | multi.class) self$type = 'classification'
      self$class = ifelse(is.null(class), 1, class)
      self$multi.class = multi.class
      private$object = object
      private$infer.type()
    }
  ), 
  private = list(
    object = NULL
  )
)

# For caret: extractPrediction
# For caret: extractProb
Prediction.mlr = R6Class("Prediction.mlr", 
  inherit = Prediction,
  public = list(), 
  private = list(
    # Automatically set task type and class name according to output
    infer.type = function(){
      tsk = mlr::getTaskType(private$object)
      if(tsk == 'classif'){
        self$type = 'classification'
      } else if(tsk == 'regr'){
        self$type = 'regression'
      } else {
        stop(sprintf('mlr task type <%s> not supported', tsk))
      }
    },
    predict.function = function(x){
      pred = predict(private$object, newdata = x)
      if(self$type == 'classification') {
        getPredictionProbabilities(pred)
      } else {
        getPredictionResponse(pred)
      }
    }
  )
)



# For caret: extractPrediction
# For caret: extractProb
Prediction.f = R6Class("Prediction.f", 
  inherit = Prediction,
  public = list(), 
  private = list(
    ## can't infer the type before first prediction
    infer.type = function(){},
    infer.f.type = function(pred){
      assert_true(any(class(pred) %in% c('integer', 'numeric', 'data.frame', 'matrix')))
      if(inherits(pred, c('data.frame', 'matrix')) && dim(pred)[2] > 1) {
        self$type = 'classification' 
      } else {
        self$type = 'regression'
      }
    }, 
    predict.function = function(x){
      pred = private$object(x)
      if(is.null(self$type)) private$infer.f.type(pred)
      pred
    }
  )
)

# For caret: extractPrediction
# For caret: extractProb
Prediction.caret = R6Class("Prediction.caret", 
  inherit = Prediction,
  public = list(), 
  private = list(
    infer.type = function(){
      mtype = private$object$modelType
      if(mtype == 'Regression'){
        self$type = 'regression'
      } else if (mtype == 'Classification'){
        self$type = 'classification'
      } else {
        stop(sprintf('caret model type %s not supported.', mtype))
      }
    },
    predict.function = function(x){
      if(self$type == 'classification'){
        predict(private$object, newdata = x,  type = 'prob')
      } else if(self$type == 'regression'){
        predict(private$object, newdata = x)
      } else {
        stop(sprintf('private$type of %s not allowed.', private$type))
      }
    }
  )
)

# For caret: extractPrediction
# For caret: extractProb
Prediction.S3 = R6Class("Prediction.S3", 
  inherit = Prediction.f,
  public = list(
    initialize = function(predict.args=NULL, ...){
      super$initialize(...)
      private$predict.args = predict.args
    }
  ), 
  private = list(
    predict.args = NULL,
    infer.type = function(){},
    predict.function = function(x){
      predict.args = c(list(object = private$object, newdata = x), private$predict.args)
      pred = do.call(predict, predict.args)
      if(private$is.label.output(pred)) stop("Output seems to be class instead of probabilities. Please use the predict.args argument.")
      if(is.null(self$type)) private$infer.f.type(pred)
      pred 
    },
    is.label.output = function(pred){
      if(inherits(pred, c('character', 'factor'))) return(TRUE)
      if(inherits(pred, c('data.frame', 'matrix')) && inherits(pred[,1], 'character')){
        return(TRUE)
      }
      FALSE
    }
  )
)




