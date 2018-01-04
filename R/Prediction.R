


## TODO
# - Allow caret
# - Allow anything with predict function
# - Implement predict
# - Think about subclasses for mlr/caret/predict/function

# returns TRUE if object has predict function
object.has.predict = function(object){
  
}


#' Get a prediction object
#' 
#' @param object function, mlr WrappedObject, S3 class with predict function, or caret train object
#' @param multi.class TRUE if object prediction is multi.class and 
#' you want to get the probabilities for all classes, not only the first one. Defaults to FALSE. 
#' The multi.class parameter is ignored when the class parameter is set. 
#' @param class In case of classification, class specifies the class for which to predict the probability. 
#' By default the first class in the prediction (first column) is chosen. 
#' @return object of type Prediction
prediction.model = function(object, class = NULL, multi.class=FALSE){
  assert_logical(multi.class, len=1)
  assert_vector(class, len=1, null.ok=TRUE)
  
  # Ignore the multi.class flag if class parameter is set
  if(!is.null(class)) multi.class = FALSE
  
  Prediction$new(object, class = class, multi.class=multi.class)
}


# For caret: extractPrediction
# For caret: extractProb
Prediction = R6Class("SingleOutputPrediction", 
  public = list(
    predict = function(newdata){
      if(self$multi.class){
        pred = private$predict.function(newdata)
      } else {
        pred = private$predict.function(newdata)
        if(self$type == 'classification'){
          pred = pred[,self$class]
        } 
      }
      pred    
    },
    multi.class = NULL,
    class = NULL,
    class.name = NULL, 
    type = NULL,
    initialize = function(object, class=NULL, multi.class=FALSE){
      self$class = class
      self$multi.class = multi.class
      private$object = object
      if(inherits(private$object, "function")) {
        private$predict.function = private$predict.f
      } else if(inherits(object, "WrappedModel")){
        private$infer.mlr.type()
        private$predict.function = private$predict.mlr
      } else {
        stop(sprintf('Object of type [%s] not supported', paste(class(object), collapse = ", ")))
      }
    }
  ), 
  private = list(
    object = NULL, 
    predict.function = NULL,
    # Automatically set task type and class name according to output
    infer.f.type = function(pred){
      assert_true(any(class(pred) %in% c('integer', 'numeric', 'data.frame', 'matrix')))
      if(inherits(pred, c('data.frame', 'matrix')) && dim(pred)[2] > 1) {
        self$type = 'classification' 
        if(is.null(self$class)) self$class = 1
        if(!(is.character(self$class))) self$class.name = colnames(pred)[self$class]
      } else {
        self$type = 'regression'
      }
    }, 
    infer.mlr.type = function(){
      tsk = mlr::getTaskType(private$object)
      if(tsk == 'classif'){
        self$type = 'classification'
        if(is.null(self$class)) self$class = 1
      } else if(tsk == 'regr'){
        self$type = 'regression'
      } else {
        stop(sprintf('mlr task type <%s> not supported', tsk))
      }
    },
    predict.f = function(x){
      pred = private$object(x)
      if(is.null(self$type)) private$infer.f.type(pred)
      pred
    }, 
    predict.mlr = function(x){
      pred = predict(private$object, newdata = x)
      if(self$type == 'classification') {
        getPredictionProbabilities(pred)
      } else {
        getPredictionResponse(pred)
      }
    }
  )
)

# For caret: extractProb
MultiOutputPrediction = R6Class('MultiOutputPrediction', 
  inherit = PredictionModel)


