


## TODO
# - Think about subclasses for mlr/caret/predict/function


#' Get a prediction object
#' 
#' @param object function, mlr WrappedObject, S3 class with predict function, or caret train object
#' @param class In case of classification, class specifies the class for which to predict the probability. 
#' By default the first class in the prediction (first column) is chosen. 
#' @return object of type Prediction
prediction.model = function(object, class = NULL, predict.args = NULL){
  assert_vector(class, len=1, null.ok=TRUE)
  if(inherits(object, "function")) {
    Prediction.f$new(object = object, class = class)
  } else if(inherits(object, "WrappedModel")){
    Prediction.mlr$new(object = object, class = class)
  } else if(inherits(object, 'train')){
    Prediction.caret$new(object = object, class = class)
  } else if(has.predict(object)) {
    Prediction.S3$new(object = object, class = class, predict.args = predict.args)
  } else {
    stop(sprintf('Object of type [%s] not supported', paste(class(object), collapse = ", ")))
  }
}



Prediction = R6::R6Class("Prediction", 
  public = list(
    predict = function(newdata, labels = FALSE){
      newdata = data.frame(newdata)
      prediction = data.frame(private$predict.function(newdata))
      # Store the class labels
      if(is.null(self$prediction.colnames)) private$extract.class.labels(prediction)
      if(self$type == 'classification' & !is.null(self$class)){
        prediction = prediction[,self$class, drop=FALSE]
      } 
      if(labels & ncol(prediction) > 1){
        prediction = self$prediction.colnames[apply(prediction, 1, which.max)]
        prediction = data.frame(..class = prediction)
      } else {
        colnames(prediction) = self$prediction.colnames
      }
      rownames(prediction) = NULL
      data.frame(prediction)
    },
    class = NULL,
    prediction.colnames = NULL, 
    type = NULL,
    print = function(){
      cat('Prediction type:', self$type, '\n')
      if(self$type == 'classification') cat('Classes: ', paste(self$prediction.colnames, collapse = ', '))
    },
    initialize = function(object, class=NULL){
      # if object has predict function, but not from caret or mlr, then 
      # it is difficult to know if it's classification or regression model
      # if class was set, then at least we can make a guess
      self$class = class
      if(!is.null(class)) self$type = 'classification'
      private$object = object
      private$infer.type()
    }
  ), 
  private = list(
    object = NULL, 
    extract.class.labels = function(prediction){
      self$prediction.colnames = colnames(prediction)
      if(!is.null(self$class)){
        self$prediction.colnames = self$prediction.colnames[self$class]
      }
    }
  )
)

# For caret: extractPrediction
# For caret: extractProb
Prediction.mlr = R6::R6Class("Prediction.mlr", 
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
Prediction.f = R6::R6Class("Prediction.f", 
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
Prediction.caret = R6::R6Class("Prediction.caret", 
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
Prediction.S3 = R6::R6Class("Prediction.S3", 
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




