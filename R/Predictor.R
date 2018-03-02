#' Predictor object
#' 
#' A \code{Predictor} object wraps any machine learning model (mlr, caret, randomForest, ...). 
#' The object is to be used for the interpretation methods. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Predictor
#' @section Usage:
#' \preformatted{
#' model = Predictor$new(object, data, y = NULL, class=NULL, 
#'   predict.fun = function(object, newdata) predict(object, newdata))
#' }
#' 
#' @section Arguments:
#' \describe{
#' \item{model}{The machine learning model. Recommended are models from mlr and caret.
#' Other machine learning with a S3 predict functions work as well, but less robust (e.g. randomForest).
#' \code{model} can also be a function that returns the prediction as a data.frame, given the features.}
#' \item{data}{the data to be used for the prediction}
#' \item{y}{The target vector or (preferably) the name of the target column in the \code{data} argument.}
#' \item{class}{The class column to be returned in case of multiClass output.}
#' \item{predict.fun}{The function to predict newdata. Only needed if \code{model} is not a model from mlr or caret package.}
#' }
#' 
#' @section Details: 
#' In case of classification, the model must return the probabilities. Predictor doesn't handle machine learning
#' models that return the labels. 
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{class}{The class column to be returned.}
#' \item{data}{data.frame with the data for the model}
#' \item{prediction.colnames}{The column names of the predictions.}
#' \item{task}{The inferred prediction task: "classification" or "regression".}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{predict(newdata)}{method to predict new data with the machine learning model. See also \link{predict.TreeSurrogate}}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' @export
#' @return object of type Predictor
#' @examples
#' if (require("mlr")) {
#' task = makeClassifTask(data = iris, target = "Species")
#' learner = makeLearner("classif.rpart", minsplit = 7, predict.type = "prob")
#' mod.mlr = train(learner, task)
#' mod = Predictor$new(mod.mlr, data = iris)
#' mod$predict(iris[1:5,])
#' 
#' mod = Predictor$new(mod.mlr, data = iris, class = "setosa")
#' mod$predict(iris[1:5,])
#' }
#' 
#' if (require("randomForest")) {
#' rf = randomForest(Species ~ ., data = iris, ntree = 20)
#' 
#' # We need this for the randomForest
#' predict.fun = function(obj, newdata) { 
#'   predict(obj, newdata = newdata, type = "prob")
#' }
#' 
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun)
#' mod$predict(iris[50:55,])
#' 
#' # Feature importance needs the target vector, which needs to be supplied: 
#' mod = Predictor$new(rf, data = iris, y = "Species", predict.fun = predict.fun)
#' }
NULL

#' @export

Predictor = R6::R6Class("Predictor", 
  public = list(
    data = NULL,
    model = NULL, 
    predict = function(newdata) {
      checkmate::assert_data_frame(newdata)
      prediction = self$prediction.function(newdata)
      if (!private$predictionChecked) {
        checkPrediction(prediction, newdata)
        private$predictionChecked = TRUE
      }
      # If S3 or function, we can only infer the task 
      # once we see the predictions
      if (is.null(self$task)) {
        self$task = inferTaskFromPrediction(prediction)
      }
      if (!is.null(self$class) & ncol(prediction) > 1) {
        prediction = prediction[,self$class, drop=FALSE]
      } 
      rownames(prediction) = NULL
      data.frame(prediction)
    },
    class = NULL,
    prediction.colnames = NULL, 
    prediction.function = NULL,
    task = NULL,
    print = function() {
      cat("Prediction task:", self$task, "\n")
      if (self$task == "classification") {
        cat("Classes: ", paste(self$prediction.colnames, collapse = ", "))
      }
    },
    initialize = function(model = NULL, data, predict.fun = NULL, y = NULL, class=NULL) {
      if(is.null(model) & is.null(predict.fun)) { 
        stop("Provide a model, a predict.fun or both!")  
      }
      self$data = Data$new(data, y = y)
      self$class = class
      self$model = model
      self$task = inferTaskFromModel(model)
      self$prediction.function = createPredictionFunction(model, self$task, predict.fun)
    }
  ), 
  private = list(
    predictionChecked = FALSE
  )
)








