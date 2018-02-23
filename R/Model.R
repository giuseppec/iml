#' Prediction Model object
#' 
#' A \code{Model} object wraps any machine learning model (mlr, caret, randomForest, ...). 
#' The object is to be used for the interpretation methods. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Model
#' @section Usage:
#' \preformatted{
#' model = Model$new(object, class=NULL, predict.args = NULL)
#' 
#' model$predict(newdata)
#' }
#' 
#' @section Arguments:
#' \describe{
#' \item{object}{The machine learning model. Recommended are models from mlr and caret.
#' Other machine learning with a S3 predict functions work as well, but less robust (e.g. randomForest).
#' \code{object} can also be a function that returns the prediction as a data.frame, given the features.}
#' \item{class}{The class column to be returned in case of multi.class output.}
#' \item{predict.args}{Further arguments for the prediction function of each model. Depends on the class of the original machine learning model. 
#' See examples.}
#' }
#' 
#' @section Details: 
#' In case of classification, the model must return the probabilities. Model doesn't handle machine learning
#' models that return the labels. 
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{class}{The class column to be returned.}
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
#' @importFrom mlr getTaskType getPredictionProbabilities getPredictionResponse
#' @export
#' @return object of type Model
#' @examples
#' if (require("mlr")){
#' task = makeClassifTask(data = iris, target = "Species")
#' learner = makeLearner("classif.rpart", minsplit = 7, predict.type = "prob")
#' mod.mlr = train(learner, task)
#' mod = Model$new(mod.mlr)
#' mod$predict(iris[1:5,])
#' 
#' mod = Model$new(mod.mlr, class = "setosa")
#' mod$predict(iris[1:5,])
#' }
#' 
#' if (require("randomForest")) {
#' rf = randomForest(Species ~ ., data = iris, ntree = 20)
#' mod = Model$new(rf, predict.args = list(type = "prob"))
#' mod$predict(iris[50:55,])
#' }
NULL

#' @export

Model = R6::R6Class("Model", 
  public = list(
    predict = function(newdata) {
      checkmate::assert_data_frame(newdata)
      prediction = self$prediction.function(newdata)
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
    initialize = function(object, class=NULL, predict.args = NULL) {
      self$class = class
      private$object = object
      self$task = inferTaskFromModel(object)
      self$prediction.function = createPredictionFunction(object, self$task, predict.args)
    }
  ), 
  private = list(
    object = NULL
  )
)








