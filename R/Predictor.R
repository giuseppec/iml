#' Predictor object
#' 
#' A \code{Predictor} object holds any machine learning model (mlr, caret, randomForest, ...) and the data to be used of analysing the model. 
#' The interpretation methods in the iml package need the machine learning model to be wrapped in a \code{Predictor} object. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Predictor
#' @section Usage:
#' \preformatted{
#' model = Predictor$new(model = NULL, data, y = NULL, class=NULL, 
#'   predict.fun = NULL, type = NULL)
#'   
#' model$predict(newdata)
#' }
#' 
#' @section Arguments:
#' \describe{
#' \item{model: }{(any)\cr The machine learning model. Recommended are models from mlr and caret.
#' Other machine learning with a S3 predict functions work as well, but less robust (e.g. randomForest).}
#' \item{data: }{(data.frame)\cr The data to be used for analysing the prediction model. Allowed column classes are: numeric, factor, integer, ordered and character.
#' For some models the data can be extracted automatically. Predictor$new throws an error when it can't extract the data automatically.}
#' \item{y: }{((`character(1)`) | numeric | factor)\cr The target vector or (preferably) the name of the target column in the \code{data} argument.
#' Predictor tries to infer the target automatically from the model.}
#' \item{class: }{(`character(1)`)\cr The class column to be returned in case of multiclass output. 
#' You can either use numbers, e.g. class=2 would take the 2nd column from the predictions, or the column name of the predicted class, e.g. class="dog".}
#' \item{predict.fun: }{(function)\cr The function to predict newdata. Only needed if \code{model} is not a model from mlr or caret package. 
#' The first argument of predict.fun has to be the model, the second the newdata: function(model, newdata)}
#' \item{type: }{(`character(1)`)\cr This argument is passed to the prediction function of the model. 
#' For regression models you usually don't have to provide the type argument.
#' The classic use case is to say type="prob" for classification models. 
#' Consult the documentation of the machine learning package you use to find which type options you have.
#' If both predict.fun and type are used, then type is passed as an argument to predict.fun.}
#' \item{batch.size: }{(`numeric(1)`)\cr The maximum number of rows to be input the model for prediction at once. Currently only respected for FeatureImp, Partial and Interaction.}
#' }
#' 
#' @section Details: 
#' A Predictor object is a container for the prediction model and the data. 
#' This ensures that the machine learning model can be analysed robustly. 
#' 
#' Note: In case of classification, the model should return one column per class with the class probability.
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{class: }{(`character(1)`)\cr The class column to be returned.}
#' \item{data: }{(data.frame)\cr data object with the data for the model interpretation.}
#' \item{prediction.colnames: }{(character)\cr The column names of the predictions.}
#' \item{task: }{(`character(1)`)\cr The inferred prediction task: "classification" or "regression".}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{predict(newdata)}{method to predict new data with the machine learning model.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' @export
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
#' 
#' mod = Predictor$new(rf, data = iris, type = "prob")
#' mod$predict(iris[50:55,])
#' 
#' # Feature importance needs the target vector, which needs to be supplied: 
#' mod = Predictor$new(rf, data = iris, y = "Species", type = "prob")
#' }
NULL

#' @export

Predictor = R6::R6Class("Predictor", 
  public = list(
    data = NULL,
    model = NULL, 
    batch.size = NULL,
    predict = function(newdata) {
      checkmate::assert_data_frame(newdata)
      # Makes sure it's not a data.table
      newdata = as.data.frame(newdata)
      # make sure only features are used
      newdata = newdata[,intersect(self$data$feature.names, colnames(newdata)), drop = FALSE]
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
    initialize = function(model = NULL, data = NULL, predict.fun = NULL, y = NULL, class=NULL, 
      type = NULL, batch.size = 1000) {
      assert_number(batch.size, lower = 1)
      if(is.null(model) & is.null(predict.fun)) { 
        stop("Provide a model, a predict.fun or both!")  
      }
      if (is.null(data)) {
        tryCatch({data = prediction::find_data(model)}, 
          error = function(e) stop("Can't extract data from model, please provide via data="))
      }
      if (is.null(y)) {
        y = find_y(model)
        # Extra fix for caret, because it renames the target in the data
        if(inherits(model, "train")) {
          colnames(data)[colnames(data) == ".outcome"] = y
        }
        # y not always needed, so ignore when not in data
        if(is.character(y) && !(y %in% names(data))) {
          y = NULL
        }
      }
      
      self$data = Data$new(data, y = y)
      self$class = class
      self$model = model
      self$task = inferTaskFromModel(model)
      self$prediction.function = create_predict_fun(model, self$task, predict.fun, type = type)
      self$batch.size = batch.size
    }
  ), 
  private = list(
    predictionChecked = FALSE
  )
)








