#' Lime
#' 
#' \code{Lime} fits a locally weighted linear regression model (logistic for classification) to explain a single machine learning prediction.
#' 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Lime
#' @section Usage:
#' \preformatted{
#' lime = Lime$new(model, data, x.interest = NULL, k = 3 run = TRUE)
#' 
#' plot(lime)
#' predict(lime, newdata)
#' lime$results
#' lime$explain(x.interest)
#' print(lime)
#' }
#' 
#' @section Arguments:
#' 
#' For Lime$new():
#' \describe{
#' \item{model}{Object of type \code{Model}. See \link{Model}.}
#' \item{data}{data.frame with the data for the prediction model.}
#' \item{x.interest}{data.frame with a single row for the instance to be explained.}
#' \item{k}{the (maximum) number of features to be used for the surrogate model.}
#' \item{run}{logical. Should the Interpretation method be run?}
#' }
#' 
#' @section Details: 
#' Data points are sampled and weighted by their proximity to the instance to be explained. 
#' A weighted glm is fitted with the machine learning model prediction as target. 
#' L1-regularisation is used to make the results sparse. 
#' The resulting model can be seen as a surrogate for the machine learning model, which is only valid for that one point.
#' Categorical features are binarized, depending on the category of the instance to be explained: 1 if the category is the same, 0 otherwise.
#' To learn more about local models, read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/lime.html
#'
#' Differences to the original Lime implementation: 
#' \itemize{
#' \item Distance measure: Uses gower proximity (= 1 - gower distance) instead of a kernel based on the Euclidean distance. Has the advantage to have a meaningful neighbourhood and no kernel width to tune.
#' \item Sampling: Uses the original data instead of sampling from normal distributions. 
#' Has the advantage to follow the original data distribution. 
#' \item Visualisation: Plots effects instead of betas. Is the same for binary features, but makes a difference for numerical features. 
#' For numerical features, plotting the betas makes no sense, 
#' because a negative beta might still increase the prediction when the feature value is also negative.
#' }
#' 
#' @section Fields:
#' \describe{
#' \item{best.fit.index}{the index of the best glmnet fit}
#' \item{k}{The number of features as set by the user.}
#' \item{model}{the glmnet object.}
#' \item{results}{data.frame with the feature names (\code{feature}) and contributions to the prediction}
#' \item{x.interest}{The data.frame with the instance to be explained See examples for usage.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{explain(x.interest)}{method to set a new data point which to explain.}
#' \item{plot()}{method to plot the Lime feature effects. See \link{plot.Lime}}
#' \item{predict()}{method to predict new data with the local model See also \link{predict.Lime}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' General R6 methods
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' 
#' @references 
#' Ribeiro, M. T., Singh, S., & Guestrin, C. (2016). "Why Should I Trust You?": Explaining the Predictions of Any Classifier. Retrieved from http://arxiv.org/abs/1602.04938
#' 
#' @seealso 
#' \code{\link{plot.Lime}} and \code{\link{predict.Lime}}
#' 
#' \code{\link{Shapley}} can also be used to explain single predictions
#' 
#' \code{\link[lime]{lime}}, the original implementation
#' @export
#' @importFrom glmnet glmnet
#' @examples 
#' # First we fit a machine learning model on the Boston housing data
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' X = Boston[-which(names(Boston) == "medv")]
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Model$new(rf)
#' 
#' # Then we explain the first instance of the dataset with the Lime method:
#' x.interest = X[1,]
#' lemon = Lime$new(mod, X, x.interest = x.interest, k = 2)
#' lemon
#' 
#' # Look at the results in a table
#' lemon$results
#' # Or as a plot
#' plot(lemon)
#'
#' # Reuse the object with a new instance to explain
#' lemon$x.interest
#' lemon$explain(X[2,])
#' lemon$x.interest
#' plot(lemon)
#'   
#' # Lime also works with multiclass classification
#' library("randomForest")
#' rf = randomForest(Species ~ ., data= iris, ntree=50)
#' mod = Model$new(rf,predict.args = list(type='prob'), class = 2)
#' X = iris[-which(names(iris) == 'Species')]
#' 
#' # Then we explain the first instance of the dataset with the Lime method:
#' lemon = Lime$new(mod, X, x.interest = X[1,], k = 2)
#' lemon$results
#' plot(lemon) 
#' }
NULL


#' @export

Lime = R6::R6Class("Lime", 
  inherit = InterpretationMethod,
  public = list(
    x.interest = NULL, 
    k = NULL,
    model = NULL,
    best.fit.index = NULL,
    predict = function(newdata = NULL, ...) {
      if (is.null(newdata)) newdata = self$x.interest
      X.recode = recode(newdata, self$x.interest)
      if (private$multiClass) {
        prediction = predict(self$model, newx=as.matrix(X.recode), type = "response")
        colnames(prediction) = colnames(private$predictResults)
      } else {
        prediction = predict(self$model, newx=as.matrix(X.recode))
      }
      if (private$multiClass) {
        data.frame(prediction[,,self$best.fit.index])
      } else {
        pred = prediction[,self$best.fit.index, drop=FALSE]
        colnames(pred) = NULL
        data.frame(prediction = pred)
      }
    },
    explain = function(x.interest) {
      self$x.interest = x.interest
      private$flush()
      self$run()
    },
    initialize = function(model, data, x.interest = NULL, k = 3, run = TRUE) {
      checkmate::assert_number(k, lower = 1, upper = ncol(data))
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      if (!require("glmnet")) {
        stop("Please install glmnet.")
      }
      super$initialize(model = model, data = data)
      self$k = k
      if (!is.null(x.interest)) {
        self$x.interest = x.interest
      }
      if (run & !is.null(x.interest)) self$run()
    }
  ),
  private = list(
    q = function(pred) probs.to.labels(pred),
    best.index = NULL,
    aggregate = function() {
      X.recode = recode(private$dataDesign, self$x.interest)
      x.recoded = recode(self$x.interest, self$x.interest)
      fam = ifelse(private$multiClass, "multinomial", "gaussian")
      y = unlist(private$qResults[1])
      self$model = glmnet(x = as.matrix(X.recode), y = y, 
        family = fam, w = private$weightSamples(), 
        intercept = TRUE, standardize = TRUE, type.multinomial = "grouped")
      res = self$model
      ## It can happen, that no n.vars matching k occurs
      if (any(res$df == self$k)) {
        best.index = max(which(res$df == self$k))
      } else {
        best.index = max(which(res$df < self$k))
        warning("Had to choose a smaller k")
      }
      self$best.fit.index = best.index
      if (private$multiClass) {
        class.results = lapply(res$beta, extract.glmnet.effects, 
          best.index = best.index, x.recoded = x.recoded, x.original = self$x.interest)
        res = data.table::rbindlist(class.results)
        res$..class = rep(names(class.results), each = ncol(X.recode))
      } else {
        res = extract.glmnet.effects(res$beta, best.index, x.recoded, self$x.interest)
      }
      res[res$beta != 0, ]
    },
    intervene = function() private$dataSample, 
    generatePlot = function() {
      p = ggplot(self$results) + 
        geom_col(aes(y = effect, x=feature.value)) + coord_flip()
      if (private$multiClass) p = p + facet_wrap("..class")
      p
    },
    weightSamples = function() {
      require("gower")
      1 - gower_dist(private$dataDesign, self$x.interest)
    }
  )
)



#' Lime prediction
#' 
#' Predict the response for newdata with the Lime model.
#' 
#' This function makes the Lime object call 
#' its iternal object$predict() method. 
#' For examples see \link{Lime}
#' @param object A Lime R6 object
#' @param newdata A data.frame for which to predict
#' @param ... Further arguments for the objects predict function
#' @return A data.frame with the predicted outcome. 
#' @seealso 
#' \link{Lime}
#' @importFrom stats predict
#' @export
predict.Lime = function(object, newdata = NULL, ...) {
  object$predict(newdata = newdata, ...)
}

#' Lime plot
#' 
#' plot.Lime() plots the feature effects of the Lime model.
#' 
#' For examples see \link{Lime}
#' @param object  A Lime R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{Lime}
plot.Lime = function(object) {
  object$plot()
}






