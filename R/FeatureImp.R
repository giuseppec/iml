#' Feature importance
#' 
#' @description 
#' \code{FeatureImp} computes feature importances for prediction models.
#' The importance is measured as the factor by which the model's prediction error increases when the feature is shuffled. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name FeatureImp
#' 
#' @section Usage:
#' \preformatted{
#' imp = FeatureImp$new(predictor, loss, method = "shuffle", run = TRUE)
#' 
#' plot(imp)
#' imp$results
#' print(imp)
#' } 
#' 
#' @section Arguments:
#' 
#' For FeatureImp$new():
#' \describe{
#' \item{predictor}{Object of type \code{Predictor}. See \link{Predictor}.}
#' \item{run}{logical. Should the Interpretation method be run?}
#' \item{loss}{The loss function. A string (e.g. "ce" for classification or "mse") or a function. See Details for allowed losses.}
#' \item{method}{Either "shuffle" or "cartesian". See Details.}
#' }
#' 
#' @section Details:
#' Read the Interpretable Machine Learning book to learn in detail about feature importance: 
#' \url{https://christophm.github.io/interpretable-ml-book/feature-importance.html}
#' 
#' Two permutation schemes are implemented: 
#' \itemize{
#' \item shuffle: A simple shuffling of the feature values, yielding n perturbed instances per feature (fast)
#' \item cartesian: Matching every instance with the feature value of all other instances, yielding n x (n-1) perturbed instances per feature (very slow)
#' }
#' 
#' The loss function can be either specified via a string, or by handing a function to \code{FeatureImp()}.
#' If you want to use your own loss function it should have this signature: function(actual, predicted).
#' Using the string is a shortcut to using loss functions from the \code{Metrics} package. 
#' Only use functions that return a single performance value, not a vector. 
#' Allowed losses are: "ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae", 
#' "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape"
#' See \code{library(help = "Metrics")} to get a list of functions. 
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{original.error}{The loss of the model before perturbing features.}
#' \item{predictor}{The prediction model that was analysed.}
#' \item{results}{data.frame with the results of the feature importance computation.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{loss(actual,predicted)}{The loss function. Can also be applied to data: \code{object$loss(actual, predicted)}}
#' \item{plot()}{method to plot the feature importances. See \link{plot.FeatureImp}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' @references 
#' Fisher, A., Rudin, C., and Dominici, F. (2018). Model Class Reliance: Variable Importance Measures for any Machine Learning Model Class, from the "Rashomon" Perspective. Retrieved from http://arxiv.org/abs/1801.01489
#' 
#' @import Metrics
#' @examples
#' if (require("rpart")) {
#' # We train a tree on the Boston dataset:
#' data("Boston", package  = "MASS")
#' tree = rpart(medv ~ ., data = Boston)
#' y = Boston$medv
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(tree, data = X, y = y)
#' 
#' # Compute feature importances as the performance drop in mean absolute error
#' imp = FeatureImp$new(mod, loss = "mae")
#' 
#' # Plot the results directly
#' plot(imp)
#' 
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' if (require("ggplot2")) {
#'   plot(imp) + theme_bw()
#'   # If you want to do your own thing, just extract the data: 
#'   imp.dat = imp$results
#'   head(imp.dat)
#'   ggplot(imp.dat, aes(x = feature, y = importance)) + geom_point() + 
#'   theme_bw()
#' }
#' 
#' # FeatureImp also works with multiclass classification. 
#' # In this case, the importance measurement regards all classes
#' tree = rpart(Species ~ ., data= iris)
#' X = iris[-which(names(iris) == "Species")]
#' y = iris$Species
#' predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
#' mod = Predictor$new(tree, data = X, y = y, predict.fun) 
#' 
#' # For some models we have to specify additional arguments for the predict function
#' imp = FeatureImp$new(mod, loss = "ce")
#' plot(imp)
#' 
#' # For multiclass classification models, you can choose to only compute performance for one class. 
#' # Make sure to adapt y
#' mod = Predictor$new(tree, data = X, y = y == "virginica", 
#'   predict.fun = predict.fun, class = "virginica") 
#' imp = FeatureImp$new(mod, loss = "ce")
#' plot(imp)
#' }
NULL

#' @export

FeatureImp = R6::R6Class("FeatureImp", 
  inherit = InterpretationMethod,
  public = list(
    loss = NULL,
    original.error = NULL,
    initialize = function(predictor, loss, method = "shuffle", run = TRUE) {
      assert_choice(method, c("shuffle", "cartesian"))
      
      if (!inherits(loss, "function")) {
        ## Only allow metrics from Metrics package
        allowedLosses = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae", 
          "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape")
        checkmate::assert_choice(loss, allowedLosses)
        private$loss.string  = loss
        loss = getFromNamespace(loss, "Metrics")
      } else {
        private$loss.string = head(loss)
      }
      if (is.null(predictor$data$y)) {
        stop("Please call Predictor$new() with the y target vector.")
      }
      super$initialize(predictor = predictor)
      self$loss = private$set.loss(loss)
      private$method = method
      private$getData = private$sampler$get.xy
      actual = private$sampler$y[[1]]
      predicted = private$q(self$predictor$predict(private$sampler$X))[[1]]
      # Assuring that levels are the same
      self$original.error = loss(actual, predicted)
      if(run) self$run()
    }
  ),
  private = list(
    method = NULL,
    # for printing
    loss.string = NULL,
    shuffleFeature = function(feature.name, method) {
      if (method == "shuffle") {
        X.inter = private$dataSample
        X.inter[feature.name] = X.inter[sample(1:nrow(private$dataSample)), feature.name]
      } else if (method == "cartesian") {
        n = nrow(private$dataSample)
        row.indices = rep(1:n, times = n)
        replace.indices = rep(1:n, each = n)
        # Indices of instances to keep. Removes those where instance matched with own value
        keep.indices = row.indices != replace.indices
        X.inter = private$dataSample[row.indices, ]
        X.inter[feature.name] = X.inter[replace.indices, feature.name]
        X.inter = X.inter[keep.indices,]
      } else {
        stop(sprintf("%s method not implemented"))
      }
      X.inter$..feature = feature.name
      X.inter 
    },
    q = function(pred) probs.to.labels(pred),
    intervene = function() {
      X.inter.list = lapply(private$sampler$feature.names, 
        function(i) private$shuffleFeature(i, method = private$method))
      data.frame(data.table::rbindlist(X.inter.list))
    },
    aggregate = function() {
      y = private$dataDesign[private$sampler$y.names]
      y.hat = private$qResults
      # For classification we work with the class labels instead of probs
      result = data.frame(feature = private$dataDesign$..feature, actual = y[[1]], 
        predicted = y.hat[[1]])
      
      result.grouped  = group_by_(result, "feature")
      result = summarise(result.grouped, original.error = self$original.error, permutation.error = self$loss(actual, predicted), 
        importance = permutation.error / self$original.error)
      result = result[order(result$importance, decreasing = TRUE),]
      result
    },
    generatePlot = function(sort = TRUE, ...) {
      res = self$results
      if (sort) {
        res$feature = factor(res$feature, levels = res$feature[order(res$importance)])
      }
      ggplot(res, aes(y = feature, x = importance)) + geom_point()+ 
        geom_segment(aes(y = feature, yend = feature, x=1, xend = importance)) + 
        scale_x_continuous("Feature Importance") + 
        scale_y_discrete("Feature")
    }, 
    set.loss = function(loss) {
      self$loss = loss
    }, 
    printParameters = function() {
      cat("error function:", private$loss.string)
    }
  )
)


#' Plot Feature Importance
#' 
#' plot.FeatureImp() plots the feature importance results of a FeatureImp object.
#' 
#' @param x A FeatureImp R6 object
#' @param sort logical. Should the features be sorted in descending order? Defaults to TRUE.
#' @param ... Further arguments for the objects plot function
#' @return ggplot2 plot object
#' @export
#' @importFrom dplyr group_by_
#' @seealso 
#' \link{FeatureImp}
#' @examples  
#' if (require("rpart")) {
#' # We train a tree on the Boston dataset:
#' data("Boston", package  = "MASS")
#' tree = rpart(medv ~ ., data = Boston)
#' y = Boston$medv
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(tree, data = X, y = y)
#' 
#' # Compute feature importances as the performance drop in mean absolute error
#' imp = FeatureImp$new(mod, loss = "mae")
#' 
#' # Plot the results directly
#' plot(imp)
#' }
plot.FeatureImp = function(x, sort = TRUE, ...) {
  x$plot(sort = sort, ...)
}






