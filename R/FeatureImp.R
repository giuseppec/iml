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
#' imp = FeatureImp$new(predictor, loss, method = "shuffle", n.repetitions = 3, run = TRUE)
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
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{loss: }{(`character(1)` | function)\cr The loss function. Either the name of a loss (e.g. "ce" for classification or "mse") or a loss function. See Details for allowed losses.}
#' \item{method: }{(`character(1)`\cr Either "shuffle" or "cartesian". See Details.}
#' \item{n.repetitions: }{`numeric(1)`\cr How often should the shuffling of the feature be repeated? Ignored if method is set to "cartesian".
#' The higher the number of repetitions the more stable the results will become.}
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
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
#' Since the feature importance is computed batch by batch, choose a lost for which an 
#' weighted average between instance can be computed (anything with a root outside doesn't work for example).
#' Allowed losses are: "ce", "mae", "mse", "mape", "msle", "percent_bias", "smape" 
#' 
#' @section Fields:
#' \describe{
#' \item{original.error: }{(`numeric(1)`)\cr The loss of the model before perturbing features.}
#' \item{predictor: }{(Predictor)\cr The prediction model that was analysed.}
#' \item{results: }{(data.frame)\cr data.frame with the results of the feature importance computation.}
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
#' @importFrom data.table copy rbindlist
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
#' mod = Predictor$new(tree, data = X, y = y, type = "prob") 
#' 
#' # For some models we have to specify additional arguments for the predict function
#' imp = FeatureImp$new(mod, loss = "ce")
#' plot(imp)
#' 
#' # For multiclass classification models, you can choose to only compute performance for one class. 
#' # Make sure to adapt y
#' mod = Predictor$new(tree, data = X, y = y == "virginica", 
#'  type = "prob", class = "virginica") 
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
    n.repetitions = NULL,
    initialize = function(predictor, loss, method = "shuffle", n.repetitions = 3, run = TRUE) {
      assert_choice(method, c("shuffle", "cartesian"))
      assert_number(n.repetitions)
      if (n.repetitions > predictor$data$n.rows) {
        message('Number of repetitions larger than number of unique permutations per row. 
          Switching to method = "cartesian"')
        method = "cartesian"
      }
      if (!inherits(loss, "function")) {
        ## Only allow metrics from Metrics package
        allowedLosses = c("ce", "mae", "mse", "mape", "msle", "percent_bias", "smape")
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
      self$n.repetitions = n.repetitions
      actual = private$sampler$y[[1]]
      predicted = private$q(self$predictor$predict(private$sampler$X))[[1]]
      # Assuring that levels are the same
      self$original.error = loss(actual, predicted)
      if(run) private$run(self$predictor$batch.size)
    }
  ),
  private = list(
    method = NULL,
    # for printing
    loss.string = NULL,
    q = function(pred) probs.to.labels(pred),
    combine.aggregations = function(agg, dat){
      if(is.null(agg)) { 
        return(dat) 
      } else {
        
      }
    },
    run = function(n){
      private$dataSample = private$getData()
      result = NULL
      X.inter.list = lapply(private$sampler$feature.names, function(i) {
        cartesian = ifelse(private$method == "cartesian", TRUE, FALSE)
        mg = MarginalGenerator$new(private$dataSample, private$dataSample, 
          features = i, n.sample.dist = self$n.repetitions, y = private$sampler$y, cartesian = cartesian)
        mg2 = mg$clone()
        while(!mg$finished) {
          dataDesign = mg$next.batch(n, y = TRUE)
          y = dataDesign[, private$sampler$y.names, with = FALSE]
          predictResults = self$predictor$predict(data.frame(dataDesign))
          private$multiClass = ifelse(ncol(predictResults) > 1, TRUE, FALSE)
          qResults = private$q(predictResults)
          
          # AGGREGATE measurements
          y.hat = qResults
          result.intermediate = data.table(feature = i, actual = y[[1]], predicted = predictResults[[1]])
          result.intermediate = result.intermediate[, list("permutation.error" = self$loss(actual, predicted), "n" = .N), by = feature]
          result = rbind(result, result.intermediate)
          result = result[, list("permutation.error" = sum(permutation.error * n)/sum(n), "n" = sum(n)), by = feature]
        }
        result
      })
      
      private$finished = TRUE
      result = rbindlist(X.inter.list)
      result$original.error = self$original.error
      result[, importance := permutation.error / self$original.error]
      result = result[order(result$importance, decreasing = TRUE),]
      # Removes the n column
      result = result[,list(feature, original.error, permutation.error, importance)]
      self$results = result
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


library("Metrics")

dt =  data.frame(actual = c(1,1,1,0,0, 1), predicted = c(1,0,1, 1,0, 0))

f1(dt$actual, dt$predicted)


dt =  data.frame(actual = c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6), predicted = c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2))
dt1 = dt[c(1,2),]
dt2 = dt[c(3:nrow(dt)),]
Metrics::smape(actual = dt$actual, predicted = dt$predicted)
  
d1 = Metrics::smape(actual = dt1$actual, predicted = dt1$predicted)
d2 = Metrics::smape(actual = dt2$actual, predicted = dt2$predicted)

(nrow(dt1)/nrow(dt)) * d1 +  (nrow(dt2)/nrow(dt)) * d2



