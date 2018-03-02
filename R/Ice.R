#' Individual conditional expectations (Ice)
#' 
#' \code{Ice} fits and plots individual conditional expectation curves for prediction models.
#' 
#' @format \code{\link{R6Class}} object.
#' @name Ice
#' 
#' @section Usage:
#' \preformatted{
#' ice = Ice$new(model, feature, grid.size = 10, center.at = NULL, run = TRUE)
#' 
#' plot(ice)
#' ice$results
#' print(ice)
#' ice$set.feature(2)
#' ice$center(1)
#' }
#' 
#' @section Arguments:
#' 
#' For Ice$new():
#' \describe{
#' \item{model}{Object of type \code{Predictor}. See \link{Predictor}}
#' \item{feature}{The feature name or index for which to compute the partial dependencies.}
#' \item{grid.size}{The size of the grid for evaluating the predictions}
#' \item{center.at}{The value for the centering of the plot. Numeric for numeric features, and the level name for factors.}
#' \item{run}{logical. Should the Interpretation method be run?}
#' }
#' 
#' 
#' @section Details:
#' The individual conditional expectation curves show how the prediction for each instance changes
#' when we vary a single feature.
#' 
#' To learn more about individual conditional expectation, 
#' read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/ice.html
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{feature.index}{The index of the features for which the partial dependence was computed.}
#' \item{feature.name}{The names of the features for which the partial dependence was computed.}
#' \item{feature.type}{The detected types of the features, either "categorical" or "numerical".}
#' \item{center.at}{The value for the centering of the plot. Numeric for numeric features, and the level name for factors.}
#' \item{grid.size}{The size of the grid.}
#' \item{results}{data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}.} 
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{center}{method to set the value(s) at which the ice computations are centered. See examples.}
#' \item{feature}{method to get/set the feature (index) for which to compute ice. See examples for usage.}
#' \item{plot()}{method to plot the individual conditional expectations. See \link{plot.Ice}}
#' }
#' @references 
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2013). Peeking Inside the Black Box: 
#' Visualizing Statistical Learning with Plots of Individual Conditional Expectation, 1-22. https://doi.org/10.1080/10618600.2014.907095 
#' @seealso 
#' \link{PartialDependence} for partial dependence plots (aggregated ice plots)
#' @importFrom dplyr left_join

#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' 
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the individual conditional expectations for the first feature
#' ice = Ice$new(mod, feature = "crim")
#' 
#' # Plot the results directly
#' plot(ice)
#' 
#' # You can center the Ice plot
#' ice$center(0)
#' plot(ice)
#' 
#' # Ice plots can be centered at initialization
#' ice = Ice$new(mod, feature = "crim", center=75)
#' plot(ice)
#' 
#' # Centering can also be removed
#' ice$center(NULL)
#' plot(ice)
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' if (require("ggplot2")) {
#' plot(ice) + theme_bw()
#' 
#' 
#' # If you want to do your own thing, just extract the data: 
#' iceData = ice$results
#' head(iceData)
#' ggplot(iceData) + 
#' geom_line(aes(x = crim, y = y.hat, group = ..individual, color = factor(..individual))) + 
#' scale_color_discrete(guide = "none")
#' }
#' # You can reuse the ice object for other features: 
#' ice$feature = "lstat"
#' plot(ice)
#' 
#' # Ice also works with multiclass classification
#' rf = randomForest(Species ~ ., data= iris, ntree=50)
#' predict.fun = function(obj, newdata) predict(obj, newdata, type = "prob")
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun)
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(Ice$new(mod, feature = "Sepal.Length"))
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun, class = "virginica")
#' plot(Ice$new(mod, feature = "Sepal.Length"))
#' 
#' # Ice plots can be centered: 
#' plot(Ice$new(mod, feature = "Sepal.Length", center = 1))
#' }
#' @export
NULL

#' @export

Ice = R6::R6Class("Ice",
  inherit = PartialDependence,
  public = list( 
    initialize = function(model, feature, grid.size = 10, center.at = NULL, run = TRUE) {
      checkmate::assert_number(center.at, null.ok = TRUE)
      private$anchor.value = center.at
      super$initialize(model = model,  feature=feature, run = run, 
        grid.size = grid.size)
    }, 
    center = function(center.at) {
      private$anchor.value = center.at
      private$flush()
      self$run()
    }
  ),
  private = list(
    generatePlot = function() {
      p = ggplot(self$results, mapping = aes_string(x = names(self$results)[1], 
        y = "y.hat", group = "..individual"))
      if (self$feature.type == "numerical") p = p + geom_line()
      else if (self$feature.type == "categorical") {
        p = p + geom_line(alpha = 0.2) 
      }
      if (private$multiClass) {
        p = p + facet_wrap("..class.name")
      } 
      p +  scale_y_continuous(expression(hat(y)))
    }, 
    intervene = function() {
      dataDesign = super$intervene()
      if (!is.null(private$anchor.value)) {
        dataDesign.anchor = private$dataSample
        dataDesign.anchor[self$feature.index] = private$anchor.value
        private$dataDesign.ids = c(private$dataDesign.ids, 1:nrow(private$dataSample))
        dataDesign = rbind(dataDesign, dataDesign.anchor)
      }
      dataDesign
    },
    aggregate = function() {
      X.id = private$dataDesign.ids
      X.results = private$dataDesign[self$feature.index]
      X.results$..individual = X.id
      if (private$multiClass) {
        y.hat.names = colnames(private$qResults)
        X.results = cbind(X.results, private$qResults)
        X.results = gather(X.results, key = "..class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        X.results["y.hat"]= private$qResults
        X.results["..class.name"] = 1
      }
      
      if (!is.null(private$anchor.value)) {
        X.aggregated.anchor = X.results[X.results[self$feature.name] == private$anchor.value, c("y.hat", "..individual", "..class.name")]
        names(X.aggregated.anchor) = c("anchor.yhat", "..individual", "..class.name")
        X.results = left_join(X.results, X.aggregated.anchor, by = c("..individual", "..class.name"))
        X.results$y.hat = X.results$y.hat - X.results$anchor.yhat
        X.results$anchor.yhat = NULL
      }
      
      # Remove class name column again if single output
      if (!private$multiClass) {
        X.results$..class.name = NULL
      }
      
      X.results
    },
    anchor.value = NULL
  ),
  active = list(
    center.at = function(x) {
      if(!missing(x)) warning("Please use $center() to change the value.")
      return(private$anchor.value)
    }
  )
)

#' Individual conditional expectation plots
#' 
#' plot.Ice() plots individiual expectation curves for each observation for one feature.
#' 
#' For examples see \link{Ice}
#' @param x The individual conditional expectation curves. An Ice R6 object
#' @param ... Further arguments for the objects plot function
#' @return ggplot2 plot object
#' @seealso 
#' \link{Ice}
plot.Ice = function(x, ...) {
  x$plot()
}

