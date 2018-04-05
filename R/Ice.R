#' Individual conditional expectations (Ice)
#' 
#' \code{Ice} fits and plots individual conditional expectation curves for prediction models.
#' 
#' @format \code{\link{R6Class}} object.
#' @name Ice
#' 
#' @section Usage:
#' \preformatted{
#' ice = Ice$new(predictor, feature, grid.size = 20, center.at = NULL, run = TRUE)
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
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{feature: }{(`character(1)`)\cr The feature name or index for which to compute the individual conditional expectations.}
#' \item{grid.size: }{(`numeric(1)`)\cr The size of the grid for evaluating the predictions}
#' \item{center.at: }{(`numeric(1)`)\cr The value for the centering of the plot. Numeric for numeric features, and the level name for factors.}
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
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
#' \item{predictor}{The prediction model that was analysed.}
#' \item{results}{data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}.} 
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{center()}{method to set the value at which the ice computations are centered. See examples.}
#' \item{set.feature()}{method to set the feature (index) for which to compute individual conditional expectations See examples for usage.}
#' \item{plot()}{method to plot the individual conditional expectations. See \link{plot.Ice}.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' @references 
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2013). Peeking Inside the Black Box: 
#' Visualizing Statistical Learning with Plots of Individual Conditional Expectation, 1-22. https://doi.org/10.1080/10618600.2014.907095 
#' @seealso 
#' \link{PartialDependence} for partial dependence plots (aggregated ice plots)

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
#' ice = Ice$new(mod, feature = "crim", center = 75)
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
#' ice$set.feature("lstat")
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
  inherit = Partial,
  public = list( 
    initialize = function(predictor, feature, grid.size = 20, center.at = NULL, run = TRUE) {
      .Deprecated("Partial", msg = "The use of the 'Ice' class is deprecated and it will 
        be removed starting from version 0.4. Please use the 'Partial' class instead.")
      super$initialize(predictor, feature, ice = TRUE, aggregation = "none", 
        grid.size = grid.size, center.at = center.at, run = TRUE)
    }
  )
)

#' Plot ICE (Individual Conditional Expectation)
#' 
#' plot.Ice() plots the individiual expectation results from an Ice object.
#' 
#' @param x An Ice R6 object
#' @param rug [logical] Should a rug be plotted to indicate the feature distribution?
#' @return ggplot2 plot object
#' @seealso 
#' \link{Ice}
#' @examples 
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
#' }
plot.Ice = function(x, rug = TRUE) {
  x$plot(rug)
}

