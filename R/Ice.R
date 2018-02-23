#' Individual conditional expectations (Ice)
#' 
#' \code{Ice} fits and plots individual conditional expectation curves for prediction models.
#' 
#' @format \code{\link{R6Class}} object.
#' @name Ice
#' 
#' @section Usage:
#' \preformatted{
#' ice = Ice$new(model, data, feature, grid.size = 10, center.at = NULL, run = TRUE)
#' 
#' plot(ice)
#' ice$data
#' print(ice)
#' ice$feature = 2
#' ice$center.at = 1
#' }
#' 
#' @section Arguments:
#' 
#' For Ice$new():
#' \describe{
#' \item{model}{Object of type \code{Model}. See \link{Model}}
#' \item{data}{data.frame with the data for the prediction model.}
#' \item{feature}{The feature index for which to compute the partial dependencies.}
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
#' \item{feature.names}{The names of the features for which the partial dependence was computed.}
#' \item{feature.type}{The detected types of the features, either "categorical" or "numerical".}
#' \item{center.at}{The value for the centering of the plot. Numeric for numeric features, and the level name for factors.}
#' \item{grid.size}{The size of the grid.}
#' \item{n.features}{The number of features (either 1 or 2)}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{center.at}{method to set the value(s) at which the ice computations are centered. See examples.}
#' \item{grid.size}{The size of the grid.}
#' \item{center}{method to get/set the feature value at which the ice computation should be centered. See examples for usage.}
#' \item{feature}{method to get/set the feature (index) for which to compute ice. See examples for usage.}
#' \item{data()}{method to extract the results of the individual conditional expectation. 
#' Returns a data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom Ice plots.}
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
#' mod = Model$new(rf)
#' 
#' # Compute the individual conditional expectations for the first feature
#' ice = Ice$new(mod, Boston, feature = 1)
#' 
#' # Plot the results directly
#' plot(ice)
#' 
#' # You can center the Ice plot
#' ice$center.at = 0
#' plot(ice)
#' 
#' # Ice plots can be centered at initialization
#' ice = Ice$new(mod, Boston, feature = 1, center=75)
#' plot(ice)
#' 
#' # Centering can also be removed
#' ice$center.at = NULL
#' plot(ice)
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' library("ggplot2")
#' plot(ice) + theme_bw()
#' 
#' # If you want to do your own thing, just extract the data: 
#' iceData = ice$data()
#' head(iceData)
#' ggplot(iceData) + 
#' geom_line(aes(x = crim, y = y.hat, group = ..individual, color = factor(..individual))) + 
#' scale_color_discrete(guide = "none")
#' 
#' # You can reuse the ice object for other features: 
#' ice$feature = 2
#' plot(ice)
#' 
#' # Ice also works with multiclass classification
#' library("randomForest")
#' rf = randomForest(Species ~ ., data= iris, ntree=50)
#' mod = Model$new(rf, predict.args = list(type = 'prob'))
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(Ice$new(mod, iris, feature = 1))
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Model$new(rf, predict.args = list(type = 'prob'), class = 1)
#' plot(Ice$new(mod, iris, feature = 1))
#' 
#' # Ice plots can be centered: 
#' plot(Ice$new(mod, iris, feature = 1, center = 1))
#' }
#' @export
NULL

#' @export

Ice = R6::R6Class("Ice",
  inherit = PartialDependence,
  public = list( 
    initialize = function(model, data, feature, grid.size = 10, center.at = NULL, run = TRUE) {
      checkmate::assert_number(center.at, null.ok = TRUE)
      private$anchor.value = center.at
      assert_count(feature)
      super$initialize(model = model, data = data, feature=feature, run = run, 
        grid.size = grid.size)
    }
  ),
  private = list(
    generate.plot = function() {
      p = ggplot(private$results, mapping = aes_string(x = names(private$results)[1], 
        y = "y.hat", group = "..individual"))
      if (self$feature.type == "numerical") p = p + geom_line()
      else if (self$feature.type == "categorical") p = p + geom_line(alpha = 0.2) + geom_point()
      
      if (private$multi.class) {
        p + facet_wrap("..class.name")
      } else {
        p
      }
    }, 
    intervene = function() {
      X.design = super$intervene()
      if (!is.null(private$anchor.value)) {
        X.design.anchor = private$X.sample
        X.design.anchor[self$feature.index] = private$anchor.value
        private$X.design.ids = c(private$X.design.ids, 1:nrow(private$X.sample))
        X.design = rbind(X.design, X.design.anchor)
      }
      X.design
    },
    aggregate = function() {
      X.id = private$X.design.ids
      X.results = private$X.design[self$feature.index]
      X.results$..individual = X.id
      if (private$multi.class) {
        y.hat.names = colnames(private$Q.results)
        X.results = cbind(X.results, private$Q.results)
        X.results = gather(X.results, key = "..class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        X.results["y.hat"]= private$Q.results
        X.results["..class.name"] = 1
      }
      
      if (!is.null(private$anchor.value)) {
        X.aggregated.anchor = X.results[X.results[self$feature.names] == private$anchor.value, c("y.hat", "..individual", "..class.name")]
        names(X.aggregated.anchor) = c("anchor.yhat", "..individual", "..class.name")
        X.results = left_join(X.results, X.aggregated.anchor, by = c("..individual", "..class.name"))
        X.results$y.hat = X.results$y.hat - X.results$anchor.yhat
        X.results$anchor.yhat = NULL
      }
      
      # Remove class name column again if single output
      if (!private$multi.class) {
        X.results$..class.name = NULL
      }
      
      X.results
    },
    anchor.value = NULL
  ),
  active = list(
    center.at = function(anchor.value) {
      if (missing(anchor.value)) return(private$anchor.value)
      private$anchor.value = anchor.value
      private$flush()
      self$run()
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

