#' Individual conditional expectations (Ice)
#' 
#' @description 
#' makeIce() fits and plots individual conditional expectation function on an arbitrary machine learning model
#' 
#' @details
#' Machine learning model try to learn the relationship \eqn{y = f(X)}. We can't visualize 
#' the learned \eqn{\hat{f}} directly for an individual, high-dimensional point \eqn{x_i}. 
#' 
#' But we can take one of the input features of an observation and change its value. 
#' We try out a grid of different values and observe the predicted outcome. 
#' This gives us the predicted \eqn{\hat{y}} as a function of feature \eqn{X_j}, which we can plot as a line. 
#' The \code{makeIce} method repeats this for all the observations in the dataset and plots all the lines in the same plot.
#' 
#' Mathematically, we split up the learned function into its parts:
#' \deqn{f(x_i) = f_1(x_{i,1}) + \ldots + f_p(x_{i,p}) + f_{1, 2}(x_{i,1}, x_{i,2}) + \ldots + f_{p-1, p}(x_{i,p-1}, x_{p}) + \ldots + f_{1\ldots p}(x_{i,1\ldots X_p})}, 
#' 
#' And we can isolate the individual conditional expectation of \eqn{y} on a single \eqn{X_j}: \eqn{f_j(X_j)} and plot it. 
#' 
#' Partial dependence plots (\link{makePartialDependence}) are the averaged lines of ice curves. 
#'  The returned object can be plotted is a \code{ggplot}
#' object. This means it can be plotted directly or be extended using ggplots \code{+} operator.   
#' To learn more about partial dependence plot, read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/ice.html
#' 
#' @param feature The index of the feature of interest.
#' @template arg_grid.size 
#' @param center.at The value for the centering of the plot. Numeric for numeric features, and the level name for factors.
#' @return An individual conditional expectation object
#' @template args_experiment_wrap
#' @return 
#' An Ice object (R6). Its methods and variables can be accessed with the \code{$}-operator:
#' \item{feature.name}{The feature name for which the partial dependence was computed.}
#' \item{feature.type}{The detected type of the feature, either "categorical" or "numerical".}
#' \item{feature.index}{The index of the feature for which the individual conditional expectations weree computed.}
#' \item{center.at}{The features value(s) at which the ice computations are centered.}
#' \item{grid.size}{The size of the grid.}
#' \item{sample.size}{The number of instances sampled from data X.}
#' \item{center}{method to get/set the feature value at which the ice computation should be centered. See examples for usage.}
#' \item{feature}{method to get/set the feature (index) for which to compute ice. See examples for usage.}
#' \item{data()}{method to extract the results of the partial dependence plot. 
#' Returns a data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom partial dependence plots.}
#' \item{plot()}{method to plot the partial dependence function. See \link{plot.PartialDependence}}
#' @references 
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2013). Peeking Inside the Black Box: 
#' Visualizing Statistical Learning with Plots of Individual Conditional Expectation, 1-22. https://doi.org/10.1080/10618600.2014.907095 
#' @seealso 
#' \link{makePartialDependence} for partial dependence plots (aggregated ice plots)
#' 
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' 
#' data("Boston", package  = "MASS")
#' mod = randomForest(medv ~ ., data = Boston, ntree = 50)
#' 
#' # Compute the individual conditional expectations for the first feature
#' ice = makeIce(mod, Boston, feature = 1)
#' 
#' # Plot the results directly
#' plot(ice)
#' 
#' # You can center the Ice plot
#' ice$center.at = 0
#' plot(ice)
#' 
#' # Ice plots can be centered at initialization
#' ice = makeIce(mod, Boston, feature = 1, center=75)
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
#' mod = randomForest(Species ~ ., data= iris, ntree=50)
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(makeIce(mod, iris, feature = 1, predict.args = list(type = 'prob')))
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' plot(makeIce(mod, iris, feature = 1, class = 1, predict.args = list(type = 'prob')))
#' 
#' # Ice plots can be centered: 
#' plot(makeIce(mod, iris, feature = 1, center = 1, predict.args = list(type = 'prob')))
#' }
#' @importFrom dplyr left_join
#' @export
NULL

#' @export

Ice = R6::R6Class("Ice",
  inherit = PartialDependence,
  public = list( 
    initialize = function(predictor, data, feature, grid.size = 10, 
      anchor.value = NULL, center.at = NULL, run = TRUE) {
      checkmate::assert_number(center.at, null.ok = TRUE)
      private$anchor.value = center.at
      assert_count(feature)
      super$initialize(predictor = predictor, data = data, feature=feature, run = run, 
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
#' For examples see \link{makeIce}
#' @param x The individual conditional expectation curves. An Ice R6 object
#' @param ... Further arguments for the objects plot function
#' @return ggplot2 plot object
#' @seealso 
#' \link{makeIce}
plot.Ice = function(x, ...) {
  x$plot()
}

