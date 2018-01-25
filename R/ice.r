#' Individual conditional expectations (ICE)
#' 
#' @description 
#' Fits and plots individual conditional expectation function on an arbitrary machine learning model
#' 
#' @details
#' Machine learning model try to learn the relationship \eqn{y = f(X)}. We can't visualize 
#' the learned \eqn{\hat{f}} directly for an individual, high-dimensional point \eqn{x_i}. 
#' 
#' But we can take one of the input features of an observation and change its value. 
#' We try out a grid of different values and observe the predicted outcome. 
#' This gives us the predicted \eqn{\hat{y}} as a function of feature \eqn{X_j}, which we can plot as a line. 
#' The \code{ice} method repeats this for all the observations in the dataset and plots all the lines in the same plot.
#' 
#' Mathematically, we split up the learned function into its parts:
#' \deqn{f(x_i) = f_1(x_{i,1}) + \ldots + f_p(x_{i,p}) + f_{1, 2}(x_{i,1}, x_{i,2}) + \ldots + f_{p-1, p}(x_{i,p-1}, x_{p}) + \ldots + f_{1\ldotsp}(x_{i,1\ldotsX_p})}, 
#' 
#' And we can isolate the individual conditional expectation of \eqn{y} on a single \eqn{X_j}: \eqn{f_j(X_j)} and plot it. 
#' 
#' Partial dependence plots (\link{\code{pdp}}) are the averaged lines of ice curves. 
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
#' data() contains a data.frame with columns for the feature grid, the predicted outcome, an identifier
#' for the individual observation and optionally an identfier for the predicted class ('..class.name')
#' @examples
#' 
#' @seealso 
#' \link{pdp} for partial dependence plots.
#' 
#' @examples
#' # We train a random forest on the Boston dataset:
#' library("randomForest")
#' data("Boston", package  = "MASS")
#' mod = randomForest(medv ~ ., data = Boston, ntree = 50)
#' 
#' # Compute the individual conditional expectations for the first feature
#' ice.obj = ice(mod, Boston, feature = 1)
#' 
#' # Plot the results directly
#' plot(ice.obj)
#' 
#' # You can center the ICE plot
#' ice.obj$center.at = 0
#' plot(ice.obj)
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' library("ggplot2")
#' plot(ice.obj)
#' 
#' # If you want to do your own thing, just extract the data: 
#' ice.dat = ice.obj$data()
#' head(ice.dat)
#' ggplot(ice.dat) + geom_line(aes(x = crim, y = y.hat, group = ..individual, color = factor(..individual))) + 
#' scale_color_discrete(guide = "none")
#' 
#' # You can reuse the ice object for other features: 
#' ice.obj$feature = 2
#' plot(ice.obj)
#' 
#' # Partial dependence plots support up to two features: 
#' ice.obj = ice(mod, Boston, feature = c(1,2))  
#' 
#' # ICE also works with multiclass classification
#' library("randomForest")
#' mod = randomForest(Species ~ ., data= iris, ntree=50)
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(ice(mod, iris, feature = 1, predict.args = list(type = 'prob')))
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' plot(ice(mod, iris, feature = 1, class = 1, predict.args = list(type = 'prob')))
#' 
#' # ICE plots can be centered: 
#' plot(ice(mod, iris, feature = 1, center = 1))
#' 
#' @importFrom dplyr left_join
#' @export
ice = function(object, X, feature, grid.size=10, center.at = NULL, class=NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  obj = ICE$new(predictor = pred, sampler = samp, anchor.value = center.at,  feature = feature, grid.size = grid.size)
  obj$run()
  obj
}




ICE = R6::R6Class('ICE',
  inherit = PDP,
  public = list( 
    initialize = function(feature, anchor.value = NULL, ...){
      checkmate::assert_number(anchor.value, null.ok = TRUE)
      private$anchor.value = anchor.value
      assert_count(feature)
      super$initialize(feature=feature, ...)
    }
  ),
  private = list(
    generate.plot = function(){
      p = ggplot(private$results, mapping = aes_string(x = names(private$results)[1], y = 'y.hat', group = '..individual'))
      if(self$feature.type == 'numerical') p = p + geom_line()
      else if (self$feature.type == 'categorical') p = p + geom_line(alpha = 0.2) + geom_point()
      
      if(ncol(private$Q.results) > 1){
        p + facet_wrap("..class.name")
      } else {
        p
      }
    }, 
    intervene = function(){
      X.design = super$intervene()
      if(!is.null(private$anchor.value)) {
        X.design.anchor = private$X.sample
        X.design.anchor[self$feature.index] = private$anchor.value
        private$X.design.ids = c(private$X.design.ids, 1:nrow(private$X.sample))
        X.design = rbind(X.design, X.design.anchor)
      }
      X.design
    },
    aggregate = function(){
      X.id = private$X.design.ids
      X.results = private$X.design[self$feature.index]
      X.results$..individual = X.id
      if(ncol(private$Q.results) > 1){
        y.hat.names = colnames(private$Q.results)
        X.results = cbind(X.results, private$Q.results)
        X.results = gather(X.results, key = "..class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        X.results['y.hat']= private$Q.results
        X.results['..class.name'] = 1
      }
      
      if(!is.null(private$anchor.value)){
        X.aggregated.anchor = X.results[X.results[self$feature.names] == private$anchor.value, c('y.hat', '..individual', '..class.name')]
        names(X.aggregated.anchor) = c('anchor.yhat', '..individual', '..class.name')
        X.results = left_join(X.results, X.aggregated.anchor, by = c('..individual', '..class.name'))
        X.results$y.hat = X.results$y.hat - X.results$anchor.yhat
        X.results$anchor.yhat = NULL
      }
      
      # Remove class name column again if single output
      if(ncol(private$Q.results) == 1){
        X.results$..class.name = NULL
      }
      
      X.results
    },
    anchor.value = NULL
  ),
  active = list(
    center.at = function(anchor.value){
      private$anchor.value = anchor.value
      private$flush()
      self$run()
    }
  )
)

