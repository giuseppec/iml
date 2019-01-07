#' Effect of all features on the model predictions
#' 
#' \code{FeatureEffects} computes and plots (individual) feature effects of prediction models. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name FeatureEffect
#' 
#' @section Usage:
#' \preformatted{
#' effect = FeatureEffects$new(predictor, method = "ale", 
#'     grid.size = 20, center.at = NULL, run = TRUE, parallel = FALSE)
#' 
#' plot(effect)
#' effect$results
#' print(effect)
#' }
#' 
#' @section Arguments:
#' 
#' For FeatureEffects$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{method: }{(`character(1)`)\cr 
#' 'ale' for accumulated local effects (the default), 
#' 'pdp' for partial dependence plot, 
#' 'ice' for individual conditional expectation curves,
#' 'pdp+ice' for partial dependence plot and ice curves within the same plot.}
#' \item{center.at: }{(`numeric(1)`)\cr Value at which the plot should be centered. Ignored in the case of two features.}
#' \item{grid.size: }{(`numeric(1)` | `numeric(2)`)\cr The size of the grid for evaluating the predictions}
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
#' \item{parallel: }{`logical(1)`\cr Should the method be executed in parallel? If TRUE, requires a cluster to be registered, see ?foreach::foreach.}
#' }
#' 
#' @section Details:
#' 
#' The FeatureEffect class compute the effect a feature has on the prediction. 
#' Different methods are implemented:
#' \itemize{
#' \item{Accumulated Local Effect (ALE) plots}
#' \item{Partial Dependence Plots (PDPs)}
#' \item{Individual Conditional Expectation (ICE) curves}
#' }
#'  
#' FeatureEffects calls FeatureEffect for each feature.
#' See ?FeatureEffect for Detais what's actually computed.
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{method: }{(`character(1)`)\cr
#' 'ale' for accumulated local effects, 
#' 'pdp' for partial dependence plot, 
#' 'ice' for individual conditional expectation curves,
#' 'pdp+ice' for partial dependence plot and ice curves within the same plot.}
#' \item{feature.name: }{(`character(1)` | `character(2)`)\cr The names of the features for which the partial dependence was computed.}
#' \item{feature.type: }{(`character(1)` | `character(2)`)\cr The detected types of the features, either "categorical" or "numerical".}
#' \item{grid.size: }{(`numeric(1)` | `numeric(2)`)\cr The size of the grid.}
#' \item{center.at: }{(`numeric(1)` | `character(1)`)\cr The value for the centering of the plot. Numeric for numeric features, and the level name for factors.}
#' \item{n.features: }{(`numeric(1)`)\cr The number of features (either 1 or 2)}
#' \item{predictor: }{(Predictor)\cr The prediction model that was analysed.}
#' \item{results: }{(data.frame)\cr data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom partial dependence plots.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{center()}{method to set the value at which the ice computations are centered. See examples.}
#' \item{set.feature()}{method to get/set feature(s) (by index) fpr  which to compute pdp. See examples for usage.}
#' \item{plot()}{method to plot the partial dependence function. See \link{plot.FeatureEffect}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' @seealso 
#' \link{plot.FeatureEffect} 
#' 
#' @references 
#' Apley, D. W. 2016. "Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models." ArXiv Preprint.
#' 
#' Friedman, J.H. 2001. "Greedy Function Approximation: A Gradient Boosting Machine." Annals of Statistics 29: 1189-1232.
#' 
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2013). Peeking Inside the Black Box: 
#' Visualizing Statistical Learning with Plots of Individual Conditional Expectation, 1-22. https://doi.org/10.1080/10618600.2014.907095 
#' @importFrom data.table melt setkeyv
#' @import ggplot2
#' @importFrom stats cmdscale ecdf quantile
#' @export
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the accumulated local effects for the first feature
#' eff = FeatureEffects$new(mod, grid.size = 30, run = TRUE)
#' eff$plot()
#' 
#' # Again, but this time with a partial dependence plot and ice curves
#' eff = FeatureEffect$new(mod, feature = "rm", method = "pdp+ice", grid.size = 30)
#' plot(eff)
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' if (require("ggplot2")) {
#'  plot(eff) + 
#'  # Adds a title
#'  ggtitle("Partial dependence") + 
#'  # Adds original predictions
#'  geom_point(data = Boston, aes(y = mod$predict(Boston)[[1]], x = rm), 
#'  color =  "pink", size = 0.5)
#' }
#' 
#' # If you want to do your own thing, just extract the data: 
#' eff.dat = eff$results
#' head(eff.dat)
#' 
#' # You can reuse the pdp object for other features: 
#' eff$set.feature("lstat")
#' plot(eff)
#'#' eff = FeatureEffects$new
#' # Only plotting the aggregated partial dependence:  
#' eff = FeatureEffect$new(mod, feature = "crim", method = "pdp")
#' eff$plot() 
#'
#' # Only plotting the individual conditional expectation:  
#' eff = FeatureEffect$new(mod, feature = "crim", method = "ice")
#' eff$plot() 
#'   
#' # Accumulated local effects and partial dependence plots support up to two features: 
#' eff = FeatureEffect$new(mod, feature = c("crim", "lstat"))  
#' plot(eff)
#' 
#' 
#' # Partial dependence plots also works with multiclass classification
#' rf = randomForest(Species ~ ., data = iris, ntree=50)
#' mod = Predictor$new(rf, data = iris, type = "prob")
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(FeatureEffect$new(mod, feature = "Petal.Width"))
#'
#' # Partial dependence plots support up to two features: 
#' eff = FeatureEffect$new(mod, feature = c("Sepal.Length", "Petal.Length"))
#' eff$plot()   
#' 
#' # show where the actual data lies
#' eff$plot(show.data = TRUE)   
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Predictor$new(rf, data = iris, type = "prob", class = 1)
#' plot(FeatureEffect$new(mod, feature = "Sepal.Length"))
#' }
NULL



#' @export


FeatureEffects = R6::R6Class("FeatureEffects", 
  inherit = InterpretationMethod,
  public = list(
    ice = NULL,
    aggregation = NULL,
    grid.size = NULL, 
    method  = NULL,
    # The named list of FeatureEffect
    effects = NULL,
    initialize = function(predictor, method = "ale", center.at = NULL, 
      grid.size = 20, run = run, parallel = FALSE) {
      
      # TODO: Implement possibility to select features here
      
      assert_numeric(grid.size, min.len = 1)
      assert_number(center.at, null.ok = TRUE)
      private$parallel = parallel
      assert_choice(method, c("ale", "pdp", "ice", "pdp+ice"))
      self$grid.size = grid.size
      self$method = method
      super$initialize(predictor)
      if(run) self$run()
    }, 
    
    center = function(center.at) {
      if(self$method == "ale") {
        warning("Centering only works for only for PDPs and ICE, but not for ALE Plots, .")
        return(NULL)
      }
      private$anchor.value = center.at
      private$flush()
      self$run(self$predictor$batch.size)
    },
    run = function() {
      predictor = self$predictor
      method = self$method
      center.at = self$center.at
      grid.size = self$grid.size
      feature.names = predictor$data$feature.names
      `%mypar%` = private$get.parallel.fct(private$parallel)
      feature_effect = function(x, predictor, method, center.at, grid.size) {
        FeatureEffect$new(feature = x, predictor = predictor, method = method, center.at = center.at, 
          grid.size = grid.size, run = TRUE)
      }
      effects = foreach(feature = feature.names,
        .packages = devtools::loaded_packages()$package, .inorder = FALSE) %mypar%
        feature_effect(feature, predictor = predictor, method = method, center.at = center.at, 
          grid.size = grid.size)
      self$effects = effects
      names(self$effects) = predictor$data$feature.names
      self$results = lapply(self$effects, function(x) {
        res = x$results
        fname.index = which(colnames(res) %in% names(self$effects))
        res$.feature = colnames(res)[fname.index]
        colnames(res)[fname.index] = ".feature"
        res
      })
    
    }
  ),
  private = list(
    printParameters = function() {
      # TODO: Implement
    },
    # make sure the default arguments match with plot.FeatureEffect
    generatePlot = function(rug = TRUE, show.data=FALSE) {
      # TODO: Build up a gtable based on user input (nrow or ncol)
      # create ggplot grid from each graphic
    }
  ), 
  active = list()
)



#' Plot FeatureEffect
#' 
#' plot.FeatureEffect() plots the results of a FeatureEffect object.
#' 
#' @param x A FeatureEffect R6 object
#' @param rug [logical] Should a rug be plotted to indicate the feature distribution? The rug will be jittered a bit, so the location may not be exact, 
#' but it avoids overplotting.
#' @param show.data Should the data points be shown? Only affects 2D plots, and ignored for 1D plots, because rug has the same information.
#' @return ggplot2 plot object
#' @seealso 
#' \link{FeatureEffect}
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the partial dependence for the first feature
#' eff = FeatureEffect$new(mod, feature = "crim")
#' 
#' # Plot the results directly
#' plot(eff)
#' }
plot.FeatureEffect = function(x, rug = TRUE, show.data = FALSE) {
  x$plot(rug, show.data)
}
