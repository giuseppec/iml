#' Effect of all features on predictions
#' 
#' \code{FeatureEffects} computes feature effects for multiple features at once.
#' 
#' @format \code{\link{R6Class}} object.
#' @name FeatureEffects
#' 
#' @section Usage:
#' \preformatted{
#' effects = FeatureEffects$new(predictor, features = NULL, method = "ale", 
#'     grid.size = 20, center.at = NULL, parallel = FALSE)
#' 
#' plot(effects)
#' effects$results
#' print(effects)
#' }
#' 
#' @section Arguments:
#' 
#' For FeatureEffects$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{features: }{(`character()`)\cr 
#' The names of the features for which the effects should be computed.
#' Default is all features used in the prediction model.}
#' \item{method: }{(`character(1)`)\cr 
#' 'ale' for accumulated local effects (the default), 
#' 'pdp' for partial dependence plot, 
#' 'ice' for individual conditional expectation curves,
#' 'pdp+ice' for partial dependence plot and ice curves within the same plot.}
#' \item{center.at: }{(`numeric(1)`)\cr Value at which the plot should be centered.}
#' \item{grid.size: }{(`numeric(1)` | `numeric(2)`)\cr The size of the grid for evaluating the predictions}
#' \item{parallel: }{`logical(1)`\cr Should the method be executed in parallel? If TRUE, requires a cluster to be registered, see ?foreach::foreach.}
#' }
#' 
#' @section Details:
#' 
#' FeatureEffects computes the effects for all given features on the model prediction.
#' FeatureEffects is a convenience class that calls FeatureEffect multiple times.
#' See ?FeatureEffect for details what's actually computed.
#' 
#' Only first-order effects can be computed with the FeatureEffects interface. 
#' If you are intereated in the visualization of interactions between two features, directly use FeatureEffect.
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{method: }{(`character(1)`)\cr
#' 'ale' for accumulated local effects, 
#' 'pdp' for partial dependence plot, 
#' 'ice' for individual conditional expectation curves,
#' 'pdp+ice' for partial dependence plot and ice curves within the same plot.}
#' \item{features: }{(`character()`)\cr
#' The names of the features for which the effects were computed.}
#' \item{grid.size: }{(`numeric(1)` | `numeric(2)`)\cr
#' The size of the grid.}
#' \item{center.at: }{(`numeric(1)` | `character(1)`)\cr
#' The value for the centering of the plot. Numeric for numeric features, and the level name for factors.}
#' \item{predictor: }{(Predictor)\cr
#' The prediction model that was analysed.}
#' \item{results: }{(list)\cr
#' A list with the results of each feature effect. Each entry is a data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom effect plots.}
#' \item{effects: }{(list)\cr
#' A list of the FeatureEffect objects for each feature. See ?FeatureEffect what you can do with them (e.g. plot them individually).
#' }
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{plot()}{method to plot the all effects. See \link{plot.FeatureEffects}}
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
#' if (require("rpart")) {
#' data("Boston", package  = "MASS")
#' rf = rpart(medv ~ ., data = Boston)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the accumulated local effects for all features
#' eff = FeatureEffects$new(mod)
#' eff$plot()
#' 
#' \dontrun{
#' # Again, but this time with a partial dependence plot
#' eff = FeatureEffects$new(mod, method = "pdp")
#' eff$plot()
#' 
#' # Only a subset of features
#' eff = FeatureEffects$new(mod, features = c("nox", "crim"))
#' eff$plot()
#' 
#' 
#' # You can access each FeatureEffect individually
#' 
#' eff.nox = eff$effects[["nox"]]
#' eff.nox$plot()
#' 
#' 
#' # FeatureEffects also works with multiclass classification
#' rf = rpart(Species ~ ., data = iris)
#' mod = Predictor$new(rf, data = iris, type = "prob")
#' 
#' FeatureEffects$new(mod)$plot(ncol = 2)
#' }
#' }
NULL



#' @export
FeatureEffects = R6::R6Class("FeatureEffects", 
  inherit = InterpretationMethod,
  public = list(
    grid.size = NULL, 
    method  = NULL,
    # The named list of FeatureEffect
    effects = NULL,
    features = NULL,
    center.at = NULL,
    initialize = function(predictor, features = NULL, method = "ale", grid.size = 20, 
      center.at = NULL, parallel = FALSE) {
      if(is.null(features)) {
        self$features = predictor$data$feature.names
      } else {
        stopifnot(all(features %in% predictor$data$feature.names))
        self$features = features
      }
      assert_numeric(grid.size, min.len = 1)
      assert_number(center.at, null.ok = TRUE)
      private$parallel = parallel
      assert_choice(method, c("ale", "pdp", "ice", "pdp+ice"))
      self$grid.size = grid.size
      self$method = method
      self$center.at = center.at
      super$initialize(predictor)
      private$run()
    }
  ),
  private = list(
    run = function() {
      predictor = self$predictor
      method = self$method
      center.at = self$center.at
      grid.size = self$grid.size
      feature.names = self$features
      `%mypar%` = private$get.parallel.fct(private$parallel)
      feature_effect = function(x, predictor, method, center.at, grid.size) {
        FeatureEffect$new(feature = x, predictor = predictor, method = method, center.at = center.at, 
          grid.size = grid.size)
      }
      effects = foreach(feature = feature.names,
        .packages = devtools::loaded_packages()$package, .inorder = FALSE) %mypar%
        feature_effect(feature, predictor = predictor, method = method, center.at = center.at, 
          grid.size = grid.size)
      self$effects = effects
      names(self$effects) = self$features
      self$results = lapply(self$effects, function(x) {
        res = x$results
        fname.index = which(colnames(res) %in% names(self$effects))
        res$.feature = colnames(res)[fname.index]
        colnames(res)[fname.index] = ".feature"
        res
      })
      private$finished = TRUE
    },
    printParameters = function() {
      cat("features:", paste(sprintf("%s", 
        self$features), collapse = ", "))
      cat("\ngrid size:", paste(self$grid.size, collapse = "x"))
    },
    # make sure the default arguments match with plot.FeatureEffect
    generatePlot = function(features = NULL, ncols = NULL, nrows = NULL, fixed_y = TRUE, ...) {
      assert_character(features, null.ok = TRUE)
      if(length(features) > 0) {
        assert_true(all(features %in% self$features))
      } else {
        features = self$features
      }
      
      # Compute size of gtable
      layout = get_layout(length(features), nrows, ncols)
      
      # Based on layout, infer which figures will be left and or bottom
      del_ylab_index = setdiff(1:length(features), 1:min(layout$nrows, length(features)))
      
      
      if(fixed_y) { 
        res = unlist(lapply(features, function(fname){
          cname = ifelse(self$method == "ale", ".ale", ".y.hat")
          values = self$effects[[fname]]$results[cname]
          c(min(values), max(values))
        }))
        ylim = c(min(res), max(res))
      } else {
        ylim = c(NA, NA)
      }
      plts = lapply(features, function(fname) {
        gg = self$effects[[fname]]$plot(..., ylim = ylim) + 
          theme(axis.title.y=element_blank()) 
        ggplotGrob(gg)
      })
      
      y_axis_label = self$effects[[1]]$.__enclos_env__$private$y_axis_label
      # Fill gtable with graphics
      ml = marrangeGrob(grobs = plts, nrow = layout$nrows, ncol = layout$ncols, 
        top = NULL, left = y_axis_label)
      # For graphics not on left side, remove y-axis names and x-axis names
      # return grid
      ml
    }
  )
)


#' Plot FeatureEffect
#' 
#' plot.FeatureEffect() plots the results of a FeatureEffect object.
#' 
#' @details 
#' 
#' In contrast to other plot methods in iml, for FeatureEffects the returned plot is not a ggplot2 object, but a grid object, 
#' a collection of multiple ggplot2 plots.
#' 
#' @param x A FeatureEffect R6 object
#' @param features [character()] For which features should the effects be plotted? Default is all features. 
#'                               You can also sort the order of the plots with this argument.
#' @param ncols The number of columns in the table of graphics
#' @param nrows The number of rows in the table of graphics
#' @param fixed_y Should the y-axis range be the same for all effects? Defaults to TRUE.
#' @param ... Further arguments for FeatureEffect$plot()
#' @return grid object
#' @importFrom gridExtra marrangeGrob
#' @seealso 
#' \link{FeatureEffects}
#' \link{plot.FeatureEffect}
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the partial dependence for the first feature
#' eff = FeatureEffects$new(mod)
#' 
#' # Plot the results directly
#' eff$plot()
#' 
#' # For a subset of features
#' eff$plot(features = c("lstat", "crim"))
#' 
#' # With a different layout
#' eff$plot(nrows = 2)
#' }
plot.FeatureEffects = function(x, features = NULL, nrows = NULL, ncols = NULL,  fixed_y = TRUE, ...) {
  x$plot(features = features, nrows = nrows, ncols = ncols, fixed_y = fixed_y, ...)
}
