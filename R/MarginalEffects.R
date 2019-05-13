#' Marginal effects of features on the prediction
#' 
#' \code{MarginalEffects} computes and plots marginal feature effects of prediction models. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name MarginalEffects
#' 
#' @section Usage:
#' \preformatted{
#' effect = FeatureEffect$new(predictor, feature, step.size,  method = "forward", 
#'     grid.size = 4)
#' 
#' plot(effect)
#' effect$results
#' effects$ame
#' print(effect)
#' }
#' 
#' @section Arguments:
#' 
#' For MarginalEffect$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{feature: }{(`character(n)` | ``numeric(n)) \cr The feature names or indices for which to compute the effects.}
#' \item{method: }{(`character(1)`)\cr 
#' 'forward' for absolute forward step marginal effect, 
#' 'derivative' for the right sided finite difference, 
#' 'derivative2' for the symetric finite difference
#' }
#' \item{step.size: }{(`numeric(n)`)\cr The size of the forward steps. Only applicable if method='forward'}
#' \item{h: }{`numeric(n)`)\cr The finite differences step size. Only applicable if method='derivative' or method='derivative2'}
#' }
#' 
#' @section Details:
#' 
#' TODO
#' 
#' @section Fields:
#' \describe{
#' \item{method: }{(`character(1)`)\cr
#' 'forward' for absolute forward step marginal effect, 
#' 'derivative' for the right sided finite difference, 
#' 'derivative2' for the symetric finite difference}
#' \item{feature.name: }{(`character(n)`)\cr The names of the features for which the marginal effects were computed.}
#' \item{feature.type: }{(`character(n)`)\cr The detected types of the features, either "categorical" or "numerical".}
#' \item{step.size: }{(`numeric(n)`)\cr The size of the forward steps. Only applicable if method='forward'}
#' \item{h: }{`numeric(n)`)\cr The finite differences step size. Only applicable if method='derivative' or method='derivative2'}
#' \item{predictor: }{(Predictor)\cr The prediction model that was analysed.}
#' \item{results: }{(data.frame)\cr data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' \item{ame: }{`numeric(1)`\cr The average marginal effects.}}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{plot()}{method to plot the marginal effects. See \link{plot.MarginalEffects}}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' @seealso 
#' \link{plot.MarginalEffects} 
#' 
#' @references 
#' TODO: reference paper here
#' @export
#' @examples
#' # TODO: Update example here
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the accumulated local effects for the first feature
#' eff = FeatureEffect$new(mod, feature = "rm",grid.size = 30)
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
#' # You can also use the object to "predict" the marginal values.
#' eff$predict(Boston[1:3,])
#' # Instead of the entire data.frame, you can also use feature values:
#' eff$predict(c(5,6,7))
#' 
#' # You can reuse the pdp object for other features: 
#' eff$set.feature("lstat")
#' plot(eff)
#'
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
#' # FeatureEffect plots also works with multiclass classification
#' rf = randomForest(Species ~ ., data = iris, ntree=50)
#' mod = Predictor$new(rf, data = iris, type = "prob")
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(FeatureEffect$new(mod, feature = "Petal.Width"))
#'
#' # FeatureEffect plots support up to two features: 
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

MarginalEffects = R6::R6Class("MarginalEffects", 
  inherit = InterpretationMethod,
  public = list(
    grid.size = NULL,
    step.size = NULL,
    feature.name = NULL,
    feature.type = NULL,
    n.features = NULL,
    method  = NULL,
    initialize = function(predictor, feature, step.size, method = "forward", grid.size = 4, h = 0.001) {
      feature_index = private$sanitize.feature(feature, predictor$data$feature.names)
      assert_numeric(feature_index, lower = 1, upper = predictor$data$n.features, min.len = 1, max.len = 2)
      assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      assert_numeric(step.size, len = length(feature))
      assert_numeric(h, len = 1)
      assert_choice(method, c("forward", "derivative", "derivative2"))
      self$method = method
      super$initialize(predictor)
      private$set_feature_from_index(feature_index)
      self$grid.size = grid.size
      private$run(self$predictor$batch.size)
    } 
  ),
  private = list(
    intervene = function(){
      private$dataSample = private$getData()
      if(self$method == "forward") {
        sample2 = private$dataSample
        sample2[, self$feature.name] = 
		sample2[, self$feature.name, with = FALSE] + self$step.size
        private$dataSample = rbind(private$dataSample, sample2)
      } else if (self$method == "derivative"){
        # TODO
	stop("not yet implemented")
      } else if (self$method == "derivative2"){
	# TODO
	stop("not yet implemented")
      }
    },
    aggregate = function(){
      # Difference between prediction and forward prediction
      original_index = 1:nrow(private$dataSample)
      findex = setdiff(1:nrow(private$dataDesign), original_index)
      predictions = self$qResults[original_index,]
      fpredictions = self$qResults[findex,]
      self$results = fpredictions - predictions
      self$results = cbind(self$results, 
			   private$dataDesign[original_index, self$feature.name, with = FALSE],
			   private$dataDesign[findex, self$feature.name, with = FALSE])
    },
    printParameters = function() {
      cat("features:", paste(sprintf("%s[%s]", 
        self$feature.name, self$feature.type), collapse = ", "))
      cat("\nstep sizes:", paste(self$step.size, collapse = "x"))
    },
    sanitize.feature = function(feature, feature.names) {
      if (is.character(feature)) {
        feature.char = feature
        stopifnot(all(feature %in% feature.names))
        feature = which(feature.char[1] == feature.names)
        if (length(feature.char) == 2) {
          feature = c(feature, which(feature.char[2] == feature.names))
        }
      }
      feature
    },
    set_feature_from_index = function(feature.index) {
      self$n.features = length(feature.index)
      self$feature.type = private$sampler$feature.types[feature.index]
      self$feature.name = private$sampler$feature.names[feature.index]
    },

    generatePlot = function(mse = FALSE) {
      # TODO: Plot distribution of MarginalEffects
    } 
  )
)


#' Plot MarginalEffects
#' 
#' plot.MarginalEffects() plots the results of a MarginalEffects object.
#' 
#' @param x A MarginalEffects R6 object
#' @param mse Plot the MSE distribution as well? 
#' @return ggplot2 plot object
#' @seealso 
#' \link{MarginalEffect}
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the ALE for the first feature
#' eff = MarginalEffect$new(mod, feature = "crim", step.size = 1)
#' 
#' # Plot the results directly
#' plot(eff)
#' }
plot.MarginalEffects = function(x, mse = FALSE) {
  assert_logical(mse)
  x$plot(mse = mse)
}
