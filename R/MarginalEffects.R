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
#' }
#' \item{step.size: }{(`numeric(n)`)\cr The size of the forward steps. Only applicable if method='forward'}
#' \item{eps: }{`numeric(n)`)\cr The finite differences step size. Only applicable if method='derivative'}
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
#' \item{feature.name: }{(`character(n)`)\cr The names of the features for which the marginal effects were computed.}
#' \item{feature.type: }{(`character(n)`)\cr The detected types of the features, either "categorical" or "numerical".}
#' \item{step.size: }{(`numeric(n)`)\cr The size of the forward steps. Only applicable if method='forward'}
#' \item{h: }{`numeric(n)`)\cr The finite differences step size. Only applicable if method='derivative'}
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
#' eff = MarginalEffects$new(mod, feature = "rm", step.size = 1)
#' eff$plot()
#' eff$ame
#'
#' # Again, but this time with derivative method 
#' eff = MarginalEffects$new(mod, feature = "rm", method = "derivative")
#' plot(eff)
#' eff$ame
#'
#' # MarginalEffects for multiple  features: 
#' eff = MarginalEffects$new(mod, feature = c("lstat", "rm"), step.size = c(1, 0.5))
#' eff$plot()   
#' 
#' # Multiclass classification
#' rf = randomForest(Species ~ ., data = iris, ntree=50)
#' mod = Predictor$new(rf, data = iris, type = "prob")
#' eff = MarginalEffects$new(mod, feature = "Petal.Width", step.size = 1)
#' eff$plot()
#' eff$ame
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
    ame = NULL,
    eps = NULL,
    mse = NULL,
    initialize = function(predictor,
			  feature,
			  step.size = NULL,
			  method = "forward",
			  grid.size = 10,
			  eps = 1e-07) {
      feature_index = private$sanitize.feature(feature,
					       predictor$data$feature.names)
      assert_numeric(feature_index, lower = 1,
		     upper = predictor$data$n.features, min.len = 1, max.len = 2)
      assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      assert_numeric(step.size, len = length(feature), null.ok = TRUE)
      assert_numeric(eps, len = 1)
      assert_choice(method, c("forward", "derivative"))
      if (method == "forward" & is.null(step.size)) {
         stop("Please set step.size")
      }
      if (method == "derivative" & length(feature) > 1) {
        stop("Only single features are allowed for derivative")
      }
      self$method = method
      self$step.size = step.size
      self$eps = eps
      super$initialize(predictor)
      private$set_feature_from_index(feature_index)
      self$grid.size = grid.size
      private$run(self$predictor$batch.size)
    }
  ),
  private = list(
    intervene = function(){
      private$dataSample = private$getData()
      if (self$method == "forward") {
        rows = rep(1:nrow(private$dataSample), times = self$grid.size + 2)
        sample2 = private$dataSample[rows, ]
	# first and last grid steps (0 and step size) are used for fME
	# steps inbetween for MSE computation
        for (j in seq_along(self$feature.name)) {
	  steps = seq(from = 0, to = self$step.size[j],
		       length.out = self$grid.size + 2)
	  steps = rep(steps, each = nrow(private$dataSample))
	  sample2[, self$feature.name[j]] =  sample2[, self$feature.name[j], with = FALSE] + steps  
	}
	private$dataDesign = sample2
      } else if (self$method == "derivative"){
	# Code from marginals package:
        setstep <- function(x) {
           x + (max(abs(x), 1, na.rm = TRUE) * sqrt(self$eps)) - x
        }
        sample1 = sample2 = private$dataSample
	feat = self$feature.name
        sample1[[feat]] = sample1[[feat]] - setstep(sample1[[feat]])
	sample2[[feat]] = sample2[[feat]] + setstep(sample2[[feat]])
	private$dataDesign = rbind(sample1, sample2)
      }
    },
    aggregate = function(){
      # Difference between prediction and forward prediction
      oindex = 1:nrow(private$dataSample)
      # last n rows
      findex = (nrow(private$dataDesign) - nrow(private$dataSample) + 1):nrow(private$dataDesign)
      predictions = private$qResults[oindex, , drop = FALSE]
      fpredictions = private$qResults[findex, , drop = FALSE]
      results = fpredictions - predictions
      # Analogue to margins package
      if (self$method == "derivative") {
        feat = self$feature.name
        results = (fpredictions - predictions) /
	        (private$dataDesign[findex, ][[feat]] -
	         private$dataDesign[oindex, ][[feat]])
      } else if (self$grid.size > 0) {
	# predictions to be used for MSE computation
	mindex = setdiff(1:nrow(private$dataDesign), c(oindex, findex))
        grid_predictions = private$qResults[mindex, ]

	# Interpolation between actual prediction and forward predictions
        interpolation_steps = ( fpredictions - predictions) /
		( self$grid.size + 1)
	interpolation_steps = data.frame(interpolation_steps)
        repe = rep(1:nrow(private$dataSample), times = self$grid.size)
	grid_linear_steps = interpolation_steps[repe, , drop = FALSE]
	grid_linear_steps = grid_linear_steps *
		rep(1:self$grid.size, each = nrow(private$dataSample))
	grid_interpolations = predictions[repe, ] + grid_linear_steps 
        self$mse = colSums((grid_predictions - grid_interpolations)^2)
      }
      fs = private$dataSample[, self$feature.name, with = FALSE]
      results = cbind(fs, results)
      if (private$multiClass) {
        results =  melt(results, variable.name = ".class",
                        value.name = ".meffect",
                        measure.vars = colnames(predictions))
        ame = lapply(unique(results$.class),
                     function(x){
                       mean(results[results$.class == x, ".meffect"][[1]])
                     })
        self$ame = unlist(ame)
      } else {
        colnames(results) = c(self$feature.name, ".meffect")
        self$ame = mean(results$.meffect)
      }
      names(self$ame) = colnames(private$qResults)
      results
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

    generatePlot = function() {
      ggplot(self$results) + geom_density(aes(x = .meffect))
    }
  )
)


#' Plot MarginalEffects
#' 
#' plot.MarginalEffects() plots the results of a MarginalEffects object.
#' 
#' @param x A MarginalEffects R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{MarginalEffects}
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the marginal effects for the first feature
#' eff = MarginalEffects$new(mod, feature = "crim", step.size = 1)
#' 
#' # Plot the results directly
#' plot(eff)
#' }
plot.MarginalEffects = function(x) {
  x$plot()
}
