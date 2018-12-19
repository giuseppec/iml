#' Prediction explanations with game theory
#' 
#' \code{Shapley} computes feature contributions for single predictions with the Shapley value, an approach from cooperative game theory. 
#' The features values of an instance cooperate to achieve the prediction. 
#' The Shapley value fairly distributes the difference of the instance's prediction and the datasets average prediction among the features. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Shapley
#' @section Usage:
#' \preformatted{
#' shapley = Shapley$new(predictor, x.interest = NULL, sample.size = 100, run = TRUE)
#' 
#' plot(shapley)
#' shapley$results
#' print(shapley)
#' shapley$explain(x.interest)
#' }
#' 
#' @section Arguments: 
#' For Shapley$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{x.interest: }{(data.frame)\cr  Single row with the instance to be explained.}
#' \item{sample.size: }{(`numeric(1)`)\cr The number of  Monte Carlo samples for estimating the Shapley value.} 
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
#' }
#' 
#' @section Details:
#' For more details on the algorithm see https://christophm.github.io/interpretable-ml-book/shapley.html
#' 
#' @section Fields:
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{results: }{(data.frame)\cr data.frame with the Shapley values (phi) per feature.}
#' \item{sample.size: }{(`numeric(1)`)\cr The number of times coalitions/marginals are sampled from data X. The higher the more accurate the explanations become.}
#' \item{x.interest: }{(data.frame)\cr Single row with the instance to be explained.}
#' \item{y.hat.interest: }{(numeric)\cr Predicted value for instance of interest}
#' \item{y.hat.average: }{(`numeric(1)`)\cr Average predicted value for data \code{X}} 
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{explain(x.interest)}{method to set a new data point which to explain.}
#' \item{plot()}{method to plot the Shapley value. See \link{plot.Shapley}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' 
#' @references 
#' Strumbelj, E., Kononenko, I. (2014). Explaining prediction models and individual predictions with feature contributions. Knowledge and Information Systems, 41(3), 647-665. https://doi.org/10.1007/s10115-013-0679-x
#' @seealso 
#' \link{Shapley}
#' 
#' @seealso 
#' A different way to explain predictions: \link{LocalModel}
#' 
#' @examples 
#' if (require("rpart")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' rf =  rpart(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#' 
#' # Then we explain the first instance of the dataset with the Shapley method:
#' x.interest = X[1,]
#' shapley = Shapley$new(mod, x.interest = x.interest)
#' shapley
#' 
#' # Look at the results in a table
#' shapley$results
#' # Or as a plot
#' plot(shapley)
#' 
#' # Explain another instance
#' shapley$explain(X[2,])
#' plot(shapley)
#' \dontrun{
#' # Shapley() also works with multiclass classification
#' rf = rpart(Species ~ ., data = iris)
#' X = iris[-which(names(iris) == "Species")]
#' mod = Predictor$new(rf, data = X, type = "prob")
#' 
#' # Then we explain the first instance of the dataset with the Shapley() method:
#' shapley = Shapley$new(mod, x.interest = X[1,])
#' shapley$results
#' plot(shapley) 
#' 
#' # You can also focus on one class
#' mod = Predictor$new(rf, data = X, type = "prob", class = "setosa")
#' shapley = Shapley$new(mod, x.interest = X[1,])
#' shapley$results
#' plot(shapley) 
#' }
#' }
NULL

#'@export

Shapley = R6::R6Class("Shapley", 
  inherit = InterpretationMethod,
  public = list(
    x.interest = NULL,
    y.hat.interest = NULL,
    y.hat.average = NULL,
    sample.size = NULL,
    explain = function(x.interest) {
      private$flush()
      private$set.x.interest(x.interest)
      self$run()
    },
    initialize = function(predictor, x.interest = NULL, sample.size = 100,  run = TRUE) {
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      super$initialize(predictor = predictor)
      self$sample.size = sample.size
      if (!is.null(x.interest)) {
        private$set.x.interest(x.interest)
      }
      private$getData = function(...) private$sampler$sample(n = self$sample.size, ...)
      if (run & !is.null(x.interest)) self$run()
    }
  ), 
  private = list(
    aggregate = function() {
      y.hat.with.k = private$qResults[1:(nrow(private$qResults)/2), , drop = FALSE]
      y.hat.without.k = private$qResults[(nrow(private$qResults)/2 + 1):nrow(private$qResults), , drop = FALSE]
      y.hat.diff = y.hat.with.k - y.hat.without.k
      cnames = colnames(y.hat.diff)
      y.hat.diff = cbind(data.table(feature = rep(colnames(private$dataDesign), times = self$sample.size)), 
        y.hat.diff)
      y.hat.diff = melt(y.hat.diff, variable.name = "class", value.name = "value", measure.vars = cnames)
      y.hat.diff = y.hat.diff[, list("phi" = mean(value), "phi.var" = var(value)), by = c("feature", "class")]
      if (!private$multiClass) y.hat.diff$class = NULL
      x.original = unlist(lapply(self$x.interest[1,], as.character))
      y.hat.diff$feature.value = sprintf('%s=%s', colnames(self$x.interest), x.original)
      y.hat.diff
    },
    intervene = function() {
      # The intervention
      runs = lapply(1:self$sample.size, function(m) {
        # randomly order features
        new.feature.order = sample(1:private$sampler$n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled = private$dataSample[sample(1:nrow(private$dataSample), 1), new.feature.order, with = FALSE]
        x.interest.shuffled = self$x.interest[, new.feature.order]
        
        featurewise = lapply(1:private$sampler$n.features, function(k) {
          k.at.index = which(new.feature.order == k)
          instance.with.k = x.interest.shuffled
          if (k.at.index < ncol(self$x.interest)) {
            instance.with.k[, (k.at.index + 1):ncol(instance.with.k)] =
              sample.instance.shuffled[, (k.at.index + 1):ncol(instance.with.k), with = FALSE]
          }
          instance.without.k = instance.with.k
          instance.without.k[, k.at.index] = sample.instance.shuffled[, k.at.index, with = FALSE]
          cbind(instance.with.k[, private$sampler$feature.names], 
            instance.without.k[, private$sampler$feature.names])
        }) 
        data.table::rbindlist(featurewise)
        
      }) 
      runs = data.table::rbindlist(runs)
      dat.with.k = data.frame(runs[,1:(ncol(runs)/2)])
      dat.without.k = data.frame(runs[,(ncol(runs)/2 + 1):ncol(runs)])
      
      rbind(dat.with.k, dat.without.k)
    }, 
    set.x.interest = function(x.interest) {
      self$x.interest = x.interest
      self$y.hat.interest = self$predictor$predict(x.interest)[1,]
      self$y.hat.average = colMeans(self$predictor$predict(private$sampler$get.x()))
    },
    generatePlot = function(sort = TRUE, ...) {
      res = self$results
      if (sort & !private$multiClass) {
        res$feature.value = factor(res$feature.value, levels = res$feature.value[order(res$phi)])
      }
      p = ggplot(res) + 
        geom_col(aes(y = phi, x=feature.value)) + coord_flip()
      if (private$multiClass) {
        p = p + facet_wrap("class")
      } else {
        p = p + ggtitle(sprintf("Actual prediction: %.2f\nAverage prediction: %.2f", 
          self$y.hat.interest, self$y.hat.average))
        
      }
      p
    },
    printParameters = function() {
      cat(sprintf("Predicted value: %f, Average prediction: %f (diff = %f)", 
        self$y.hat.interest, self$y.hat.average, self$y.hat.interest - self$y.hat.average))
    }
  )
)

#' Plot Shapley
#' 
#' plot.Shapley() plots the Shapley values - the contributions of feature values to the prediction. 
#' 
#' @param object  A Shapley R6 object
#' @param sort logical. Should the feature values be sorted by Shapley value? Ignored for multi.class output.
#' @return ggplot2 plot object
#' @seealso 
#' \link{Shapley}
#' @examples 
#' \dontrun{
#' if (require("rpart")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' rf =  rpart(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#' 
#' # Then we explain the first instance of the dataset with the Shapley method:
#' x.interest = X[1,]
#' shapley = Shapley$new(mod, x.interest = x.interest)
#' plot(shapley)
#' }
#' }
plot.Shapley = function(object, sort = TRUE) {
  object$plot(sort = sort)
}




