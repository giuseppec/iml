#' Game theoretic prediction explanations
#' 
#' \code{Shapley} computes feature contributions for single predictions with the Shapley value, an approach from cooperative game theory approach. 
#' The features values of an instance cooperate to achieve the prediction. 
#' The Shapley value fairly distributes the difference of the instance's prediction and the datasets average prediction among the features. 
#' A features contribution can be negative. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Shapley
#' @section Usage:
#' \preformatted{
#' shapley = Shapley$new(model, data, x.interest = NULL, 
#'   sample.size = 100, run = TRUE)
#' 
#' plot(shapley)
#' shapley$results
#' print(shapley)
#' }
#' 
#' @section Arguments: 
#' For Shapley$new():
#' \describe{
#' \item{model}{object of type \code{Model}. See \link{Model}.}
#' \item{data}{data.frame with the data to which compare the x.interest.}
#' \item{x.interest}{data.frame with a single row for the instance to be explained.}
#' \item{sample.size}{The number of instances to be sampled from the data.} 
#' \item{maxdepth}{The maximum depth of the tree. Default is 2.}
#' \item{run}{logical. Should the Interpretation method be run?}
#' }
#' 
#' @section Details:
#' For more details on the algorithm see https://christophm.github.io/interpretable-ml-book/shapley.html
#' 
#' @section Fields:
#' \describe{
#' \item{sample.size}{The number of times coalitions/marginals are sampled from data X. The higher the more accurate the explanations become.}
#' \item{x.interest}{data.frame with the instance of interest}
#' \item{y.hat.interest}{predicted value for instance of interest}
#' \item{y.hat.averate}{average predicted value for data \code{X}} 
#' \item{results}{data.frame with sampled feature X together with the leaf node information (columns ..node and ..path) 
#' and the predicted \eqn{\hat{y}} for tree and machine learning model (columns starting with ..y.hat).}
#' \item{x.interest}{data.frame with a single row for the instance to be explained.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{explain(x.interest)}{method to set a new data point which to explain.}
#' \item{plot()}{method to plot the Shapley value. See \link{plot.Shapley}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' General R6 methods
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' 
#' @references 
#' Strumbelj, E., Kononenko, I. (2014). Explaining prediction models and individual predictions with feature contributions. Knowledge and Information Systems, 41(3), 647-665. https://doi.org/10.1007/s10115-013-0679-x
#' @seealso 
#' \link{Lime}
#' 
#' @seealso 
#' A different way to explain predictions: \link{Lime}
#' 
#' @examples 
#' # First we fit a machine learning model on the Boston housing data
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf =  randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Model$new(rf)
#' X = Boston[-which(names(Boston) == "medv")]
#' 
#' # Then we explain the first instance of the dataset with the Shapley method:
#' x.interest = X[1,]
#' shapley = Shapley$new(mod, X, x.interest = x.interest)
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
#' 
#' # Shapley() also works with multiclass classification
#' rf = randomForest(Species ~ ., data= iris, ntree=50)
#' mod = Model$new(rf, predict.args = list(type='prob'))
#' X = iris[-which(names(iris) == "Species")]
#' 
#' # Then we explain the first instance of the dataset with the Shapley() method:
#' shapley = Shapley$new(mod, X, x.interest = X[1,])
#' shapley$results
#' plot(shapley) 
#' 
#' # You can also focus on one class
#' mod = Model$new(rf, predict.args = list(type = "prob"), class = "setosa")
#' shapley = Shapley$new(mod, X, x.interest = X[1,])
#' shapley$results
#' plot(shapley) 
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
    initialize = function(model, data, x.interest = NULL, sample.size = 100,  run = TRUE) {
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      super$initialize(model = model, data = data)
      self$sample.size = sample.size
      if (!is.null(x.interest)) {
        private$set.x.interest(x.interest)
      }
      private$get.data = function(...) private$sampler$sample(n = self$sample.size, ...)
      if (run & !is.null(x.interest)) self$run()
    }
  ), 
  private = list(
    aggregate = function() {
      y.hat.with.k = private$Q.results[1:(nrow(private$Q.results)/2), , drop = FALSE]
      y.hat.without.k = private$Q.results[(nrow(private$Q.results)/2 + 1):nrow(private$Q.results), , drop = FALSE]
      y.hat.diff = y.hat.with.k - y.hat.without.k
      cnames = colnames(y.hat.diff)
      y.hat.diff = cbind(data.frame(feature = rep(colnames(private$X.design), times = self$sample.size)), 
        y.hat.diff)
      y.hat.diff = gather(y.hat.diff, key = "class", "value", one_of(cnames))
      y.hat.diff.grouped  =   group_by(y.hat.diff, feature, class)
      y.hat.diff = summarise(y.hat.diff.grouped, phi = mean(value), phi.var = var(value))
      if (!private$multi.class) y.hat.diff$class = NULL
      y.hat.diff$featureValue = sprintf('%s=%s', colnames(self$x.interest), self$x.interest)
      y.hat.diff
    },
    intervene = function() {
      # The intervention
      runs = lapply(1:self$sample.size, function(m) {
        # randomly order features
        new.feature.order = sample(1:private$sampler$n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled = private$X.sample[sample(1:nrow(private$X.sample), 1), new.feature.order]
        x.interest.shuffled = self$x.interest[new.feature.order]
        
        featurewise = lapply(1:private$sampler$n.features, function(k) {
          k.at.index = which(new.feature.order == k)
          instance.with.k = x.interest.shuffled
          if (k.at.index < ncol(self$x.interest)) {
            instance.with.k[(k.at.index + 1):ncol(instance.with.k)] =
              sample.instance.shuffled[(k.at.index + 1):ncol(instance.with.k)]
          }
          instance.without.k = instance.with.k
          instance.without.k[k.at.index] = sample.instance.shuffled[k.at.index]
          cbind(instance.with.k[private$sampler$feature.names], instance.without.k[private$sampler$feature.names])
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
      self$y.hat.interest = private$model$predict(x.interest)[1,]
      self$y.hat.average = colMeans(private$model$predict(private$sampler$get.x()))
    },
    generate.plot = function(sort = TRUE, ...) {
      res = self$results
      if (sort & !private$multi.class) {
        res$featureValue = factor(res$featureValue, levels = res$featureValue[order(res$phi)])
      }
      p = ggplot(res) + 
        geom_col(aes(y = phi, x=featureValue)) + coord_flip()
      if (private$multi.class) p = p + facet_wrap("class")
      p
    },
    print.parameters = function() {
      cat(sprintf("Predicted value: %f, Average prediction: %f (diff = %f)", 
        self$y.hat.interest, self$y.hat.average, self$y.hat.interest - self$y.hat.average))
    }
  )
)

#' Shapley plot
#' 
#' plot.Shapley() plots the Shapley values - the contributions of feature values to the prediction. 
#' 
#' For examples see \link{Shapley}
#' @param object  A Shapley R6 object
#' @param sort logical. Should the feature values be sorted by Shapley value? Only works for single model output.
#' @return ggplot2 plot object
#' @seealso 
#' \link{Shapley}
plot.Shapley = function(object, sort = TRUE) {
  object$plot(sort = sort)
}




