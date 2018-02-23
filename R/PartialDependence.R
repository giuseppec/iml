#' Partial Dependence Plot
#' 
#' \code{PartialDependence} computes and plots partial dependence functions of prediction models. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name PartialDependence
#' 
#' @section Usage:
#' \preformatted{
#' pdp = PartialDependence$new(model, data, feature, grid.size = 10, run = TRUE)
#' 
#' plot(pdp)
#' pdp$results
#' print(pdp)
#' pdp$feature = 2
#' }
#' 
#' @section Arguments:
#' 
#' For PartialDependence$new():
#' \describe{
#' \item{model}{Object of type \code{Model}. See \link{Model}}
#' \item{data}{data.frame with the data for the prediction model.}
#' \item{feature}{The feature index for which to compute the partial dependencies. 
#' Either a single number or vector of two numbers.}
#' \item{grid.size}{The size of the grid for evaluating the predictions}
#' \item{run}{logical. Should the Interpretation method be run?}
#' }
#' 
#' @section Details:
#' The partial dependence plot calculates and plots the dependence of f(X) on a single or two features.
#' To learn more about partial dependence plot, read the Interpretable Machine Learning book: 
#' https://christophm.github.io/interpretable-ml-book/pdp.html
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{feature.index}{The index of the features for which the partial dependence was computed.}
#' \item{feature.names}{The names of the features for which the partial dependence was computed.}
#' \item{feature.type}{The detected types of the features, either "categorical" or "numerical".}
#' \item{grid.size}{The size of the grid.}
#' \item{n.features}{The number of features (either 1 or 2)}
#' \item{data()}{data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom partial dependence plots.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{feature}{method to get/set feature(s) (by index) fpr  which to compute pdp. See examples for usage.}
#' \item{plot()}{method to plot the partial dependence function. See \link{plot.PartialDependence}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' General R6 methods
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' @seealso 
#' \link{plot.PartialDependence}
#' 
#' \code{\link{Ice}} for individual conditional expectation plots. 
#' 
#' 
#' 
#' @references 
#' Friedman, J.H. 2001. "Greedy Function Approximation: A Gradient Boosting Machine." Annals of Statistics 29: 1189-1232.
#' 
#' @importFrom tidyr gather
#' @importFrom dplyr one_of group_by group_by_at summarise
#' @import ggplot2
#' @export
#' @examples
#' # We train a random forest on the Boston dataset:
#' library("randomForest")
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Model$new(rf)
#' 
#' # Compute the partial dependence for the first feature
#' pdp.obj = PartialDependence$new(mod, Boston, feature = 1)
#' 
#' # Plot the results directly
#' plot(pdp.obj)
#' 
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' library("ggplot2")
#' plot(pdp.obj) + theme_bw()
#' 
#' # If you want to do your own thing, just extract the data: 
#' pdp.dat = pdp.obj$results
#' head(pdp.dat)
#' 
#' # You can reuse the pdp object for other features: 
#' pdp.obj$feature = 2
#' plot(pdp.obj)
#' 
#' # Partial dependence plots support up to two features: 
#' pdp.obj = PartialDependence$new(mod, Boston, feature = c(1,2))  
#' 
#' # Partial dependence plots also works with multiclass classification
#' library("randomForest")
#' rf = randomForest(Species ~ ., data= iris, ntree=50)
#' mod = Model$new(rf, predict.args = list(type = 'prob'))
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(PartialDependence$new(mod, iris, feature = 1))
#'
#' # Partial dependence plots support up to two features: 
#' pdp.obj = PartialDependence$new(mod, iris, feature = c(1,3))
#' pdp.obj$plot()   
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Model$new(rf, predict.args = list(type = 'prob'), class = 1)
#' plot(PartialDependence$new(mod, iris, feature = 1))
#' 
NULL



#' @export

PartialDependence = R6::R6Class("PartialDependence", 
  inherit = InterpretationMethod,
  public = list(
    grid.size = NULL, 
    feature.index = NULL, 
    feature.names = NULL,
    n.features = NULL, 
    feature.type= NULL,
    initialize = function(model, data, feature, grid.size = 10, run = TRUE) {
      checkmate::assert_numeric(feature, lower = 1, upper = ncol(data), min.len = 1, max.len = 2)
      checkmate::assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      if (length(feature) == 2) checkmate::assert_false(feature[1] == feature[2])
      super$initialize(model, data)
      private$set.feature(feature)
      private$set.grid.size(grid.size)
      private$grid.size.original = grid.size
      if(run) self$run()
    }
  ), 
  private = list(
    aggregate = function() {
      results = private$X.design[self$feature.index]
      
      if (private$multi.class) {
        y.hat.names = colnames(private$Q.results)
        results = cbind(results, private$Q.results)
        results = gather(results, key = "..class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        results["y.hat"]= private$Q.results
        results["..class.name"] = private$model$class
      }
      results.grouped = group_by_at(results, self$feature.names)
      if ("..class.name" %in% colnames(results)) {
        results.grouped = group_by(results.grouped, ..class.name, add = TRUE)
      }
      results = data.frame(summarise(results.grouped, y.hat = mean(y.hat)))
      results 
    },
    intervene = function() {
      grid = get.1D.grid(private$X.sample[[self$feature.index[1]]], self$feature.type[1], self$grid.size[1])
      
      private$X.design.ids = rep(1:nrow(private$X.sample), times = length(grid))
      X.design = private$X.sample[private$X.design.ids,]
      X.design[self$feature.index[1]] = rep(grid, each = nrow(private$X.sample))
      
      if (self$n.features == 2) {
        grid2 = get.1D.grid(private$X.sample[[self$feature.index[2]]], self$feature.type[2], self$grid.size[2])
        private$X.design.ids = rep(private$X.design.ids, times = length(grid2))
        X.design2 = X.design[rep(1:nrow(X.design), times = length(grid2)), ]
        X.design2[self$feature.index[2]] = rep(grid2, each = nrow(X.design))
        return(X.design2)
      }
      X.design
    }, 
    X.design.ids = NULL, 
    grid.size.original = NULL,
    set.feature = function(feature.index) {
      self$feature.index = feature.index
      self$n.features = length(feature.index)
      self$feature.type = private$sampler$feature.types[feature.index]
      self$feature.names = private$sampler$feature.names[feature.index]
    },
    print.parameters = function() {
      cat("features:", paste(sprintf("%s[%s]", self$feature.names, self$feature.type), collapse = ", "))
      cat("\ngrid size:", paste(self$grid.size, collapse = "x"))
    },
    generate.plot = function() {
      if (self$n.features == 1) {
        p = ggplot(self$results, mapping = aes_string(x = self$feature.names,"y.hat"))
        if (self$feature.type == "numerical") {
          p = p + geom_path() 
        } else if (self$feature.type == "categorical") {
          p = p + geom_point()
        }
      } else if (self$n.features == 2) {
        if (all(self$feature.type %in% "numerical") | all(self$feature.type %in% "categorical")) {
          p = ggplot(self$results) + 
            geom_tile(aes_string(x = self$feature.names[1], 
              y = self$feature.names[2], 
              fill = "y.hat"))
        } else {
          categorical.feature = self$feature.names[self$feature.type=="categorical"]
          numerical.feature = setdiff(self$feature.names, categorical.feature)
          p = ggplot(self$results) + 
            geom_line(aes_string(x = numerical.feature, y = "y.hat", 
              group = categorical.feature, color = categorical.feature))
        }
      }
      if (private$multi.class) {
        p = p + facet_wrap("..class.name")
      } 
      p
    }, 
    set.grid.size = function(size) {
      self$grid.size = numeric(length=self$n.features)
      names(self$grid.size) = private$sampler$feature.names[self$feature.index]
      private$set.grid.size.single(size[1], 1)
      if (self$n.features > 1) {
        if (length(size) == 1) {
          # If user only provided 1D grid size
          private$set.grid.size.single(size[1], 2)
        } else {
          # If user provided 2D grid.size
          private$set.grid.size.single(size[2], 2)
        }
      }
    }, 
    set.grid.size.single = function(size, feature.number) {
      self$grid.size[feature.number] = ifelse(self$feature.type[feature.number] == "numerical", 
        size, length(unique(private$sampler$get.x()[[self$feature.index[feature.number]]])))
    }
  ), 
  active = list(
    feature = function(feature.index) {
      private$flush()
      private$set.feature(feature.index)
      private$set.grid.size(private$grid.size.original)
      self$run()
    }
  )
)


#' Partial dependence plot
#' 
#' plot.PartialDependence() plots a line for a single feature and a tile plot for 2 features.
#' 
#' For examples see \link{PartialDependence}
#' @param x The partial dependence. A PartialDependence R6 object
#' @param ... Further arguments for the objects plot function
#' @return ggplot2 plot object
#' @seealso 
#' \link{PartialDependence}
plot.PartialDependence = function(x, ...) {
  x$plot(...)
}








