#' Partial Dependence Plot
#' 
#' \code{PartialDependence} computes and plots partial dependence functions of prediction models. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name PartialDependence
#' 
#' @section Usage:
#' \preformatted{
#' pdp = PartialDependence$new(predictor, feature, grid.size = 20, run = TRUE)
#' 
#' plot(pdp)
#' pdp$results
#' print(pdp)
#' pdp$set.feature(2)
#' }
#' 
#' @section Arguments:
#' 
#' For PartialDependence$new():
#' \describe{
#' \item{predictor}{Object of type \code{Predictor}. See \link{Predictor}.}
#' \item{feature}{The feature name or index for which to compute the partial dependencies. 
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
#' \item{feature.name}{The names of the features for which the partial dependence was computed.}
#' \item{feature.type}{The detected types of the features, either "categorical" or "numerical".}
#' \item{grid.size}{The size of the grid.}
#' \item{n.features}{The number of features (either 1 or 2)}
#' \item{predictor}{The prediction model that was analysed.}
#' \item{results}{data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom partial dependence plots.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{set.feature}{method to get/set feature(s) (by index) fpr  which to compute pdp. See examples for usage.}
#' \item{plot()}{method to plot the partial dependence function. See \link{plot.PartialDependence}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
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
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the partial dependence for the first feature
#' pdp.obj = PartialDependence$new(mod, feature = "crim")
#' 
#' # Plot the results directly
#' plot(pdp.obj)
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' if (require("ggplot2")) {
#'  plot(pdp.obj) + theme_bw()
#' }
#' 
#' # If you want to do your own thing, just extract the data: 
#' pdp.dat = pdp.obj$results
#' head(pdp.dat)
#' 
#' # You can reuse the pdp object for other features: 
#' pdp.obj$set.feature("lstat")
#' plot(pdp.obj)
#' 
#' # Partial dependence plots support up to two features: 
#' pdp.obj = PartialDependence$new(mod, feature = c("crim", "lstat"))  
#' plot(pdp.obj)
#' 
#' # Partial dependence plots also works with multiclass classification
#' rf = randomForest(Species ~ ., data = iris, ntree=50)
#' predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun)
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(PartialDependence$new(mod, feature = "Sepal.Length"))
#'
#' # Partial dependence plots support up to two features: 
#' pdp.obj = PartialDependence$new(mod, feature = c("Sepal.Length", "Petal.Length"))
#' pdp.obj$plot()   
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun, class = 1)
#' plot(PartialDependence$new(mod, feature = "Sepal.Length"))
#' }
NULL



#' @export

PartialDependence = R6::R6Class("PartialDependence", 
  inherit = InterpretationMethod,
  public = list(
    grid.size = NULL, 
    feature.index = NULL, 
    feature.name = NULL,
    n.features = NULL, 
    feature.type= NULL,
    initialize = function(predictor, feature, grid.size = 20, run = TRUE) {
      feature = private$sanitizeFeature(feature, predictor$data$feature.names)
      checkmate::assert_numeric(feature, lower = 1, upper = predictor$data$n.features, min.len = 1, max.len = 2)
      checkmate::assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      if (length(feature) == 2) checkmate::assert_false(feature[1] == feature[2])
      super$initialize(predictor)
      private$setFeatureFromIndex(feature)
      private$set.grid.size(grid.size)
      private$grid.size.original = grid.size
      if(run) self$run()
    }, 
    set.feature = function(feature) {
      feature = private$sanitizeFeature(feature, self$predictor$data$feature.names)
      private$flush()
      private$setFeatureFromIndex(feature)
      private$set.grid.size(private$grid.size.original)
      self$run()
    }
  ), 
  private = list(
    aggregate = function() {
      results = private$dataDesign[self$feature.index]
      if (private$multiClass) {
        y.hat.names = colnames(private$qResults)
        results = cbind(results, private$qResults)
        results = gather(results, key = "..class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        results["y.hat"]= private$qResults
        results["..class.name"] = self$predictor$class
      }
      results.grouped = group_by_at(results, self$feature.name)
      if ("..class.name" %in% colnames(results)) {
        results.grouped = group_by(results.grouped, ..class.name, add = TRUE)
      }
      results = data.frame(summarise(results.grouped, y.hat = mean(y.hat)))
      results 
    },
    intervene = function() {
      grid = get.1D.grid(private$dataSample[[self$feature.index[1]]], self$feature.type[1], self$grid.size[1])
      
      private$dataDesign.ids = rep(1:nrow(private$dataSample), times = length(grid))
      dataDesign = private$dataSample[private$dataDesign.ids,]
      dataDesign[self$feature.index[1]] = rep(grid, each = nrow(private$dataSample))
      
      if (self$n.features == 2) {
        grid2 = get.1D.grid(private$dataSample[[self$feature.index[2]]], self$feature.type[2], self$grid.size[2])
        private$dataDesign.ids = rep(private$dataDesign.ids, times = length(grid2))
        dataDesign2 = dataDesign[rep(1:nrow(dataDesign), times = length(grid2)), ]
        dataDesign2[self$feature.index[2]] = rep(grid2, each = nrow(dataDesign))
        return(dataDesign2)
      }
      dataDesign
    }, 
    dataDesign.ids = NULL, 
    grid.size.original = NULL,
    setFeatureFromIndex = function(feature.index) {
      self$feature.index = feature.index
      self$n.features = length(feature.index)
      self$feature.type = private$sampler$feature.types[feature.index]
      self$feature.name = private$sampler$feature.names[feature.index]
    },
    printParameters = function() {
      cat("features:", paste(sprintf("%s[%s]", self$feature.name, self$feature.type), collapse = ", "))
      cat("\ngrid size:", paste(self$grid.size, collapse = "x"))
    },
    generatePlot = function() {
      if (self$n.features == 1) {
        p = ggplot(self$results, mapping = aes_string(x = self$feature.name,"y.hat")) + 
          scale_y_continuous(expression(hat(y)))
        if (self$feature.type == "numerical") {
          p = p + geom_path() 
        } else if (self$feature.type == "categorical") {
          p = p + geom_point()
        }
        
      } else if (self$n.features == 2) {
        if (all(self$feature.type %in% "numerical") | all(self$feature.type %in% "categorical")) {
          p = ggplot(self$results) + 
            geom_tile(aes_string(x = self$feature.name[1], 
              y = self$feature.name[2], 
              fill = "y.hat")) + 
            scale_fill_continuous(expression(hat(y)))
        } else {
          categorical.feature = self$feature.name[self$feature.type=="categorical"]
          numerical.feature = setdiff(self$feature.name, categorical.feature)
          p = ggplot(self$results) + 
            geom_line(aes_string(x = numerical.feature, y = "y.hat", 
              group = categorical.feature, color = categorical.feature)) + 
            scale_y_continuous(expression(hat(y)))
        }
      }
      if (private$multiClass) {
        p = p + facet_wrap("..class.name")
      } 
      p
    }, 
    set.grid.size = function(size) {
      self$grid.size = numeric(length=self$n.features)
      names(self$grid.size) = private$sampler$feature.name[self$feature.index]
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
    }, 
    sanitizeFeature = function(feature, feature.names) {
      if (is.character(feature)) {
        feature.char = feature
        stopifnot(all(feature %in% feature.names))
        feature = which(feature.char[1] == feature.names)
        if (length(feature.char) == 2) {
          feature = c(feature, which(feature.char[2] == feature.names))
        }
      }
      feature
    }
  )
)


#' Plot Partial Dependence
#' 
#' plot.PartialDependence() plots the results of a PartialDependence object.
#' 
#' @param x A PartialDependence R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{PartialDependence}
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the partial dependence for the first feature
#' pdp.obj = PartialDependence$new(mod, feature = "crim")
#' 
#' # Plot the results directly
#' plot(pdp.obj)
#' }
plot.PartialDependence = function(x) {
  x$plot()
}








