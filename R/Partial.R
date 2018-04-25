#' Partial Dependence and Individual Conditional Expectation
#' 
#' \code{Partial} computes and plots (individual) partial dependence functions of prediction models. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Partial
#' 
#' @section Usage:
#' \preformatted{
#' pd = Partial$new(predictor, feature, ice = TRUE, aggregation = "pdp", 
#'     grid.size = 20,  center.at = NULL, run = TRUE)
#' 
#' plot(pd)
#' pd$results
#' print(pd)
#' pd$set.feature(2)
#' pd$center(1)
#' }
#' 
#' @section Arguments:
#' 
#' For Partial$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{feature: }{(`character(1)` | `character(2)` | `numeric(1)` | `numeric(2)`) \cr The feature name or index for which to compute the partial dependencies.}
#' \item{ice: }{(`logical(1)`)\cr Should individual curves be calculated? Ignored in the case of two features.}
#' \item{center.at: }{(`numeric(1)`)\cr Value at which the plot should be centered. Ignored in the case of two features.}
#' \item{grid.size: }{(`numeric(1)` | `numeric(2)`)\cr The size of the grid for evaluating the predictions}
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
#' }
#' 
#' @section Details:
#' The partial dependence plot calculates and plots the dependence of f(X) on a single or two features.
#' It's the aggregate of all individual conditional expectation curves, that describe how, for a single
#' observation, the prediction changes when the feature changes. 
#' 
#' To learn more about partial dependence plot, read the Interpretable Machine Learning book: 
#' https://christophm.github.io/interpretable-ml-book/pdp.html
#' 
#' And for individual conditional expectation: 
#' https://christophm.github.io/interpretable-ml-book/ice.html
#' 
#' 
#' @section Fields:
#' \describe{
#' \item{feature.index: }{(`numeric(1)` | `numeric(2)`)\cr The index of the feature(s) for which the partial dependence was computed.}
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
#' \item{plot()}{method to plot the partial dependence function. See \link{plot.Partial}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' @seealso 
#' \link{plot.Partial} 
#' 
#' @references 
#' Friedman, J.H. 2001. "Greedy Function Approximation: A Gradient Boosting Machine." Annals of Statistics 29: 1189-1232.
#' 
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2013). Peeking Inside the Black Box: 
#' Visualizing Statistical Learning with Plots of Individual Conditional Expectation, 1-22. https://doi.org/10.1080/10618600.2014.907095 
#' @importFrom data.table melt
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
#' pdp.obj = Partial$new(mod, feature = "crim")
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
#' # Only plotting the aggregated partial dependence:  
#' pdp.obj = Partial$new(mod, feature = "crim", ice = FALSE)
#' pdp.obj$plot() 
#'
#' # Only plotting the individual conditional expectation:  
#' pdp.obj = Partial$new(mod, feature = "crim", aggregation = "none")
#' pdp.obj$plot() 
#'   
#' # Partial dependence plots support up to two features: 
#' pdp.obj = Partial$new(mod, feature = c("crim", "lstat"))  
#' plot(pdp.obj)
#' 
#' 
#' # Partial dependence plots also works with multiclass classification
#' rf = randomForest(Species ~ ., data = iris, ntree=50)
#' predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun)
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(Partial$new(mod, feature = "Petal.Width"))
#'
#' # Partial dependence plots support up to two features: 
#' pdp.obj = Partial$new(mod, feature = c("Sepal.Length", "Petal.Length"))
#' pdp.obj$plot()   
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Predictor$new(rf, data = iris, predict.fun = predict.fun, class = 1)
#' plot(Partial$new(mod, feature = "Sepal.Length"))
#' }
NULL



#' @export

Partial = R6::R6Class("Partial", 
  inherit = InterpretationMethod,
  public = list(
    ice = NULL,
    aggregation = NULL,
    grid.size = NULL, 
    feature.index = NULL, 
    feature.name = NULL,
    n.features = NULL, 
    feature.type= NULL,
    initialize = function(predictor, feature, ice = TRUE, aggregation = "pdp", center.at = NULL, grid.size = 20, run = TRUE) {
      feature = private$sanitize.feature(feature, predictor$data$feature.names)
      assert_numeric(feature, lower = 1, upper = predictor$data$n.features, min.len = 1, max.len = 2)
      assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      assert_number(center.at, null.ok = TRUE)
      assert_logical(ice)
      assert_choice(aggregation, c("none", "pdp"))
      if (aggregation == "none" & !ice) stop("ice can't be FALSE and aggregation 'none' at the same time")
      if (length(feature) == 2) { 
        assert_false(feature[1] == feature[2])
        ice = FALSE
        center.at = NULL
      }
      private$anchor.value = center.at
      self$ice = ice
      self$aggregation = aggregation
      super$initialize(predictor)
      private$setFeatureFromIndex(feature)
      private$set.grid.size(grid.size)
      private$grid.size.original = grid.size
      if(run) self$run()
    }, 
    set.feature = function(feature) {
      feature = private$sanitize.feature(feature, self$predictor$data$feature.names)
      private$flush()
      private$setFeatureFromIndex(feature)
      private$set.grid.size(private$grid.size.original)
      self$run()
    },
    center = function(center.at) {
      private$anchor.value = center.at
      private$flush()
      self$run()
    }
  ), 
  private = list(
    anchor.value = NULL,
    aggregate = function() {
      x.id = private$data.design.id
      results.ice = private$dataDesign[, self$feature.index, with = FALSE]
      results.ice$.id = x.id
      if (private$multiClass) {
        y.hat.names = colnames(private$qResults)
        results.ice = cbind(results.ice, private$qResults)
        results.ice = melt(results.ice, variable.name = ".class.name", 
          value.name = ".y.hat", measure.vars = y.hat.names)
      } else {
        results.ice[, ".y.hat" := private$qResults]
        results.ice$.class.name = 1
      }
      
      if (!is.null(private$anchor.value)) {
        anchor.index = which(results.ice[,self$feature.name, with=FALSE] == private$anchor.value)
        X.aggregated.anchor = results.ice[anchor.index, c(".y.hat", ".id", ".class.name"), with = FALSE]
        names(X.aggregated.anchor) = c("anchor.yhat", ".id", ".class.name")
        # In case that the anchor value was also part of grid
        X.aggregated.anchor = unique(X.aggregated.anchor)
        results.ice = merge(results.ice, X.aggregated.anchor, by = c(".id", ".class.name"))
        results.ice$.y.hat = results.ice$.y.hat - results.ice$anchor.yhat
        results.ice$anchor.yhat = NULL
      }
      
      results = data.table()
      if (self$aggregation == "pdp") {
        if (private$multiClass) {
          results.aggregated = results.ice[, list(.y.hat = mean(.y.hat)), 
            by = c(self$feature.name, ".class.name")]
        } else {
          results.aggregated = results.ice[, list(.y.hat = mean(.y.hat)), 
            by = c(self$feature.name)]
        }
        results.aggregated$.type = "pdp"
        results = rbind(results, results.aggregated)
      }
      if (!private$multiClass) { 
        results.ice$.class.name = NULL
      }
      if (self$ice) {
        results.ice$.type = "ice"
        results = rbind(results, results.ice, fill = TRUE)
      }
      results
    },
    intervene = function() {
      grid = get.1D.grid(private$dataSample[[self$feature.index[1]]], self$feature.type[1], self$grid.size[1])
      
      private$data.design.id = rep(1:nrow(private$dataSample), times = length(grid))
      dataDesign = private$dataSample[private$data.design.id,]
      dataDesign[, self$feature.index[1]] = rep(grid, each = nrow(private$dataSample))
      
      if (!is.null(private$anchor.value)) {
        dataDesign.anchor = private$dataSample
        dataDesign.anchor[, self$feature.index] = private$anchor.value
        private$data.design.id = c(private$data.design.id, 1:nrow(private$dataSample))
        dataDesign = rbind(dataDesign, dataDesign.anchor)
      }
      if (self$n.features == 2) {
        grid2 = get.1D.grid(private$dataSample[[self$feature.index[2]]], self$feature.type[2], self$grid.size[2])
        private$data.design.id = rep(private$data.design.id, times = length(grid2))
        dataDesign2 = dataDesign[rep(1:nrow(dataDesign), times = length(grid2)), ]
        dataDesign2[, self$feature.index[2]] = rep(grid2, each = nrow(dataDesign))
        return(dataDesign2)
      }
      dataDesign
    }, 
    data.design.id = NULL, 
    grid.size.original = NULL,
    setFeatureFromIndex = function(feature.index) {
      self$feature.index = feature.index
      self$n.features = length(feature.index)
      self$feature.type = private$sampler$feature.types[feature.index]
      self$feature.name = private$sampler$feature.names[feature.index]
    },
    printParameters = function() {
      cat("features:", paste(sprintf("%s[%s]", 
        self$feature.name, self$feature.type), collapse = ", "))
      cat("\ngrid size:", paste(self$grid.size, collapse = "x"))
    },
    generatePlot = function(rug = TRUE) {
      if (is.null(private$anchor.value)) {
        y.axis.label = expression(hat(y))
      } else {
        y.axis.label = bquote(hat(y)-hat(y)[x == .(private$anchor.value)])
      }
      
      if (self$n.features == 1) {
        if (self$ice) {
          p = ggplot(self$results[self$results$.type == "ice",], 
            mapping = aes_string(x = self$feature.name, 
              y = ".y.hat", group = ".id")) + scale_y_continuous(y.axis.label)
        } else {
          p = ggplot(self$results, mapping = aes_string(x = self$feature.name, y = ".y.hat")) + 
            scale_y_continuous(y.axis.label)
        }
        if (self$feature.type == "numerical") {
          p = p + geom_line(alpha = 0.2) 
        } else if (self$feature.type == "categorical") {
          p = p + geom_boxplot(aes_string(group = self$feature.name)) 
        }
        if (self$aggregation != "none") {
          aggr = self$results[self$results$.type == "pdp", ]
          if (self$ice) {
            p = p + geom_line(data = aggr, mapping = aes_string(x = self$feature.name, y = ".y.hat"), 
              size = 2, color = "gold") 
          }
          p = p + geom_line(data = aggr, mapping = aes_string(x = self$feature.name, y = ".y.hat"), 
            size = 1, color = "black")
        }
      } else if (self$n.features == 2) {
        if (all(self$feature.type %in% "numerical") | all(self$feature.type %in% "categorical")) {
          p = ggplot(self$results, mapping = aes_string(x = self$feature.name[1], 
            y = self$feature.name[2], 
            fill = ".y.hat")) + geom_tile() + 
            scale_fill_continuous(y.axis.label)
        } else {
          categorical.feature = self$feature.name[self$feature.type=="categorical"]
          numerical.feature = setdiff(self$feature.name, categorical.feature)
          p = ggplot(self$results, mapping = aes_string(x = numerical.feature, y = ".y.hat", 
            group = categorical.feature, color = categorical.feature)) + 
            geom_line() + scale_y_continuous(y.axis.label)
        }
      }
      if (rug) {
        rug.dat = cbind(private$sampler$get.x(), data.frame(.y.hat = self$results$.y.hat[1]), 
          .id = 1)
        rug.dat = rug.dat[sample(1:nrow(rug.dat)),]
        sides = ifelse(self$n.features == 2 && self$feature.type[1] == self$feature.type[2], "bl", "b")
        p = p + geom_rug(data = rug.dat, alpha = 0.2, sides = sides, 
          position = position_jitter(width = 0.1, height = 0.1))
      }
      if (private$multiClass) {
        p = p + facet_wrap(".class.name")
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
    }
  ),
  active = list(
    center.at = function(x) {
      if(!missing(x)) warning("Please use $center() to change the value.")
      return(private$anchor.value)
    }
  )
)


#' Plot Partial Dependence
#' 
#' plot.Partial() plots the results of a Partial object.
#' 
#' @param x A Partial R6 object
#' @param rug [logical] Should a rug be plotted to indicate the feature distribution?
#' @return ggplot2 plot object
#' @seealso 
#' \link{Partial}
#' @examples
#' # We train a random forest on the Boston dataset:
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the partial dependence for the first feature
#' pdp.obj = Partial$new(mod, feature = "crim")
#' 
#' # Plot the results directly
#' plot(pdp.obj)
#' }
plot.Partial = function(x, rug = TRUE) {
  x$plot(rug)
}
