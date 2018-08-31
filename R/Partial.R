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
#' Advantages over ALEPlot implementation in the ALEPlot package
#' Plot is only produced when deciding to plot, can be used for multi class, returns ggplot which can be modified,
#' don't have to always define the predict function
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
#' rf = lm(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = Boston)
#' 
#' # Compute the partial dependence for the first feature
#' pdp.obj = Partial$new(mod, feature = "rm", aggregation = "ale", grid.size = 30, ice = FALSE)
#' pdp.obj$plot()
#' y.fun = function(X.model, newdata) {
#' X.model$predict(newdata)[[1]] 
#' }
#' ALEPlot::ALEPlot(Boston, mod, J = "rm", pred.fun = y.fun)
#' 
#' pdp.obj = Partial$new(mod, feature = "rm", aggregation = "pdp", grid.size = 30)
#' plot(pdp.obj)
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
#' mod = Predictor$new(rf, data = iris, type = "prob")
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(Partial$new(mod, feature = "Petal.Width"))
#'
#' # Partial dependence plots support up to two features: 
#' pdp.obj = Partial$new(mod, feature = c("Sepal.Length", "Petal.Length"))
#' pdp.obj$plot()   
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' mod = Predictor$new(rf, data = iris, type = "prob", class = 1)
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
    feature.name = NULL,
    n.features = NULL, 
    feature.type= NULL,
    initialize = function(predictor, feature, ice = TRUE, aggregation = "pdp", center.at = NULL, grid.size = 20, run = TRUE) {
      feature = private$sanitize.feature(feature, predictor$data$feature.names)
      assert_numeric(feature, lower = 1, upper = predictor$data$n.features, min.len = 1, max.len = 2)
      assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      assert_number(center.at, null.ok = TRUE)
      assert_logical(ice)
      assert_choice(aggregation, c("none", "pdp", "ale"))
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
      if(run) self$run(self$predictor$batch.size)
    }, 
    set.feature = function(feature) {
      feature = private$sanitize.feature(feature, self$predictor$data$feature.names)
      private$flush()
      private$setFeatureFromIndex(feature)
      private$set.grid.size(private$grid.size.original)
      self$run(self$predictor$batch.size)
    },
    center = function(center.at) {
      if(self$aggregation == "ale") {
        warning("Centering only works for only for PDPs and ICE, but not for ALE Plots, .")
        return(NULL)
      }
      private$anchor.value = center.at
      private$flush()
      self$run(self$predictor$batch.size)
    },
    run = function(n) {
      if(self$aggregation == "ale") {
        self$ice = FALSE
        self$run.ale() 
      } else {
        self$run.pdp(self$predictor$batch.size)
      }
    },
    # TODO: Add vignette on Partial, compare also to ALEPLot
    # TODO: Implement 2D for categorical?? Or just create an issue
    # TODO: Implement 2D for categorical x numerical
    # TODO: Check difference for K higher between original and iml implementation. Difference seems to be in the grid already at K >= 10.
    #        Maybe it doesn't matter.
    # TODO: Remove inst test stuff
    # TODO: Update documentation
    # TODO: Rename result columns to something meaningful to the user
    # TODO: Cite paper
    # TODO: Make aggregation = "ale" the default, or at least put a warning or deprecation that it will become default
    # TODO: Test order_levels
    # TODO: Continue ale.cat
    # TODO: implement barplot for ale.cat
    # TODO: Add option to plot total effects for 1D and 2D. for this add ale0, ale1, ale2 to results
    # TODO MAYBE: Allow to plot data as points into the plot??
    # TODO: Implement nearest neighbour tile for 2D plot or create issue
    # TODO: Renam res to deltas within the functions?
    # TODO: Fix cat x num plot
    # TODO: Compare cat x num with implementation from ALEPLot package
    # TODO: Implement NA behaviour for 2D num x num. seems to affect computation.
    # TODO: Implement NA behaviour for 2D cat x num. seems to affect computation.
    # TODO: Make grid.dt a vector, maybe that simplifies things
    # TODO: Thoroughly document code, because it will be tough to understand it again
    #       Maybe rename some variable to reflect if you are working with the cells or with the cell edges
    # TODO: Test impute_cells
    # TODO: Use impute_cells for numerical x numerical
    # TODO: Test num x cat 
    run.ale = function() {
      private$dataSample = private$getData()
      if(self$n.features  == 1) {
        if(self$feature.type == "numerical") { # one numerical feature
          results = calculate.ale.num(dat = private$dataSample, run.prediction = private$run.prediction, 
            feature.name = self$feature.name, grid.size = self$grid.size)
        } else { # one categorical feature
          results = calculate.ale.cat(dat = private$dataSample, run.prediction = private$run.prediction, 
            feature.name = self$feature.name)
        }
      } else { # two features
        if(all(self$feature.type == "numerical")){ # two numerical features
          results = calculate.ale.num.num(dat = private$dataSample, run.prediction = private$run.prediction, 
            feature.name = self$feature.name, grid.size = self$grid.size)
        } else if(all(self$feature.type == "categorical")) { # two categorical features
          stop("ALE for two categorical features is not yet implemented.")
        } else { # mixed numerical and categorical
          results = calculate.ale.num.cat(dat = private$dataSample, run.prediction = private$run.prediction, 
            feature.name = self$feature.name, grid.size = self$grid.size)
        }
      }
      # only keep .class when multiple outputs
      if(!private$multiClass) results$.class = NULL
      self$results = results
    },
    run.pdp = function(n) {
      private$dataSample = private$getData()
      grid.dt = get.grid(private$getData()[,self$feature.name, with = FALSE], self$grid.size, anchor.value = private$anchor.value)
      mg = MarginalGenerator$new(grid.dt, private$dataSample, self$feature.name, id.dist = TRUE, cartesian = TRUE)
      results.ice = data.table()
      while(!mg$finished) {
        results.ice.inter = mg$next.batch(n)
        predictions = private$run.prediction(results.ice.inter)
        results.ice.inter = results.ice.inter[, c(self$feature.name, ".id.dist"), with = FALSE]
        if (private$multiClass) {
          y.hat.names = colnames(predictions)
          results.ice.inter = cbind(results.ice.inter, predictions)
          results.ice.inter = melt(results.ice.inter, variable.name = ".class", 
            value.name = ".y.hat", measure.vars = y.hat.names)
        } else {
          results.ice.inter[, ".y.hat" := predictions]
          results.ice.inter$.class = 1
        }
        results.ice = rbind(results.ice, results.ice.inter)
      }
      
      if (!is.null(private$anchor.value)) {
        anchor.index = which(results.ice[,self$feature.name, with=FALSE] == private$anchor.value)
        X.aggregated.anchor = results.ice[anchor.index, c(".y.hat", ".id.dist", ".class"), with = FALSE]
        names(X.aggregated.anchor) = c("anchor.yhat", ".id.dist", ".class")
        # In case that the anchor value was also part of grid
        X.aggregated.anchor = unique(X.aggregated.anchor)
        results.ice = merge(results.ice, X.aggregated.anchor, by = c(".id.dist", ".class"))
        results.ice$.y.hat = results.ice$.y.hat - results.ice$anchor.yhat
        results.ice$anchor.yhat = NULL
      }
      results = data.table()
      if (self$aggregation == "pdp") {
        if (private$multiClass) {
          results.aggregated = results.ice[, list(.y.hat = mean(.y.hat)), 
            by = c(self$feature.name, ".class")]
        } else {
          results.aggregated = results.ice[, list(.y.hat = mean(.y.hat)), 
            by = c(self$feature.name)]
        }
        results.aggregated$.type = "pdp"
        results = rbind(results, results.aggregated)
      }
      if (!private$multiClass) { 
        results.ice$.class = NULL
      }
      if (self$ice) {
        results.ice$.type = "ice"
        results = rbind(results, results.ice, fill = TRUE)
        results$.id = results$.id.dist
        results$.id.dist = NULL
      }
      self$results = data.frame(results)
    }
  ), 
  private = list(
    anchor.value = NULL,
    grid.size.original = NULL,
    setFeatureFromIndex = function(feature.index) {
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
        if(self$aggregation == "ale") {
          y.axis.label = "ALE"
        } else {
          y.axis.label = expression(hat(y))
        }
      } else {
        y.axis.label = bquote(hat(y)-hat(y)[x == .(private$anchor.value)])
      }
      
      if (self$n.features == 1) {
        y.name = ifelse(self$aggregation == "ale", ".ale", ".y.hat")
        if (self$ice) {
          p = ggplot(self$results[self$results$.type == "ice",], 
            mapping = aes_string(x = self$feature.name, 
              y = ".y.hat", group = ".id")) + scale_y_continuous(y.axis.label)
        } else {
          p = ggplot(self$results, mapping = aes_string(x = self$feature.name, y = y.name)) + 
            scale_y_continuous(y.axis.label)
        }
        if (self$feature.type == "numerical") {
          p = p + geom_line(alpha = 0.2) 
        } else if (self$feature.type == "categorical") {
          p = p + geom_boxplot(aes_string(group = self$feature.name)) 
        }
        if (self$aggregation != "none") {
          aggr = self$results[self$results$.type != "ice", ]
          if (self$ice) {
            p = p + geom_line(data = aggr, mapping = aes_string(x = self$feature.name, y = y.name), 
              size = 2, color = "gold") 
          }
          p = p + geom_line(data = aggr, mapping = aes_string(x = self$feature.name, y = y.name), 
            size = 1, color = "black")
        }
      } else if (self$n.features == 2) {
        if (self$aggregation == "ale") {
          if(any(self$feature.type %in% "categorical")){
            browser()
            categorical.feature = self$feature.name[self$feature.type=="categorical"]
            numerical.feature = setdiff(self$feature.name, categorical.feature)
            p = ggplot(self$results, x = as.numeric(self$results[categorical.feature])) + 
              geom_rect(aes(ymin = .bottom, ymax = .top, fill = .ale), xmin = self$results$.left, xmax = self$results$.right) + 
              scale_x_continuous(self$feature.name[1]) + scale_y_continuous(self$feature.name[2]) + 
              scale_fill_continuous(y.axis.label)
          } else {
            # Adding x and y to aesthetics for the rug plot later
            p = ggplot(self$results, mapping = aes_string(x = self$feature.name[1], y = self$feature.name[2])) + 
              geom_rect(aes(xmin = .left, xmax = .right, ymin = .bottom, ymax = .top, fill = .ale)) + 
              scale_x_continuous(self$feature.name[1]) + scale_y_continuous(self$feature.name[2]) + 
              scale_fill_continuous(y.axis.label)
          }
        } else  if (all(self$feature.type %in% "numerical") | all(self$feature.type %in% "categorical")) {
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
        # Need some dummy data for ggplot to accept the data.frame
        rug.dat = cbind(private$sampler$get.x(), data.frame(.y.hat = 1, .id = 1, .ale = 1))
        rug.dat = rug.dat[sample(1:nrow(rug.dat)),]
        sides = ifelse(self$n.features == 2 && self$feature.type[1] == self$feature.type[2], "bl", "b")
        p = p + geom_rug(data = rug.dat, alpha = 0.2, sides = sides, 
          position = position_jitter(width = 0.1, height = 0.1))
      }
      if (private$multiClass) {
        p = p + facet_wrap(".class")
      } 
      p
    }, 
    # This function ensures that the grid is always of length 2 when 2 features are provided
    set.grid.size = function(size) {
      self$grid.size = numeric(length=self$n.features)
      names(self$grid.size) = self$feature.name
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
        size, length(unique(private$sampler$get.x()[[self$feature.name[feature.number]]])))
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
#' @param rug [logical] Should a rug be plotted to indicate the feature distribution? The rug will be jittered a bit, so the location may not be exact, 
#' but it avoids overplotting.
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
