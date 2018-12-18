#' Effect of features on the model predictions (deprecated)
#' 
#' Deprecated, please use 'FeatureEffect', see ?FeatureEffect
#' 
#' @format \code{\link{R6Class}} object.
#' @name Partial
#' 
#' @seealso 
#' \link{FeatureEffect} 
NULL

#' @export
Partial = R6::R6Class("Partial", 
  inherit = FeatureEffect,
  public = list(
    initialize = function(predictor, feature, aggregation = "pdp",ice = TRUE, center.at = NULL, grid.size = 20, run = TRUE) {
      assert_choice(aggregation, c("ale", "pdp", "none"))
      assert_logical(ice)
      if(length(feature) == 2) ice = FALSE
      .Deprecated("FeatureEffect", "iml", "The FeatureEffect class replaces the Partial class. Partial will be removed in future versions.")
      if(aggregation == "none") method = "ice"
      if((aggregation == "pdp") & ice) method = "pdp+ice"
      if((aggregation == "pdp") & !ice) method = "pdp"
      if(aggregation == "ale") method = "ale"
      super$initialize(predictor=predictor, feature=feature, method = method, center.at = center.at, 
        grid.size = grid.size, run = run)
    }))



#' Effect of features on the model predictions
#' 
#' \code{FeatureEffect} computes and plots (individual) feature effects of prediction models. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name FeatureEffect
#' 
#' @section Usage:
#' \preformatted{
#' effect = FeatureEffect$new(predictor, feature, method = "ale", 
#'     grid.size = 20,  center.at = NULL, run = TRUE)
#' 
#' plot(effect)
#' effect$results
#' print(effect)
#' effectd$set.feature("x2")
#' }
#' 
#' @section Arguments:
#' 
#' For FeatureEffect$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{feature: }{(`character(1)` | `character(2)` | `numeric(1)` | `numeric(2)`) \cr The feature name or index for which to compute the partial dependencies.}
#' \item{method: }{(`character(1)`)\cr 
#' 'ale' for accumulated local effects (the default), 
#' 'pdp' for partial dependence plot, 
#' 'ice' for individual conditional expectation curves,
#' 'pdp+ice' for partial dependence plot and ice curves within the same plot.}
#' \item{center.at: }{(`numeric(1)`)\cr Value at which the plot should be centered. Ignored in the case of two features.}
#' \item{grid.size: }{(`numeric(1)` | `numeric(2)`)\cr The size of the grid for evaluating the predictions}
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
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
#' Accumuluated local effects and partial dependence plots both show the average model prediction over the feature.
#' The difference is that ALE are computed as accumulated differences over the conditional distribution and partial dependence plots 
#' over the marginal distribution.
#' ALE plots preferable to PDPs, because they are faster and unbiased when features are correlated.
#' 
#' ALE plots for categorical features are automatically ordered by the similarity of the categories based on the distribution of the other features for instances in a category.
#' When the feature is an ordered factor, the ALE plot leaves the order as is.
#' 
#' Individual conditional expectation curves describe how, for a single
#' observation, the prediction changes when the feature changes and can be combined with partial dependence plots.
#' 
#' To learn more about accumulated local effects, read the Interpretable Machine Learning book: 
#' \url{https://christophm.github.io/interpretable-ml-book/ale.html}
#' 
#' And for the partial dependence plot:
#' \url{https://christophm.github.io/interpretable-ml-book/pdp.html}
#' 
#' And for individual conditional expectation: 
#' \url{https://christophm.github.io/interpretable-ml-book/ice.html}
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

FeatureEffect = R6::R6Class("FeatureEffect", 
  inherit = InterpretationMethod,
  public = list(
    ice = NULL,
    aggregation = NULL,
    grid.size = NULL, 
    feature.name = NULL,
    n.features = NULL, 
    feature.type = NULL,
    method  = NULL,
    initialize = function(predictor, feature, method = "ale", center.at = NULL, grid.size = 20, run = TRUE) {
      feature_index = private$sanitize.feature(feature, predictor$data$feature.names)
      assert_numeric(feature_index, lower = 1, upper = predictor$data$n.features, min.len = 1, max.len = 2)
      assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      assert_number(center.at, null.ok = TRUE)
      assert_choice(method, c("ale", "pdp", "ice", "pdp+ice"))
      self$method = method
      if (length(feature_index) == 2) { 
        assert_false(feature_index[1] == feature_index[2])
        center.at = NULL
        if(method %in% c("ice", "pdp+ice")) {
          stop("ICE is not implemented for two features.")
        } 
      }
      private$anchor.value = center.at
      super$initialize(predictor)
      private$set_feature_from_index(feature_index)
      if(length(feature_index) == 1 & length(unique(self$predictor$data$get.x()[[feature_index]])) == 1) 
        stop("feature has only one unique value")
      private$set.grid.size(grid.size)
      private$grid.size.original = grid.size
      if(run) self$run(self$predictor$batch.size)
    }, 
    set.feature = function(feature) {
      feature = private$sanitize.feature(feature, self$predictor$data$feature.names)
      private$flush()
      private$set_feature_from_index(feature)
      private$set.grid.size(private$grid.size.original)
      self$run(self$predictor$batch.size)
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
    run = function(n) {
      if(self$method == "ale") {
        private$run.ale() 
      } else {
        private$run.pdp(self$predictor$batch.size)
      }
    }),
  private = list(
    anchor.value = NULL,
    grid.size.original = NULL,
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
      if (self$method %in% c("pdp", "pdp+ice")) {
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
      if (self$method %in% c("ice", "pdp+ice")) {
        results.ice$.type = "ice"
        results = rbind(results, results.ice, fill = TRUE)
        results$.id = results$.id.dist
        results$.id.dist = NULL
        # sory by id
        setkeyv(results, ".id")
      }
      self$results = data.frame(results)
    }
    , 
    set_feature_from_index = function(feature.index) {
      self$n.features = length(feature.index)
      self$feature.type = private$sampler$feature.types[feature.index]
      self$feature.name = private$sampler$feature.names[feature.index]
    },
    printParameters = function() {
      cat("features:", paste(sprintf("%s[%s]", 
        self$feature.name, self$feature.type), collapse = ", "))
      cat("\ngrid size:", paste(self$grid.size, collapse = "x"))
    },
    # make sure the default arguments match with plot.FeatureEffect
    generatePlot = function(rug = TRUE, show.data=FALSE) {
      if (is.null(private$anchor.value)) {
        if(self$method == "ale") {
          y_axis_label = "ALE"
          if(!is.null(self$predictor$data$y.names) & self$n.features == 1) {
            axis_label_names = paste(self$predictor$data$y.names, sep = ", ")
            y_axis_label = sprintf("ALE of %s", axis_label_names)
          }
        } else {
          y_axis_label = expression(hat(y))
          if(!is.null(self$predictor$data$y.names) & self$n.features == 1) {
            axis_label_names = paste(self$predictor$data$y.names, sep = ", ")
            y_axis_label = sprintf("Predicted %s", axis_label_names)
          }
        }
      } else {
        y_axis_label = bquote(hat(y)-hat(y)[x == .(private$anchor.value)])
      }
      
      if (self$n.features == 1) {
        y.name = ifelse(self$method == "ale", ".ale", ".y.hat")
        p = ggplot(self$results, 
          mapping = aes_string(x = self$feature.name, 
            y = y.name)) + scale_y_continuous(y_axis_label)
        if (self$feature.type == "categorical") {
          if (self$method %in% c("ice", "pdp+ice")){
            p = p + geom_boxplot(data = self$results[self$results$.type == "ice",], aes_string(group = self$feature.name))
          } else {
            p = p + geom_col() 
          }
        } else {
          if (self$method %in% c("ice", "pdp+ice")) {
            p = p + geom_line(alpha = 0.2, mapping = aes(group = .id))
          }
          if (self$method == "pdp+ice") {
            aggr = self$results[self$results$.type != "ice", ]
            p = p + geom_line(data = aggr, size = 2, color = "gold") 
          }
          if (self$method %in% c("ale", "pdp")) {
            p =  p + geom_line()
          }
        }
      } else if (self$n.features == 2) {
        if (self$method == "ale") {
          # 2D ALEPlot with categorical x numerical
          if(any(self$feature.type %in% "categorical")){
            res = self$results
            categorical.feature = self$feature.name[self$feature.type=="categorical"]
            numerical.feature = setdiff(self$feature.name, categorical.feature)
            res[,categorical.feature] = as.numeric(res[,categorical.feature])
            cat.breaks = unique(res[[categorical.feature]])
            cat.labels = levels(self$results[[categorical.feature]])[cat.breaks]
            p = ggplot(res, aes_string(x = categorical.feature, y = numerical.feature)) + 
              geom_rect(aes(ymin = .bottom, ymax = .top, fill = .ale, xmin = .left, xmax = .right)) + 
              scale_x_continuous(categorical.feature, breaks = cat.breaks, labels = cat.labels) + 
              scale_y_continuous(numerical.feature) + 
              scale_fill_continuous(y_axis_label)
            
            # A bit stupid, but can't adding a rug is special here, because i handle the 
            # categorical feature as a numeric feauture in the plot
            if (rug) {
              dat = private$sampler$get.x()
              levels(dat[[categorical.feature]]) = levels(self$results[, categorical.feature])
              dat[,categorical.feature] = as.numeric(dat[,categorical.feature,with=FALSE][[1]])
              # Need some dummy data for ggplot to accept the data.frame
              rug.dat = cbind(dat, data.frame(.y.hat = 1, .id = 1, .ale = 1))
              p = p + geom_rug(data = rug.dat, alpha = 0.2, sides = "bl", 
                position = position_jitter(width = 0.1, height = 0.1))
              rug = FALSE
            }
            if (show.data) {
              dat = private$sampler$get.x()
              levels(dat[[categorical.feature]]) = levels(self$results[, categorical.feature])
              dat[,categorical.feature] = as.numeric(dat[,categorical.feature,with=FALSE][[1]])
              p = p + geom_point(data = dat, alpha = 0.3)
              show.data = FALSE
            }
          } else {
            # Adding x and y to aesthetics for the rug plot later
            p = ggplot(self$results, mapping = aes_string(x = self$feature.name[1], y = self$feature.name[2])) + 
              geom_rect(aes(xmin = .left, xmax = .right, ymin = .bottom, ymax = .top, fill = .ale)) + 
              scale_x_continuous(self$feature.name[1]) + scale_y_continuous(self$feature.name[2]) + 
              scale_fill_continuous(y_axis_label)
          }
        } else  if (all(self$feature.type %in% "numerical") | all(self$feature.type %in% "categorical")) {
          p = ggplot(self$results, mapping = aes_string(x = self$feature.name[1], 
            y = self$feature.name[2])) + geom_tile(aes(fill = .y.hat)) + 
            scale_fill_continuous(y_axis_label)
        } else {
          categorical.feature = self$feature.name[self$feature.type=="categorical"]
          numerical.feature = setdiff(self$feature.name, categorical.feature)
          p = ggplot(self$results, mapping = aes_string(x = numerical.feature, y = ".y.hat")) + 
            geom_line(aes_string(group = categorical.feature, color = categorical.feature)) + 
            scale_y_continuous(y_axis_label)
          show.data = FALSE
        }
        
        if (show.data) {
          dat = private$sampler$get.x()
          dat[,self$feature.name] = lapply(dat[,self$feature.name,with=FALSE], as.numeric)
          p = p + geom_point(data = dat, alpha = 0.3)
        }
      }
      if (rug) {
        # Need some dummy data for ggplot to accept the data.frame
        rug.dat = private$sampler$get.x()
        rug.dat$.id =    ifelse(is.null(self$results$.id), NA, self$results$.id[1])
        rug.dat$.ale =   ifelse(is.null(self$results$.ale), NA, self$results$.ale[1])
        rug.dat$.y.hat = ifelse(is.null(self$results$.y.hat), NA, self$results$.y.hat[1])
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
