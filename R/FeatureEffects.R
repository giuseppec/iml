# FeatureEffects ---------------------------------------------------------------

#' @title Effect of a feature on predictions
#'
#' @description `FeatureEffects` computes and plots feature effects
#'   of multiple features at once.
#' @importFrom data.table melt setkeyv
#' @importFrom stats cmdscale ecdf quantile
#'
#' @details
#'
#' FeatureEffects computes the effects for all given features on the model
#' prediction. [FeatureEffects] is a convenience class that calls FeatureEffect
#' multiple times. See `?FeatureEffect` for details what's actually computed.
#'
#' Only first-order effects can be computed with the [FeatureEffects] interface.
#' If you are interested in the visualization of interactions between two
#' features, directly use [FeatureEffect].
#'
#' @seealso [plot.FeatureEffects]
#' @template parallel
#' @references
#' Apley, D. W. 2016. "Visualizing the Effects of Predictor Variables in Black
#' Box Supervised Learning Models." ArXiv Preprint.
#'
#' Friedman, J.H. 2001. "Greedy Function Approximation: A Gradient Boosting
#' Machine." Annals of Statistics 29: 1189-1232.
#'
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2013). Peeking
#' Inside the Black Box: Visualizing Statistical Learning with Plots of
#' Individual Conditional Expectation, 1-22.
#' https://doi.org/10.1080/10618600.2014.907095
#'
#' @examples
#' # We train a random forest on the Boston dataset:
#' library("rpart")
#' data("Boston", package = "MASS")
#' rf <- rpart(medv ~ ., data = Boston)
#' mod <- Predictor$new(rf, data = Boston)
#'
#' # Compute the accumulated local effects for all features
#' eff <- FeatureEffects$new(mod)
#' eff$plot()
#' \dontrun{
#' # Again, but this time with a partial dependence plot
#' eff <- FeatureEffects$new(mod, method = "pdp")
#' eff$plot()
#'
#' # Only a subset of features
#' eff <- FeatureEffects$new(mod, features = c("nox", "crim"))
#' eff$plot()
#'
#' # You can access each FeatureEffect individually
#'
#' eff.nox <- eff$effects[["nox"]]
#' eff.nox$plot()
#'
#'
#' # FeatureEffects also works with multiclass classification
#' rf <- rpart(Species ~ ., data = iris)
#' mod <- Predictor$new(rf, data = iris, type = "prob")
#'
#' FeatureEffects$new(mod)$plot(ncol = 2)
#' }
#' @name FeatureEffects
#' @export
FeatureEffects <- R6Class("FeatureEffects",
  inherit = InterpretationMethod,

  public = list(

    #' @description Create a FeatureEffects object
    #' @param predictor [Predictor]\cr
    #'   The object (created with `Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param features (`character()`)\cr
    #' The names of the features for which to compute the feature effects.
    #' @param feature (`character(1)` | `character(2)` | `numeric(1)` |
    #'   `numeric(2)`)\cr
    #'   The feature name or index for which to compute the effects.
    #' @param method (`character(1)`)\cr
    #' - 'ale' for accumulated local effects,
    #' - 'pdp' for partial dependence plot,
    #' - 'ice' for individual conditional expectation curves,
    #' - 'pdp+ice' for partial dependence plot and ice curves within the same
    #' plot.
    #' @param center.at (`numeric(1)`)\cr
    #'   Value at which the plot should be centered. Ignored in the case of two
    #'   features.
    #' @param grid.size (`numeric(1)` | `numeric(2)`)\cr
    #'   The size of the grid for evaluating the predictions.
    initialize = function(predictor,
                          features = NULL,
                          method = "ale",
                          center.at = NULL,
                          grid.size = 20) {

      if (is.null(features)) {
        self$features <- predictor$data$feature.names
      } else {
        stopifnot(all(features %in% predictor$data$feature.names))
        self$features <- features
      }
      assert_numeric(grid.size, min.len = 1)
      assert_number(center.at, null.ok = TRUE)
      assert_choice(method, c("ale", "pdp", "ice", "pdp+ice"))
      self$grid.size <- grid.size
      self$method <- method
      self$center.at <- center.at
      super$initialize(predictor)
      suppressPackageStartupMessages(private$run())
    },

    #' @field grid.size (`numeric(1)` | `numeric(2)`)\cr
    #'  The size of the grid.
    grid.size = NULL,

    #' @field method (`character(1)`)\cr
    #' - "ale" for accumulated local effects,
    #' - "pdp" for partial dependence plot,
    #' - "ice" for individual conditional expectation curves,
    #' - "pdp+ ice" for partial dependence plot and ice curves within the same
    #' plot.
    method = NULL,

    #' @field effects ([list])\cr
    #' Named list of FeatureEffects.
    effects = NULL,

    #' @field features (`character()`)\cr
    #' The names of the features for which the effects were computed.
    features = NULL,

    #' @field center.at [numeric]\cr
    #'  Value at which the plot was centered. Ignored in the case of two
    #'  features.
    center.at = NULL
  ),

  private = list(
    run = function() {
      effects <- future.apply::future_lapply(self$features,
        function(x) {
          feature_effect <- function(x, predictor, method, center.at, grid.size) {
            FeatureEffect$new(
              feature = x, predictor = predictor, method = method,
              center.at = center.at, grid.size = grid.size
            )
          }

          feature_effect(x,
            predictor = self$predictor,
            method = self$method,
            center.at = self$center.at,
            grid.size = self$grid.size
          )
        },
        future.seed = TRUE,
        future.packages = loadedNamespaces()
      )

      self$effects <- effects
      names(self$effects) <- self$features
      self$results <- future.apply::future_lapply(self$effects, function(x) {
        res <- x$results
        fname.index <- which(colnames(res) %in% names(self$effects))
        res$.feature <- colnames(res)[fname.index]
        colnames(res)[fname.index] <- ".borders"
        res
      },
      future.seed = TRUE
      )
      private$finished <- TRUE
    },

    printParameters = function() {
      cat("features:", paste(sprintf(
        "%s",
        self$features
      ), collapse = ", "))
      cat("\ngrid size:", paste(self$grid.size, collapse = "x"))
    },

    # make sure the default arguments match with plot.FeatureEffect
    generatePlot = function(features = NULL, ncols = NULL, nrows = NULL,
                            fixed_y = TRUE, ...) {

      assert_character(features, null.ok = TRUE)
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("patchwork", quietly = TRUE)
      if (length(features) > 0) {
        assert_true(all(features %in% self$features))
      } else {
        features <- self$features
      }

      if (fixed_y) {
        res <- unlist(future.apply::future_lapply(features, function(fname) {
          values <- self$effects[[fname]]$results[".value"]
          c(min(values), max(values))
        }))
        ylim <- c(min(res), max(res))
      } else {
        ylim <- c(NA, NA)
      }
      plts <- future.apply::future_lapply(features, function(fname) {
        self$effects[[fname]]$plot(..., ylim = ylim) +
          theme(axis.title.y = element_blank())
      })

      y_axis_label <- self$effects[[1]]$.__enclos_env__$private$y_axis_label

      patchwork::wrap_plots(plts, nrow = nrows, ncol = ncols) +
        patchwork::plot_annotation(title = y_axis_label)
    }
  )
)

# Plot.FeatureEffects ----------------------------------------------------------

#' Plot FeatureEffect
#'
#' plot.FeatureEffect() plots the results of a FeatureEffect object.
#'
#' @importFrom gridExtra marrangeGrob
#' @details
#'
#' In contrast to other plot methods in {iml}, for FeatureEffects the returned
#' plot is not a ggplot2 object, but a grid object, a collection of multiple
#' ggplot2 plots.
#'
#' @param x A [FeatureEffect] object.
#' @param features [character] For which features should the effects be
#'   plotted? Default is all features. You can also sort the order of the plots
#'   with this argument.
#' @param ncols The number of columns in the table of graphics
#' @param nrows The number of rows in the table of graphics
#' @param fixed_y Should the y-axis range be the same for all effects? Defaults
#'   to TRUE.
#' @param ... Further arguments for `FeatureEffect$plot()`
#' @return grid object
#' @seealso [FeatureEffects] [plot.FeatureEffect]
#' @examples
#' # We train a random forest on the Boston dataset:
#' library("randomForest")
#' data("Boston", package = "MASS")
#' rf <- randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod <- Predictor$new(rf, data = Boston)
#'
#' # Compute the partial dependence for the first feature
#' eff <- FeatureEffects$new(mod)
#'
#' # Plot the results directly
#' eff$plot()
#'
#' # For a subset of features
#' eff$plot(features = c("lstat", "crim"))
#'
#' # With a different layout
#' eff$plot(nrows = 2)
plot.FeatureEffects <- function(x, features = NULL, nrows = NULL, ncols = NULL,
                                fixed_y = TRUE, ...) {
  x$plot(
    features = features, nrows = nrows, ncols = ncols,
    fixed_y = fixed_y, ...
  )
}
