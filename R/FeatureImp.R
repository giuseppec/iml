#' Feature importance
#'
#' @description
#' `FeatureImp` computes feature importance for prediction models. The
#' importance is measured as the factor by which the model's prediction error
#' increases when the feature is shuffled.
#'
#' @details
#'
#' To compute the feature importance for a single feature, the model prediction
#' loss (error) is measured before and after shuffling the values of the
#' feature. By shuffling the feature values, the association between the outcome
#' and the feature is destroyed. The larger the increase in prediction error,
#' the more important the feature was. The shuffling is repeated to get more
#' accurate results, since the permutation feature importance tends to be quite
#' unstable.
#' Read the Interpretable Machine Learning book to learn about feature
#' importance in detail:
#' \url{https://christophm.github.io/interpretable-ml-book/feature-importance.html}
#'
#' The loss function can be either specified via a string, or by handing a
#' function to `FeatureImp()`. If you want to use your own loss function it
#' should have this signature:
#'
#' ```
#' function(actual, predicted)
#' ```
#'
#' Using the string is
#' a shortcut to using loss functions from the `Metrics` package. Only use
#' functions that return a single performance value, not a vector. Allowed
#' losses are: `"ce"`, `"f1"`, `"logLoss"`, `"mae"`, `"mse"`, `"rmse"`, `"mape"`,
#' `"mdae"`, `"msle"`, `"percent_bias"`, `"rae"`, `"rmse"`, `"rmsle"`, `"rse"`,
#' `"rrse"` and `"smape"`.
#'
#'  See `library(help = "Metrics")` to get a list of functions.
#'
#' @references
#' Fisher, A., Rudin, C., and Dominici, F. (2018). Model Class Reliance:
#' Variable Importance Measures for any Machine Learning Model Class, from the
#' "Rashomon" Perspective. Retrieved from http://arxiv.org/abs/1801.01489
#'
#' @import Metrics
#' @importFrom data.table copy rbindlist
#' @template parallel
#' @examples
#' library("rpart")
#' # We train a tree on the Boston dataset:
#' data("Boston", package = "MASS")
#' tree <- rpart(medv ~ ., data = Boston)
#' y <- Boston$medv
#' X <- Boston[-which(names(Boston) == "medv")]
#' mod <- Predictor$new(tree, data = X, y = y)
#'
#'
#' # Compute feature importances as the performance drop in mean absolute error
#' imp <- FeatureImp$new(mod, loss = "mae")
#'
#' # Plot the results directly
#' plot(imp)
#'
#'
#' # Since the result is a ggplot object, you can extend it:
#' library("ggplot2")
#' plot(imp) + theme_bw()
#' # If you want to do your own thing, just extract the data:
#' imp.dat <- imp$results
#' head(imp.dat)
#' ggplot(imp.dat, aes(x = feature, y = importance)) +
#'   geom_point() +
#'   theme_bw()
#'
#' # We can also look at the difference in model error instead of the ratio
#' imp <- FeatureImp$new(mod, loss = "mae", compare = "difference")
#'
#' # Plot the results directly
#' plot(imp)
#'
#' # We can calculate feature importance for a subset of features 
#' imp <- FeatureImp$new(mod, loss = "mae", features = c("crim", "zn", "indus"))
#' plot(imp)
#'
#'# We can calculate joint importance of groups of features
#'groups = list(
#'  grp1 = c("crim", "zn", "indus", "chas"),
#'  grp2 = c("nox", "rm", "age", "dis"),
#'  grp3 = c("rad", "tax", "ptratio", "black", "lstat")
#')
#'imp <- FeatureImp$new(mod, loss = "mae", features = groups)
#'plot(imp)
#'
#' # FeatureImp also works with multiclass classification.
#' # In this case, the importance measurement regards all classes
#' tree <- rpart(Species ~ ., data = iris)
#' X <- iris[-which(names(iris) == "Species")]
#' y <- iris$Species
#' mod <- Predictor$new(tree, data = X, y = y, type = "prob")
#'
#' # For some models we have to specify additional arguments for the predict function
#' imp <- FeatureImp$new(mod, loss = "ce")
#' plot(imp)
#'
#' # For multiclass classification models, you can choose to only compute
#' # performance for one class.
#' # Make sure to adapt y
#' mod <- Predictor$new(tree,
#'   data = X, y = y == "virginica",
#'   type = "prob", class = "virginica"
#' )
#' imp <- FeatureImp$new(mod, loss = "ce")
#' plot(imp)
#' @name FeatureImp
#' @export
FeatureImp <- R6::R6Class("FeatureImp",
  inherit = InterpretationMethod,

  public = list(

    #' @description Create a FeatureImp object
    #' @param predictor [Predictor]\cr
    #'   The object (created with `Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param loss (`character(1)` | [function])\cr
    #'   The loss function. Either the name of a loss (e.g. `"ce"` for
    #'   classification or `"mse"`) or a function. See Details for allowed
    #'   losses.
    #' @param compare (`character(1)`)\cr
    #' Either `"ratio"` or `"difference"`.
    #' Should importance be measured as the difference or as the ratio of
    #' original model error and model error after permutation?
    #' - Ratio: error.permutation/error.orig
    #' - Difference: error.permutation - error.orig
    #' @param n.repetitions (`numeric(1)`)\cr
    #' How often should the shuffling of the feature be repeated?
    #' The higher the number of repetitions the more stable and accurate the
    #' results become.
    #' @param features (`character or list`)\cr
    #' For which features do you want importance scores calculated. The default
    #' value of `NULL` implies all features. Use a named list of character vectors
    #' to define groups of features for which joint importance will be calculated.
    #' See examples.
    #' @return (data.frame)\cr
    #' data.frame with the results of the feature importance computation. One
    #' row per feature with the following columns:
    #' - importance.05 (5% quantile of importance values from the repetitions)
    #' - importance (median importance)
    #' - importance.95 (95% quantile) and the permutation.error (median error
    #' over all repetitions).
    #'
    #' The distribution of the importance is also visualized as a bar in the
    #' plots, the median importance over the repetitions as a point.
    #'
    initialize = function(predictor, loss, compare = "ratio",
                          n.repetitions = 5, features = NULL) {

      assert_choice(compare, c("ratio", "difference"))
      assert_number(n.repetitions)
      self$compare <- compare
      
      
      if (!inherits(loss, "function")) {
        ## Only allow metrics from Metrics package
        allowedLosses <- c(
          "ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae",
          "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape"
        )
        checkmate::assert_choice(loss, allowedLosses)
        private$loss_string <- loss
        loss <- getFromNamespace(loss, "Metrics")
      } else {
        private$loss_string <- head(loss)
      }
      if (is.null(predictor$data$y)) {
        stop("Please call Predictor$new() with the y target vector.")
      }
      super$initialize(predictor = predictor)
      self$loss <- private$set_loss(loss)
      private$getData <- private$sampler$get.xy
      self$n.repetitions <- n.repetitions
      actual <- private$sampler$y[[1]]
      predicted <- private$run.prediction(private$sampler$X)[[1]]
      # Assuring that levels are the same
      self$original.error <- loss(actual, predicted)
      if (self$original.error == 0 & self$compare == "ratio") {
        warning("Model error is 0, switching from compare='ratio' to compare='difference'")
        self$compare <- "difference"
      }
      
      # process features argument
        if (is.null(features)) {
          features <- private$sampler$feature.names
        }
        if (!is.list(features)) {
          features <- as.list(features)
          names(features) <- unlist(features)
        }
        assert_subset(unique(unlist(features)), private$sampler$feature.names, empty.ok = FALSE)
        self$features <- features
        
      # suppressing package startup messages
      suppressPackageStartupMessages(private$run(self$predictor$batch.size))
    },

    #' @field loss (`character(1)` | [function])\cr
    #'   The loss function. Either the name of a loss (e.g. `"ce"` for
    #'   classification or `"mse"`) or a function.
    loss = NULL,

    #' @field original.error (`numeric(1)`)\cr
    #' The loss of the model before perturbing features.
    original.error = NULL,

    #' @field n.repetitions [integer]\cr
    #'   Number of repetitions.
    n.repetitions = NULL,

    #' @field compare (`character(1)`)\cr Either `"ratio"` or `"difference"`,
    #'   depending on whether the importance was calculated as difference
    #'   between original model error and model error after permutation or as
    #'   ratio.
    compare = NULL,
    
    #' @field features (`list`)\cr Features for which importance scores are to
    #' be calculated. The names are the feature/group names, while the contents
    #' specify which feature(s) are to be permuted.
    features = NULL
    
  ),

  private = list(
    # for printing
    loss_string = NULL,
    q = function(pred) probs.to.labels(pred),
    combine.aggregations = function(agg, dat) {
      if (is.null(agg)) {
        return(dat)
      }
    },
    run = function(n) {

      private$dataSample <- private$getData()
      result <- NULL

      estimate_feature_imp <- function(group,
                                       features,  
                                       data.sample,
                                       y,
                                       n.repetitions,
                                       y.names,
                                       pred,
                                       loss) {
        
        cnames <- setdiff(colnames(data.sample), y.names)
        qResults <- data.table::data.table()
        y.vec <- data.table::data.table()
        for (repi in 1:n.repetitions) {
          mg <- MarginalGenerator$new(data.sample, data.sample,
            features = features, n.sample.dist = 1, y = y, cartesian = FALSE,
            id.dist = TRUE
          )
          while (!mg$finished) {
            data.design <- mg$next.batch(n, y = TRUE)
            y.vec <- rbind(y.vec, data.design[, y.names, with = FALSE])
            qResults <- rbind(
              qResults,
              pred(data.design[, cnames, with = FALSE])
            )
          }
        }
        # AGGREGATE measurements
        results <- data.table::data.table(
          feature = group, actual = y.vec[[1]], predicted = qResults[[1]],
          num_rep = rep(1:n.repetitions, each = nrow(data.sample))
        )
        results <- results[, list("permutation_error" = loss(actual, predicted)),
          by = list(feature, num_rep)
        ]
        results
      }

      n.repetitions <- self$n.repetitions
      data.sample <- private$dataSample
      y <- private$sampler$y
      y.names <- private$sampler$y.names
      pred <- private$run.prediction
      loss <- self$loss

      result <- rbindlist(unname(
        future.apply::future_mapply(estimate_feature_imp,
          group = names(self$features),
          features = self$features,
          MoreArgs = list(
            data.sample = data.sample,
            y = y,
            n.repetitions = n.repetitions,
            y.names = y.names,
            pred = pred,
            loss = loss
          ),
          SIMPLIFY = FALSE,
          future.seed = TRUE,
          future.globals = FALSE,
          future.packages = loadedNamespaces()
        )
      ), use.names = TRUE)

      if (self$compare == "ratio") {
        result[, importance_raw := permutation_error / self$original.error]
      } else {
        result[, importance_raw := permutation_error - self$original.error]
      }
      result <- result[, list(
        "importance" = median(importance_raw),
        "permutation.error" = median(permutation_error),
        "importance.05" = quantile(importance_raw, probs = 0.05),
        "importance.95" = quantile(importance_raw, probs = 0.95)
      ), by = list(feature)]
      result <- result[order(result$importance, decreasing = TRUE), ]
      # Removes the n column
      result <- result[, list(
        feature, importance.05, importance, importance.95,
        permutation.error
      )]
      private$finished <- TRUE
      self$results <- data.frame(result)
    },
    generatePlot = function(sort = TRUE, ...) {
      requireNamespace("ggplot2", quietly = TRUE)
      res <- self$results
      if (sort) {
        res$feature <- factor(res$feature,
          levels = res$feature[order(res$importance)]
        )
      }
      xstart <- ifelse(self$compare == "ratio", 1, 0)
      ggplot(res, aes(y = feature, x = importance)) +
        geom_segment(aes(y = feature, yend = feature, x = importance.05, xend = importance.95), size = 1.5, color = "darkslategrey") +
        geom_point(size = 3) +
        scale_x_continuous(sprintf("Feature Importance (loss: %s)", private$loss_string)) +
        scale_y_discrete("")
    },
    set_loss = function(loss) {
      self$loss <- loss
    },
    printParameters = function() {
      cat("error function:", private$loss_string)
    }
  )
)

#' @title Plot Feature Importance
#' @description
#' `plot.FeatureImp()` plots the feature importance results of a FeatureImp
#' object.
#'
#' @param x A [FeatureImp] object
#' @param sort logical. Should the features be sorted in descending order?
#'   Defaults to TRUE.
#' @param ... Further arguments for the objects plot function
#' @return ggplot2 plot object
#' @export
#'
#' @details
#' The plot shows the importance per feature.
#'
#' When `n.repetitions` in `FeatureImp$new` was larger than 1, then we get
#' multiple importance estimates per feature. The importance are aggregated and
#' the plot shows the median importance per feature (as dots) and also the
#' 90%-quantile, which helps to understand how much variance the computation has
#' per feature.
#'
#' @seealso [FeatureImp]
#' @examples
#' library("rpart")
#' # We train a tree on the Boston dataset:
#' data("Boston", package = "MASS")
#' tree <- rpart(medv ~ ., data = Boston)
#' y <- Boston$medv
#' X <- Boston[-which(names(Boston) == "medv")]
#' mod <- Predictor$new(tree, data = X, y = y)
#'
#' # Compute feature importances as the performance drop in mean absolute error
#' imp <- FeatureImp$new(mod, loss = "mae")
#'
#' # Plot the results directly
#' plot(imp)
plot.FeatureImp <- function(x, sort = TRUE, ...) {
  x$plot(sort = sort, ...)
}
