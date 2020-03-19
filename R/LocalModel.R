#' @title LocalModel
#'
#' @description
#' `LocalModel` fits locally weighted linear regression models (logistic
#' regression for classification) to explain single predictions of a prediction
#' model.
#'
#' @details
#' A weighted glm is fitted with the machine learning model prediction as
#' target. Data points are weighted by their proximity to the instance to be
#' explained, using the gower proximity measure. L1-regularization is used to
#' make the results sparse.
#'
#' The resulting model can be seen as a surrogate for the machine learning
#' model, which is only valid for that one point. Categorical features are
#' binarized, depending on the category of the instance to be explained: 1 if
#' the category is the same, 0 otherwise.
#'
#' To learn more about local models, read the Interpretable Machine Learning
#' book: \url{https://christophm.github.io/interpretable-ml-book/lime.html}
#'
#' The approach is similar to LIME, but has the following differences:
#' - **Distance measure**: Uses as default the gower proximity (= 1 - gower
#' distance) instead of a kernel based on the Euclidean distance. Has the
#' advantage to have a meaningful neighborhood and no kernel width to tune.
#' When the distance is not `"gower"`, then the [stats::dist()] function with the
#' chosen method will be used, and turned into a similarity measure:
#' \eqn{sqrt(exp(-(distance^2) / (kernel.width^2)))}.
#' - **Sampling**: Uses the original data instead of sampling from normal
#' distributions. Has the advantage to follow the original data distribution.
#' - **Visualization**: Plots effects instead of betas. Both are the same for binary
#' features, but ared different for numerical features. For numerical features,
#' plotting the betas makes no sense, because a negative beta might still
#' increase the prediction when the feature value is also negative.
#'
#' To learn more about local surrogate models, read the Interpretable Machine
#' Learning book:
#' \url{https://christophm.github.io/interpretable-ml-book/lime.html}
#'
#' @references
#' Ribeiro, M. T., Singh, S., & Guestrin, C. (2016). "Why Should I Trust You?":
#' Explaining the Predictions of Any Classifier. Retrieved from
#' http://arxiv.org/abs/1602.04938
#'
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its
#' properties". Biometrics, 27, 623--637.
#'
#' @seealso
#' \code{\link{plot.LocalModel}} and \code{\link{predict.LocalModel}}
#'
#' \code{\link{Shapley}} can also be used to explain single predictions
#'
#' The package `lime` with the original implementation
#' @export
#' @importFrom stats dist
#' @examples
#' library("randomForest")
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package = "MASS")
#' X <- Boston[-which(names(Boston) == "medv")]
#' rf <- randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod <- Predictor$new(rf, data = X)
#'
#' # Explain the first instance of the dataset with the LocalModel method:
#' x.interest <- X[1, ]
#' lemon <- LocalModel$new(mod, x.interest = x.interest, k = 2)
#' lemon
#'
#' # Look at the results in a table
#' lemon$results
#' # Or as a plot
#' plot(lemon)
#'
#' # Reuse the object with a new instance to explain
#' lemon$x.interest
#' lemon$explain(X[2, ])
#' lemon$x.interest
#' plot(lemon)
#'
#' # LocalModel also works with multiclass classification
#' rf <- randomForest(Species ~ ., data = iris, ntree = 50)
#' X <- iris[-which(names(iris) == "Species")]
#' mod <- Predictor$new(rf, data = X, type = "prob", class = "setosa")
#'
#' # Then we explain the first instance of the dataset with the LocalModel method:
#' lemon <- LocalModel$new(mod, x.interest = X[1, ], k = 2)
#' lemon$results
#' plot(lemon)
#' @export
LocalModel <- R6Class("LocalModel",
  inherit = InterpretationMethod,

  public = list(

    #' @description Create a Local Model object.
    #' @template predictor
    #' @param x.interest [data.frame]\cr
    #'   Single row with the instance to be explained.
    #' @param dist.fun `character(1)`)\cr
    #'   The name of the distance function for computing proximities (weights in
    #'   the linear model). Defaults to `"gower"`. Otherwise will be forwarded
    #'   to [stats::dist].
    #' @param kernel.width (`numeric(1)`)\cr
    #'   The width of the kernel for the proximity computation.
    #'   Only used if dist.fun is not `"gower"`.
    #' @param k `numeric(1)`\cr
    #'   The number of features.
    #' @return [data.frame]\cr
    #'   Results with the feature names (`feature`) and contributions to the
    #'   prediction.
    initialize = function(predictor, x.interest, dist.fun = "gower",
                          kernel.width = NULL, k = 3) {

      assert_number(k, lower = 1, upper = predictor$data$n.features)
      assert_data_frame(x.interest, null.ok = TRUE)
      assert_choice(dist.fun, c(
        "gower", "euclidean", "maximum",
        "manhattan", "canberra", "binary", "minkowski"
      ))
      if (!require("glmnet")) {
        stop("Please install glmnet.")
      }
      super$initialize(predictor = predictor)
      self$k <- k
      if (!is.null(x.interest)) {
        self$x.interest <- private$match_cols(x.interest)
      }
      private$weight.fun <- private$get.weight.fun(dist.fun, kernel.width)

      if (!is.null(x.interest)) private$run()
    },

    #' @description Method to predict new data with the local model See also
    #' [predict.LocalModel].
    #' @template newdata
    #' @param ... Not used
    predict = function(newdata = NULL, ...) {
      if (is.null(newdata)) {
        newdata <- self$x.interest
      } else {
        newdata <- private$match_cols(newdata)
      }
      X.recode <- recode(newdata, self$x.interest)

      if (private$multiClass) {
        prediction <- predict(self$model,
          newx = as.matrix(X.recode),
          type = "response"
        )
        prediction <- data.frame(prediction[, , self$best.fit.index])
        colnames(prediction) <- colnames(private$predictResults)
        prediction
      } else {
        prediction <- predict(self$model, newx = as.matrix(X.recode))
        pred <- prediction[, self$best.fit.index, drop = FALSE]
        colnames(pred) <- NULL
        data.frame(prediction = pred)
      }
    },

    #' @description Set a new data point to explain.
    #' @param x.interest [data.frame]\cr
    #'   Single row with the instance to be explained.
    explain = function(x.interest) {
      self$x.interest <- private$match_cols(x.interest)
      private$flush()
      private$run()
    },

    #' @field x.interest [data.frame]\cr
    #'   Single row with the instance to be explained.
    x.interest = NULL,

    #' @field k `numeric(1)`\cr
    #'   The number of features as set by the user.
    k = NULL,

    #' @field model `glmnet`\cr
    #'  The fitted local model.
    model = NULL,

    #' @field best.fit.index `numeric(1)`\cr
    #'   The index of the best glmnet fit.
    best.fit.index = NULL
  ),

  private = list(
    q = function(pred) probs.to.labels(pred),
    best.index = NULL,
    match_cols = function(newdata) {
      self$predictor$data$match_cols(data.frame(newdata))
    },
    aggregate = function() {

      X.recode <- recode(private$dataDesign, self$x.interest)
      x.recoded <- recode(self$x.interest, self$x.interest)
      fam <- ifelse(private$multiClass, "multinomial", "gaussian")
      y <- unlist(private$qResults[1])
      w <- private$weight.fun(X.recode, x.recoded)
      self$model <- glmnet(
        x = as.matrix(X.recode), y = y, family = fam, w = w, intercept = TRUE,
        standardize = TRUE, type.multinomial = "grouped"
      )
      res <- self$model
      ## It can happen, that no n.vars matching k occurs
      if (any(res$df == self$k)) {
        best.index <- max(which(res$df == self$k))
      } else {
        best.index <- max(which(res$df < self$k))
        warning("Had to choose a smaller k")
      }
      self$best.fit.index <- best.index
      if (private$multiClass) {
        class.results <- lapply(res$beta, extract.glmnet.effects,
          best.index = best.index, x.recoded = x.recoded,
          x.original = self$x.interest
        )
        res <- data.table::rbindlist(class.results)
        res$.class <- rep(names(class.results), each = ncol(X.recode))
      } else {
        res <- extract.glmnet.effects(
          res$beta, best.index, x.recoded,
          self$x.interest
        )
      }
      res[res$beta != 0, ]
    },
    intervene = function() private$dataSample,
    generatePlot = function() {
      requireNamespace("ggplot2", quietly = TRUE)
      p <- ggplot(self$results) +
        geom_col(aes(y = effect, x = reorder(feature.value, effect))) +
        coord_flip() +
        ylab("effect") +
        xlab("")
      if (!private$multiClass) {
        original_prediction <- self$predictor$predict(self$x.interest)[[1]]
        p <- p + ggtitle(sprintf(
          "Actual prediction: %.2f\nLocalModel prediction: %.2f",
          original_prediction, self$predict()
        ))
      }
      if (private$multiClass) p <- p + facet_wrap(".class")
      p
    },
    get.weight.fun = function(dist.fun, kernel.width) {
      if (dist.fun == "gower") {
        require("gower")
        function(X, x.interest) {
          1 - gower_dist(X, x.interest)
        }
      } else if (is.character(dist.fun)) {
        assert_numeric(kernel.width)
        function(X, x.interest) {
          d <- dist(rbind(x.interest, X), method = dist.fun)[1 + 1:nrow(X)]
          sqrt(exp(-(d^2) / (kernel.width^2)))
        }
      } else {
        dist.fun
      }
    },
    weight.fun = NULL
  )
)

# Predict.LocalModel -----------------------------------------------------------

#' Predict LocalModel
#'
#' Predict the response for newdata with the LocalModel model.
#'
#' @param object A LocalModel R6 object
#' @param newdata A data.frame for which to predict
#' @param ... Further arguments for the objects predict function
#' @return A data.frame with the predicted outcome.
#' @seealso [LocalModel]
#' @importFrom stats predict
#' @export
#' @examples
#' library("randomForest")
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package = "MASS")
#' X <- Boston[-which(names(Boston) == "medv")]
#' rf <- randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod <- Predictor$new(rf, data = X)
#'
#' # Explain the first instance of the dataset with the LocalModel method:
#' x.interest <- X[1, ]
#' lemon <- LocalModel$new(mod, x.interest = x.interest, k = 2)
#' predict(lemon, newdata = x.interest)
predict.LocalModel <- function(object, newdata = NULL, ...) {
  object$predict(newdata = newdata, ...)
}

# Plot.LocalModel --------------------------------------------------------------

#' Plot Local Model
#'
#' `plot.LocalModel()` plots the feature effects of a LocalModel object.
#'
#' @param object  A LocalModel R6 object
#' @return ggplot2 plot object
#' @seealso [LocalModel]
#' @examples
#' library("randomForest")
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package = "MASS")
#' X <- Boston[-which(names(Boston) == "medv")]
#' rf <- randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod <- Predictor$new(rf, data = X)
#'
#' # Explain the first instance of the dataset with the LocalModel method:
#' x.interest <- X[1, ]
#' lemon <- LocalModel$new(mod, x.interest = x.interest, k = 2)
#' plot(lemon)
plot.LocalModel <- function(object) {
  object$plot()
}
