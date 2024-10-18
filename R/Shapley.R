#' @title Prediction explanations with game theory
#'
#' @description
#' `Shapley` computes feature contributions for single predictions with the
#' Shapley value, an approach from cooperative game theory. The features values
#' of an instance cooperate to achieve the prediction. The Shapley value fairly
#' distributes the difference of the instance's prediction and the datasets
#' average prediction among the features.
#'
#'
#' @details
#' For more details on the algorithm see
#' \url{https://christophm.github.io/interpretable-ml-book/shapley.html}
#'
#' @references
#' Strumbelj, E., Kononenko, I. (2014). Explaining prediction models and
#' individual predictions with feature contributions. Knowledge and Information
#' Systems, 41(3), 647-665. https://doi.org/10.1007/s10115-013-0679-x
#' @seealso [Shapley]
#'
#' @seealso
#' A different way to explain predictions: [LocalModel]
#'
#' @examples
#' library("rpart")
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package = "MASS")
#' rf <- rpart(medv ~ ., data = Boston)
#' X <- Boston[-which(names(Boston) == "medv")]
#' mod <- Predictor$new(rf, data = X)
#'
#' # Then we explain the first instance of the dataset with the Shapley method:
#' x.interest <- X[1, ]
#' shapley <- Shapley$new(mod, x.interest = x.interest)
#' shapley
#'
#' # Look at the results in a table
#' shapley$results
#' # Or as a plot
#' plot(shapley)
#'
#' # Explain another instance
#' shapley$explain(X[2, ])
#' plot(shapley)
#' \dontrun{
#' # Shapley() also works with multiclass classification
#' rf <- rpart(Species ~ ., data = iris)
#' X <- iris[-which(names(iris) == "Species")]
#' mod <- Predictor$new(rf, data = X, type = "prob")
#'
#' # Then we explain the first instance of the dataset with the Shapley() method:
#' shapley <- Shapley$new(mod, x.interest = X[1, ])
#' shapley$results
#' plot(shapley)
#'
#' # You can also focus on one class
#' mod <- Predictor$new(rf, data = X, type = "prob", class = "setosa")
#' shapley <- Shapley$new(mod, x.interest = X[1, ])
#' shapley$results
#' plot(shapley)
#' }
#' @export
Shapley <- R6Class("Shapley",
  inherit = InterpretationMethod,

  public = list(

    #' @description Create a Shapley object
    #' @template predictor
    #' @param x.interest [data.frame]\cr
    #'   Single row with the instance to be explained.
    #' @param sample.size `numeric(1)`\cr
    #'   The number of Monte Carlo samples for estimating the Shapley value.
    #' @return [data.frame]\cr
    #'   [data.frame] with the Shapley values (phi) per feature.
    initialize = function(predictor, x.interest = NULL, sample.size = 100) {
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      x.interest = as.data.frame(x.interest)
      super$initialize(predictor = predictor)
      x.interest <- x.interest[setdiff(colnames(x.interest), predictor$data$y.names)]
      self$sample.size <- sample.size
      if (!is.null(x.interest)) {
        private$set.x.interest(x.interest)
      }
      private$getData <- function(...) {
        private$sampler$sample(n = self$sample.size, ...)
      }
      if (!is.null(x.interest)) private$run()
    },

    #' @description Set a new data point which to explain.
    #' @param x.interest [data.frame]\cr
    #'   Single row with the instance to be explained.
    explain = function(x.interest) {
      private$flush()
      private$set.x.interest(x.interest)
      private$run()
    },

    #' @field x.interest [data.frame]\cr
    #'   Single row with the instance to be explained.
    x.interest = NULL,

    #' @field y.hat.interest [numeric]\cr
    #'   Predicted value for instance of interest.
    y.hat.interest = NULL,

    #' @field y.hat.average `numeric(1)`\cr
    #'   Average predicted value for data `X`.
    y.hat.average = NULL,

    #' @field sample.size `numeric(1)`\cr
    #'   The number of times coalitions/marginals are
    #'   sampled from data X. The higher the more accurate the explanations
    #'   become.
    sample.size = NULL
  ),

  private = list(
    aggregate = function() {

      y.hat.with.k <- private$qResults[1:(nrow(private$qResults) / 2), , drop = FALSE]
      y.hat.without.k <- private$qResults[(nrow(private$qResults) / 2 + 1):nrow(private$qResults), , drop = FALSE]
      y.hat.diff <- y.hat.with.k - y.hat.without.k
      cnames <- colnames(y.hat.diff)
      y.hat.diff <- cbind(
        data.table(feature = rep(colnames(private$dataDesign), times = self$sample.size)),
        y.hat.diff
      )
      y.hat.diff <- data.table::melt(y.hat.diff, variable.name = "class", value.name = "value", measure.vars = cnames)
      y.hat.diff <- y.hat.diff[, list("phi" = mean(value), "phi.var" = var(value)), by = c("feature", "class")]
      if (!private$multiClass) y.hat.diff$class <- NULL
      x.original <- unlist(lapply(self$x.interest[1, ], as.character))
      y.hat.diff$feature.value <- rep(sprintf("%s=%s", colnames(self$x.interest), x.original), times = length(cnames))
      y.hat.diff
    },

    intervene = function() {
      # The intervention
      runs <- lapply(1:self$sample.size, function(m) {
        # randomly order features
        new.feature.order <- sample(1:private$sampler$n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled <- private$dataSample[sample(1:nrow(private$dataSample), 1),
          new.feature.order,
          with = FALSE
        ]
        x.interest.shuffled <- self$x.interest[, new.feature.order]

        featurewise <- lapply(1:private$sampler$n.features, function(k) {
          k.at.index <- which(new.feature.order == k)
          instance.with.k <- x.interest.shuffled
          if (k.at.index < ncol(self$x.interest)) {
            instance.with.k[, (k.at.index + 1):ncol(instance.with.k)] <-
              sample.instance.shuffled[, (k.at.index + 1):ncol(instance.with.k),
                with = FALSE
              ]
          }
          instance.without.k <- instance.with.k
          instance.without.k[, k.at.index] <- sample.instance.shuffled[,
            k.at.index,
            with = FALSE
          ]
          cbind(
            instance.with.k[, private$sampler$feature.names],
            instance.without.k[, private$sampler$feature.names]
          )
        })
        data.table::rbindlist(featurewise)

      })
      runs <- data.table::rbindlist(runs)
      dat.with.k <- data.frame(runs[, 1:(ncol(runs) / 2)])
      dat.without.k <- data.frame(runs[, (ncol(runs) / 2 + 1):ncol(runs)])

      rbind(dat.with.k, dat.without.k)
    },

    set.x.interest = function(x.interest) {
      self$x.interest <- x.interest
      self$y.hat.interest <- self$predictor$predict(x.interest)[1, ]
      self$y.hat.average <- colMeans(self$predictor$predict(private$sampler$get.x()))
    },

    generatePlot = function(sort = TRUE, ...) {
      requireNamespace("ggplot2", quietly = TRUE)
      res <- self$results
      if (sort & !private$multiClass) {
        res$feature.value <- factor(res$feature.value,
          levels = res$feature.value[order(res$phi)]
        )
      }
      p <- ggplot(res) +
        geom_col(aes(y = phi, x = feature.value)) +
        coord_flip() +
        xlab("")
      if (private$multiClass) {
        p <- p + facet_wrap("class")
      } else {
        p <- p + ggtitle(sprintf(
          "Actual prediction: %.2f\nAverage prediction: %.2f",
          self$y.hat.interest, self$y.hat.average
        ))
      }
      p
    },

    printParameters = function() {
      cat(sprintf(
        "Predicted value: %f, Average prediction: %f (diff = %f)",
        self$y.hat.interest, self$y.hat.average,
        self$y.hat.interest - self$y.hat.average
      ))
    }
  )
)

#' @title Plot Shapley
#' @description
#' plot.Shapley() plots the Shapley values - the contributions of feature values
#' to the prediction.
#'
#' @param object A Shapley R6 object
#' @param sort [logical]\cr
#'   Should the feature values be sorted by Shapley value? Ignored for
#'   multi.class output.
#' @return ggplot2 plot object
#' @seealso [Shapley]
#' @examples
#' \dontrun{
#' library("rpart")
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package = "MASS")
#' rf <- rpart(medv ~ ., data = Boston)
#' X <- Boston[-which(names(Boston) == "medv")]
#' mod <- Predictor$new(rf, data = X)
#'
#' # Then we explain the first instance of the dataset with the Shapley method:
#' x.interest <- X[1, ]
#' shapley <- Shapley$new(mod, x.interest = x.interest)
#' plot(shapley)
#' }
plot.Shapley <- function(object, sort = TRUE) {
  object$plot(sort = sort)
}
