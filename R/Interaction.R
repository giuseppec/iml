# Interaction ------------------------------------------------------------------

#' Feature interactions
#'
#' `Interaction` estimates the feature interactions in a prediction model.
#'
#' @importFrom data.table dcast
#' @template parallel
#' @details
#' Interactions between features are measured via the decomposition of the
#' prediction function: If a feature `j` has no interaction with any other
#' feature, the prediction function can be expressed as the sum of the partial
#' function that depends only on `j` and the partial function that only depends
#' on features other than `j`. If the variance of the full function is
#' completely explained by the sum of the partial functions, there is no
#' interaction between feature `j` and the other features. Any variance that is
#' not explained can be attributed to the interaction and is used as a measure
#' of interaction strength.
#'
#' The interaction strength between two features is the proportion of the
#' variance of the 2-dimensional partial dependence function that is not
#' explained by the sum of the two 1-dimensional partial dependence functions.
#'
#' The interaction is measured by Friedman's H-statistic (square root of the
#' H-squared test statistic) and takes on values between 0 (no interaction) to 1
#' (100% of standard deviation of f(x) du to interaction).
#'
#' To learn more about interaction effects, read the Interpretable Machine Learning book:
#' \url{https://christophm.github.io/interpretable-ml-book/interaction.html}
#'
#' @references Friedman, Jerome H., and Bogdan E. Popescu. "Predictive learning
#' via rule ensembles." The Annals of Applied Statistics 2.3 (2008): 916-954.
#'
#' @examples
#' \dontrun{
#' library("rpart")
#' set.seed(42)
#' # Fit a CART on the Boston housing data set
#' data("Boston", package = "MASS")
#' rf <- rpart(medv ~ ., data = Boston)
#' # Create a model object
#' mod <- Predictor$new(rf, data = Boston[-which(names(Boston) == "medv")])
#'
#' # Measure the interaction strength
#' ia <- Interaction$new(mod)
#'
#' # Plot the resulting leaf nodes
#' plot(ia)
#'
#' # Extract the results
#' dat <- ia$results
#' head(dat)
#'
#' # Interaction also works with multiclass classification
#' rf <- rpart(Species ~ ., data = iris)
#' mod <- Predictor$new(rf, data = iris, type = "prob")
#'
#' # For some models we have to specify additional arguments for the
#' # predict function
#' ia <- Interaction$new(mod)
#'
#' ia$plot()
#'
#' # For multiclass classification models, you can choose to only show one class:
#' mod <- Predictor$new(rf, data = iris, type = "prob", class = "virginica")
#' plot(Interaction$new(mod))
#' }
#'
#' @name Interaction
#' @export
Interaction <- R6Class("Interaction",
  inherit = InterpretationMethod,

  public = list(

    #' @description Create an Interaction object
    #' @template predictor
    #' @template feature
    #' @template grid.size
    #' @return
    #' [data.frame] with the interaction strength (column `.interation`) per
    #' feature calculated as Friedman's H-statistic and - in the case of a
    #' multi-dimensional outcome - per class.
    initialize = function(predictor, feature = NULL, grid.size = 30) {
      assert_vector(feature, len = 1, null.ok = TRUE)
      assert_number(grid.size, lower = 2)
      if (!is.null(feature) && is.numeric(feature)) {
        private$feature <- predictor$data$feature.names[feature]
      } else {
        private$feature <- feature
      }
      self$grid.size <- min(grid.size, predictor$data$n.rows)
      super$initialize(predictor)
      private$run(predictor$batch.size)
    },

    # The fitted tree
    #' @field grid.size (`logical(1)`)\cr
    #' The number of values per feature that should be used to estimate the
    #' interaction strength.
    grid.size = NULL
  ),

  private = list(
    run = function(batch.size) {
      features <- setdiff(private$sampler$feature.names, private$feature)
      data.sample <- private$sampler$get.x()
      probe <- self$predictor$predict(data.frame(data.sample[1, ]))
      private$multiClass <- ifelse(ncol(probe) > 1, TRUE, FALSE)

      self$results <- rbindlist(unname(future.apply::future_lapply(features, function(x) {
        private$interaction.run.single(
          dataSample = data.sample,
          feature.name = c(x, private$feature),
          grid.size = self$grid.size,
          batch.size = batch.size,
          q = private$q,
          predictor = self$predictor
        )
      },
      future.seed = TRUE,
      future.packages = loadedNamespaces()
      )), use.names = TRUE)

      private$finished <- TRUE
    },
    generatePlot = function(sort = TRUE, ...) {
      requireNamespace("ggplot2", quietly = TRUE)
      res <- self$results
      if (sort & !private$multiClass) {
        res$.feature <- factor(res$.feature,
          levels = res$.feature[order(res$.interaction)]
        )
      }

      y.axis.label <- ifelse(is.null(private$feature), "Interaction strength",
        sprintf("Interaction strength with %s", private$feature)
      )
      p <- ggplot(res, aes(y = .feature, x = .interaction)) +
        geom_point() +
        geom_segment(aes(yend = .feature, x = 0, xend = .interaction)) +
        scale_x_continuous("Overall interaction strength") +
        scale_y_discrete("Features")
      if (private$multiClass) {
        p <- p + facet_wrap(".class")
      }
      p
    },

    interaction.run.single = function(batch.size, dataSample, feature.name,
                                      grid.size, predictor, q) {

      assert_data_table(dataSample)
      assert_character(feature.name,
        min.len = 1, max.len = 2,
        any.missing = FALSE
      )
      assert_number(grid.size)

      grid.dat <- dataSample[sample(1:nrow(dataSample), size = grid.size), ]
      dist.dat <- dataSample

      res.intermediate <- data.table()
      if (length(feature.name) == 1) {
        mg_j <- MarginalGenerator$new(grid.dat, dist.dat, feature.name, cartesian = TRUE)
        batch.size.split <- floor(batch.size) / 2
        mg_noj <- MarginalGenerator$new(grid.dat, dist.dat,
          setdiff(colnames(dataSample), feature.name),
          cartesian = TRUE
        )

        while (!mg_j$finished) {
          partial_j <- mg_j$next.batch(batch.size.split)
          partial_j$.type <- "j"
          partial_noj <- mg_noj$next.batch(batch.size.split)
          partial_noj$.type <- "no.j"
          grid.dat$.type <- "f"
          grid.dat$.id <- 1:nrow(grid.dat)
          res.intermediate <- rbind(res.intermediate, partial_j, partial_noj,
            grid.dat,
            use.names = TRUE
          )
        }
      } else if (length(feature.name) == 2) {
        batch.size.split <- floor(batch.size / 3)
        mg_jk <- MarginalGenerator$new(grid.dat, dist.dat, feature.name,
          cartesian = TRUE
        )
        mg_j <- MarginalGenerator$new(grid.dat, dist.dat, feature.name[1],
          cartesian = TRUE
        )
        mg_k <- MarginalGenerator$new(grid.dat, dist.dat, feature.name[2],
          cartesian = TRUE
        )

        while (!mg_j$finished) {
          partial_jk <- mg_jk$next.batch(batch.size.split)
          partial_jk$.type <- "jk"
          partial_j <- mg_j$next.batch(batch.size.split)
          partial_j$.type <- "j"
          partial_k <- mg_k$next.batch(batch.size.split)
          partial_k$.type <- "k"
          res.intermediate <- rbind(res.intermediate, partial_jk, partial_j,
            partial_k,
            use.names = TRUE
          )
        }
      }
      predictResults <- predictor$predict(data.frame(res.intermediate))
      qResults <- q(predictResults)
      res.intermediate$.feature <- paste(feature.name, collapse = ":")
      aggregate.interaction(res.intermediate, qResults, feature.name)
    },
    feature = NULL
  )
)

# plot.Interaction -------------------------------------------------------------

#' Plot Interaction
#'
#' `plot.Interaction()` plots the results of an Interaction object.
#'
#' @param x An Interaction R6 object
#' @param sort logical. Should the features be sorted in descending order?
#'   Defaults to TRUE.
#' @return ggplot2 plot object
#' @seealso [Interaction]
#' @examples
#' # We train a tree on the Boston dataset:
#' \dontrun{
#' library("rpart")
#' data("Boston", package = "MASS")
#' rf <- rpart(medv ~ ., data = Boston)
#' mod <- Predictor$new(rf, data = Boston)
#'
#' # Compute the interactions
#' ia <- Interaction$new(mod)
#'
#' # Plot the results directly
#' plot(ia)
#' }
plot.Interaction <- function(x, sort = TRUE) {
  x$plot(sort = sort)
}

# helper functions -------------------------------------------------------------

# The test statistic as defined in:
# Friedman, Jerome H., and Bogdan E. Popescu. "Predictive learning via rule
# ensembles." The Annals of Applied Statistics 2.3 (2008): 916-954.
# Measures the variance explained by the interaction
h.test <- function(f.all, f.j, f.no.j) {
  assert_numeric(f.all, any.missing = FALSE)
  assert_numeric(f.j, any.missing = FALSE)
  assert_numeric(f.no.j, any.missing = FALSE)
  # center
  f.all <- my.scale(f.all)
  f.j <- my.scale(f.j)
  f.no.j <- my.scale(f.no.j)
  # statistics
  sqrt(sum((f.all - (f.j + f.no.j))^2) / sum(f.all^2))
}

my.scale <- function(x) {
  x.scaled <- scale(x, center = TRUE, scale = FALSE)
  x.scaled[is.na(x.scaled)] <- 0
  x.scaled
}

aggregate.interaction <- function(partial_dat, prediction, feature) {

  assert_data_table(partial_dat)
  assert_data_frame(prediction)
  assert_character(feature, null.ok = TRUE)
  assert_true(all(feature %in% colnames(partial_dat)))
  # for suppressing NOTE in R CMD check:
  jk <- j <- k <- f <- no.j <- .feature <- .id <- .class <- .type <- NULL

  if (ncol(prediction) == 1) {
    partial_dat$.y.hat <- prediction
    partial_dat$.class <- 1
  } else {
    y.hat.names <- colnames(prediction)
    partial_dat <- cbind(partial_dat, prediction)
    partial_dat <- data.table::melt(partial_dat,
      variable.name = ".class",
      value.name = ".y.hat", measure.vars = y.hat.names
    )
  }
  partial_dat <- partial_dat[, c(".id", ".feature", ".type", ".y.hat", ".class")]
  pd <- data.table::dcast(partial_dat, .feature + .id + .class ~ .type,
    value.var = ".y.hat", fun.aggregate = mean
  )
  if (length(feature) == 2) {
    res <- data.frame(pd[, list(.interaction = h.test(jk, j, k)), by = list(.feature, .class)])
  } else {
    res <- data.frame(pd[, list(.interaction = h.test(f, j, no.j)), by = list(.feature, .class)])
  }
  if (ncol(prediction) == 1) {
    res$.class <- NULL
  }
  res
}
