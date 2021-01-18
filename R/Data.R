#' @importFrom data.table data.table
Data <- R6::R6Class("Data",
  public = list(
    X = NULL,
    y = NULL,
    y.names = NULL,
    feature.types = NULL,
    feature.names = NULL,
    n.features = NULL,
    n.rows = NULL,
    prob = NULL,
    # Removes additional columns, stops when some are missing
    match_cols = function(newdata) {
      colnames_new <- colnames(newdata)
      missing_columns <- setdiff(self$feature.names, colnames_new)
      if (length(missing_columns) > 0) {
        stop(sprintf("Missing columns: %s", paste(missing_columns, collapse = ", ")))
      }
      additional_columns <- setdiff(colnames_new, self$feature.names)
      if (length(additional_columns) > 0) {
        warning(sprintf("Dropping additional columns: %s", paste(additional_columns, collapse = ", ")))
      }
      newdata[self$feature.names]
    },
    sample = function(n = 100, replace = TRUE, prob = NULL, get.y = FALSE) {
      if (is.null(prob) & !is.null(self$prob)) {
        prob <- self$prob
      }
      indices <- sample.int(self$n.rows,
        size = n,
        replace = replace, prob = prob
      )
      if (get.y) {
        cbind(self$X[indices, ], self$y[indices, ])
      } else {
        self$X[indices, ]
      }
    },
    get.x = function(...) {
      self$X
    },
    get.xy = function(...) {
      cbind(self$X, self$y)
    },
    print = function() {
      cat("Sampling from data.frame with", nrow(self$X), "rows and", ncol(self$X), "columns.\n")
    },
    initialize = function(X, y = NULL, prob = NULL) {

      assertDataFrame(X, all.missing = FALSE)
      assertNamed(X)
      if (length(y) == 1 & is.character(y)) {
        assert_true(y %in% names(X))
        self$y <- X[, y, drop = FALSE]
        self$y.names <- y
      } else if (inherits(y, "data.frame")) {
        assertDataFrame(y, all.missing = FALSE, null.ok = TRUE, nrows = nrow(X))
        self$y <- y
        self$y.names <- colnames(self$y)
        if (length(intersect(colnames(self$y), colnames(X))) != 0) {
          stop("colnames of y and X have to be different.")
        }
      } else if (is.vector(y) | is.factor(y)) {
        assert_vector(y, any.missing = FALSE, null.ok = TRUE, len = nrow(X))
        self$y <- data.frame(.y = y)
        self$y.names <- colnames(self$y)
      }
      self$X <- data.table(X[, setdiff(colnames(X), self$y.names), drop = FALSE])
      if (ncol(self$X) == 1) stop("Only 1 feature was provided. The iml package is only useful and works for multiple features.")
      self$prob <- prob
      self$feature.types <- get.feature.type(unlist(lapply(self$X, function(x){class(x)[1]})))
      self$feature.names <- colnames(self$X)
      self$n.features <- ncol(self$X)
      self$n.rows <- nrow(self$X)
      names(self$feature.types) <- self$feature.names
    }
  )
)
