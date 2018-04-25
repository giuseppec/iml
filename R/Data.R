#' @importFrom data.table data.table
Data  = R6::R6Class("Data",
  public = list(
    X = NULL,
    y = NULL,
    y.names = NULL,
    feature.types = NULL,
    feature.names = NULL,
    n.features = NULL,
    prob = NULL,
    sample = function(n=100, replace = TRUE, prob = NULL, get.y=FALSE) {
      if (is.null(prob) & !is.null(self$prob)) {
        prob = self$prob
      }
      indices = sample.int(private$nrows, size = n, 
        replace = replace, prob = prob)
      if (get.y) {
        cbind(self$X[indices,], self$y[indices,])
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
      cat("Sampling from data.frame with", nrow(self$X), "rows and", ncol(self$X), "columns.")
    },
    initialize = function(X, y = NULL, prob = NULL) {
      assertDataFrame(X, all.missing = FALSE)
      assertNamed(X)
      self$X = data.table(X)
      if (length(y) == 1 & is.character(y)) {
        assert_true(y %in% names(X))
        self$y = X[,y, drop = FALSE]
        self$y.names = y
        self$X = self$X[, (self$y.names):= NULL ]
      } else if (inherits(y, "data.frame")) {
        assertDataFrame(y, all.missing = FALSE, null.ok = TRUE, nrows = nrow(X))
        self$y = y
        self$y.names = colnames(self$y)
        if (length(intersect(colnames(self$y), colnames(self$X))) != 0) {
          stop("colnames of y and X have to be different.")
        }
      } else if (is.vector(y) | is.factor(y)) {
        assert_vector(y, any.missing = FALSE, null.ok = TRUE, len = nrow(X))
        self$y = data.frame(.y = y)
        self$y.names = colnames(self$y)
      } 
      self$prob = prob
      self$feature.types = get.feature.type(unlist(lapply(X, class)))
      self$feature.names = colnames(X)
      self$n.features = ncol(X)
      names(self$feature.types) = self$feature.names
      private$nrows = nrow(X)
    }
  ),
  private  = list(
   nrows = NULL
  )
)
