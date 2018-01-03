

DataSampler  = R6Class('DataSampler',
  public = list(
    X = NULL,
    feature.types = NULL,
    feature.names = NULL,
    w = NULL,
    sample = function(n, replace = TRUE, prob = NULL) {
      indices = sample.int(private$nrows, size = n, 
        replace = replace, prob = prob)
      self$X[indices, ]
    },
    get.x = function(...){
      self$X
    },
    initialize = function(X, w = NULL){
      self$X = X
      self$w = w
      self$feature.types = get.feature.type(unlist(lapply(X, class)))
      self$feature.names = colnames(X)
      names(self$feature.types) = self$feature.names
      private$nrows = nrow(X)
    }
  ),
  private  = list(
   nrows = NULL
  )
)
