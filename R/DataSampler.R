DataSampler  = R6::R6Class('DataSampler',
  public = list(
    X = NULL,
    feature.types = NULL,
    feature.names = NULL,
    n.features = NULL,
    prob = NULL,
    sample = function(n, replace = TRUE, prob = NULL) {
      if(is.null(prob) & !is.null(self$prob)) {
        prob = self$prob
      }
      indices = sample.int(private$nrows, size = n, 
        replace = replace, prob = prob)
      self$X[indices, ]
    },
    get.x = function(...){
      self$X
    },
    print = function(){
      cat("Sampling from data.frame with", nrow(self$X), "rows and", ncol(self$X), "columns.")
    },
    initialize = function(X, prob = NULL){
      assertDataFrame(X, all.missing = FALSE)
      self$X = X
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
