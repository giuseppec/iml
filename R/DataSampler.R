DataSampler  = R6::R6Class('DataSampler',
  public = list(
    X = NULL,
    y = NULL,
    y.names = NULL,
    feature.types = NULL,
    feature.names = NULL,
    n.features = NULL,
    prob = NULL,
    sample = function(n, replace = TRUE, prob = NULL, get.y=FALSE) {
      if(is.null(prob) & !is.null(self$prob)) {
        prob = self$prob
      }
      indices = sample.int(private$nrows, size = n, 
        replace = replace, prob = prob)
      if(get.y){
        cbind(self$X[indices,], self$y[indices,])
      } else {
        self$X[indices, ]
      }
    },
    get.x = function(...){
      self$X
    },
    get.xy = function(...){
      cbind(self$X, self$y)
    },
    print = function(){
      cat("Sampling from data.frame with", nrow(self$X), "rows and", ncol(self$X), "columns.")
    },
    initialize = function(X, y = NULL, prob = NULL){
      assertDataFrame(X, all.missing = FALSE)
      assert_named(X)
      assertDataFrame(y, all.missing = FALSE, null.ok = TRUE)
      self$X = X
      if(!missing(y)){
        self$y = y
        self$y.names = names(y)
        assert_true(nrow(X)==nrow(y))
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
