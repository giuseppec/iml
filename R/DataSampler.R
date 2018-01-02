DataSampler  = R6Class('DataSampler',
  public = list(
    var.types = NULL,
    w = NULL,
    sample.bootstrap = function(n){
      sample(self$X, size = n, replace = TRUE)
    },
    sample = function(n) {
      sample(self$X, size = n, replace = FALSE)
    },
    sample.lime = function(n){
      ## TODO: Implement
    },
    sample.weighted = function(n, w=NULL){
      if(!w){
        w = self$w
      }
      w = w / sum(w)
      sample(self$X, size = n, prob = w, replace = FALSE)
    },
    get.x = function(){
      self$X
    },
    Xset = function(){

    },
    Xall = function(){

    },
    initialize = function(X, w = NULL){
      self$X = X
      self$w = w
      self$var.types = unlist(lapply(X, class))
      private$means = apply(X, 2, mean)
      private$sds = apply(X, 2, sd)
    }
  ),
  private  = list(
    # TODO: Implement for categorical
   means = NULL,
   sds = NULL
  )
)
