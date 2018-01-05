

perm.imp = function(object, X, y, feature.index, class=NULL, multi.class=FALSE){
  PermImp$new(object = object, X=X, y=y, feature.index=feature.index, class = class, multi.class = FALSE)
}

## TODO: Extend to multiple features. Either within this class or as a new class. 
## TODO: Use different performance function for regression
## TODO: performance function as a parameter in intialize
## TODO: implement random sampling instead of whole x
PermImp = R6Class('PermImp', 
  inherit = Experiment,
  public = list(
    y = NULL,
    feature.index = NULL,
    intervene = function(){
      X.inter = self$X.sample
      X.inter[self$feature.index] = X.inter[sample(1:nrow(self$X.sample)), self$feature.index]
      rbind(X, X.inter)
    },
    aggregate = function(){
      stopifnot(length(private$Q.results)/2 == length(self$y))
      performance = function(y.hat, y){
        mean(log(y.hat + 0.00001)*y + log((1-y.hat) + 0.00001) * (1-y))
      }
      pp = performance(private$Q.results[1:(length(private$Q.results)/2)], self$y) - 
        performance(private$Q.results[(length(private$Q.results)/2 + 1):length(private$Q.results)], self$y)
      data.frame(performance = pp, feature = private$feature.names[self$feature.index])
    },
    initialize = function(object, X, y, feature.index, class, multi.class){
      ## TODO: Add check that nrow(X) the same as length(y) or nrow(y)
      if(multi.class) stop("multi.class not supported yet for permutation feature importance")
      super$initialize(object, X, class, multi.class)
      self$y = y
      self$feature.index = feature.index
      private$sample.x = private$sampler$get.x
    }
  ),
  private = list(
    generate.plot = function(){
      ggplot(private$results) + geom_bar(aes(x = feature, y = performance), stat='identity')
    }
  ), 
  active = list(
    feature = function(feature.index){
      self$feature.index = feature.index
      private$flush()
    }
  )
)









