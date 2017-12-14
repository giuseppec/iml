
## TODO: Extend to multiple features. Either within this class or as a new class. 

PermImp = R6Class('PermImp', 
  inherit = Experiment,
  public = list(
    y = NULL,
    feature.index = NULL,
    sampler = function(){
      self$X
    },
    intervene = function(){
      X.inter = self$X.sample
      X.inter[self$feature.index] = X.inter[sample(1:nrow(self$X.sample)), self$feature.index]
      rbind(X, X.inter)
    },
    plot = function(){
      plot(private$results)
    }, 
    aggregate = function(){
      stopifnot(length(self$Q.results)/2 == length(self$y))
      performance = function(y.hat, y){
        mean(log(y.hat + 0.00001)*y + log((1-y.hat) + 0.00001) * (1-y))
      }
      performance(self$Q.results[1:(length(self$Q.results)/2)], self$y) - 
        performance(self$Q.results[(length(self$Q.results)/2 + 1):length(self$Q.results)], self$y)
    },
    initialize = function(f, X, y, feature.index){
      self$f = f
      self$X = X
      self$y = y
      self$feature.index = feature.index
    }
  )
)









