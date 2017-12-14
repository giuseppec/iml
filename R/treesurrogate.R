## Craven, M. W., & Shavlik, J. W. (1996). 
## Extracting tree-structured representations of trained neural networks. 
## Advances in Neural Information Processing Systems, 8, 24–30. 
## Retrieved from citeseer.ist.psu.edu/craven96extracting.html
TreeSurrogate = R6Class('TreeSurrogate', 
  inherit = Experiment,
  public = list(
    intervene = function(){self$X.sample},
    plot = function(){
      plot(private$results)
    }, 
    aggregate = function(){
      dat = cbind(y.hat = self$Q.results, self$X.design)
      print(head(dat))
      partykit::ctree(y.hat ~ ., data = dat)
    },
    initialize = function(f, X, sample.size=100){
      self$sample.size = sample.size
      self$f = f
      self$X = X
    }
  )
)









