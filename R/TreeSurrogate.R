
tree.surrogate = function(object, X, sample.size=100, class = NULL, multi.class = FALSE, ...){
  TreeSurrogate$new(object = object, X=X, sample.size=sample.size, class = class, multi.class = multi.class, ...)$run()
}

## Craven, M. W., & Shavlik, J. W. (1996).
## Extracting tree-structured representations of trained neural networks.
## Advances in Neural Information Processing Systems, 8, 24–30.
## Retrieved from citeseer.ist.psu.edu/craven96extracting.html
# TODO: Implement/search plot funciton with ggplot or overwrite Experiment$plot()
# TODO: Implement multi.class 
TreeSurrogate = R6Class('TreeSurrogate',
  inherit = Experiment,
  public = list(
    intervene = function(){self$X.sample},
    aggregate = function(){
      dat = cbind(y.hat = private$Q.results, self$X.design)
      rpart::rpart(y.hat ~ ., data = dat)
    },
    plot = function(){
      self$run()
      plot(private$results)
    },
    summary = function(){
      self$run()
      summary(private$results)
    },
    initialize = function(object, X, sample.size, class, multi.class, ...){
      if(multi.class){ stop('multi.class not yet supported for surrogate models')}
      super$initialize(object, X, class, multi.class, ...)
      self$sample.size = sample.size
    }
  )
)
