
tree.surrogate = function(object, X, sample.size=100, class = NULL, multi.class = FALSE, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, multi.class = multi.class, ...)
  
  TreeSurrogate$new(predictor = pred, sampler = samp, sample.size=sample.size)$run()
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
    initialize = function(predictor, sampler, sample.size){
      super$initialize(predictor, sampler)
      self$sample.size = sample.size
      if(self$predictor$multi.class) private$predict = self$predictor$predict.class
    }
  )
)
