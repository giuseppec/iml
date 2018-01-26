#' Permutation feature importance
#' 
#' 
#' @param loss The loss function to use: l(actual, predicted) -> R
#' 
#' @export
#' @template args_experiment_wrap
perm.imp = function(object, X, y, class=NULL, loss, ...){
  samp = DataSampler$new(X, y=data.frame(y = y))
  pred = prediction.model(object, class = class, ...)
  
  PermImps$new(predictor = pred, sampler = samp, loss=loss)$run()
}


PermImps = R6::R6Class('PermImps', 
  inherit = RepeatedExperiment,
  public = list(
    error.original = NULL,
    initialize = function(predictor, sampler, loss){
      experiments = lapply(1:sampler$n.features, function(feature.index){
        PermImp$new(predictor = predictor, sampler = sampler, feature.index = feature.index, loss = loss)
      })
      super$initialize(predictor, sampler, experiments)
      error.original = experiments[[1]]$error.original
    }, 
    plot = function(){
      plt.data = private$results
      # Order features in descending order for plot
      plt.data.order = order(plt.data$performance)
      plt.data$feature = factor(plt.data$feature, levels = plt.data$feature[plt.data.order])
      ggplot(plt.data) + geom_point(aes(x = performance, y = feature))
    }
  )
)


## TODO: Use different performance function for regression
## TODO: performance function as a parameter in intialize
## TODO: implement random sampling instead of whole x
## TODO: Implement multi.class
PermImp = R6::R6Class('PermImp', 
  inherit = Experiment,
  public = list(
    y = NULL,
    feature.index = NULL,
    loss = NULL,
    error.original = NULL,
    initialize = function(predictor, sampler, feature.index, loss){
      ## TODO: Add check that nrow(X) the same as length(y) or nrow(y)
      super$initialize(predictor = predictor, sampler = sampler)
      self$feature.index = feature.index
      self$loss = private$set.loss(loss)
      private$sample.x = private$sampler$get.xy
      self$error.original = loss(private$predict(private$sampler$X), private$sampler$y)
    }
  ),
  private = list(
    intervene = function(){
      X.inter = private$X.sample
      X.inter[self$feature.index] = X.inter[sample(1:nrow(private$X.sample)), self$feature.index]
      rbind(private$X.sample, X.inter)
    },
    aggregate = function(){
      y = private$X.design[[private$sampler$y.names]]
      stopifnot(nrow(private$Q.results) == length(y))
      length.y = length(y)
      normal.index = 1:(length.y/2)
      shuffled.index = (length.y/2 + 1):length.y
      importance = self$loss(private$Q.results[shuffled.index,1], y[shuffled.index]) /
        self$loss(private$Q.results[normal.index,1], y[normal.index])

      data.frame(importance = importance, feature = private$sampler$feature.names[self$feature.index])
    },
    generate.plot = function(){
      ggplot(private$results) + geom_bar(aes(x = feature, y = loss), stat='identity')
    }, 
    set.loss = function(loss){
      self$loss = loss
    }
  ), 
  active = list(
    feature = function(feature.index){
      self$feature.index = feature.index
      private$flush()
    }
  )
)







