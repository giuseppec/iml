#' Permutation feature importance
#' 
#' 
#' @export
#' @template args_experiment_wrap
perm.imp = function(object, X, y, class=NULL, ...){
  samp = DataSampler$new(X, y=data.frame(y = y))
  pred = prediction.model(object, class = class, ...)
  
  PermImps$new(predictor = pred, sampler = samp)$run()
}


PermImps = R6::R6Class('PermImps', 
  inherit = RepeatedExperiment,
  public = list(
    initialize = function(predictor, sampler){
      experiments = lapply(1:sampler$n.features, function(feature.index){
        PermImp$new(predictor = predictor, sampler = sampler, feature.index = feature.index)
      })
      super$initialize(predictor, sampler, experiments)
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
    initialize = function(predictor, sampler, feature.index){
      ## TODO: Add check that nrow(X) the same as length(y) or nrow(y)
      super$initialize(predictor = predictor, sampler = sampler)
      self$feature.index = feature.index
      private$sample.x = private$sampler$get.xy
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
      performance = function(y.hat, y){
        mean(log(y.hat + 0.00001)*y + log((1-y.hat) + 0.00001) * (1-y))
      }
      length.y = length(y)
      pp = performance(private$Q.results[1:length.y,1], y) - 
        performance(private$Q.results[(length.y + 1):(2*length.y),1], y)
      data.frame(performance = pp, feature = private$sampler$feature.names[self$feature.index])
    },
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







