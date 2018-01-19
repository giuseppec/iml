#' Permutation feature importance
#' 
#' 
#' @export
#' @template args_experiment_wrap
perm.imp = function(object, X, y, class=NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  PermImps$new(predictor = pred, sampler = samp, y=y)$run()
}


PermImps = R6Class('PermImps', 
  inherit = RepeatedExperiment,
  public = list(
    initialize = function(predictor, sampler, y){
      experiments = lapply(1:sampler$n.features, function(feature.index){
        PermImp$new(predictor = predictor, sampler = sampler, y = y, feature.index = feature.index)
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
PermImp = R6Class('PermImp', 
  inherit = Experiment,
  public = list(
    y = NULL,
    feature.index = NULL,
    initialize = function(predictor, sampler, feature.index, y){
      ## TODO: Add check that nrow(X) the same as length(y) or nrow(y)
      super$initialize(predictor = predictor, sampler = sampler)
      self$y = y
      self$feature.index = feature.index
      private$sample.x = private$sampler$get.x
    }
  ),
  private = list(
    intervene = function(){
      X.inter = private$X.sample
      X.inter[self$feature.index] = X.inter[sample(1:nrow(private$X.sample)), self$feature.index]
      rbind(X, X.inter)
    },
    aggregate = function(){
      stopifnot(nrow(private$Q.results)/2 == length(self$y))
      performance = function(y.hat, y){
        mean(log(y.hat + 0.00001)*y + log((1-y.hat) + 0.00001) * (1-y))
      }
      length.y = length(y)
      pp = performance(private$Q.results[1:length.y,1], self$y) - 
        performance(private$Q.results[(length.y + 1):(2*length.y),1], self$y)
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







