#' Local models
#' 
#' @export
#' @template args_experiment_wrap
shapley = function(object, X, x.interest, sample.size=100, class=NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  Shapley$new(predictor = pred, sampler = samp, x.interest=x.interest, sample.size=sample.size)$run()
}

## TODO: instead having an outer loop over features,
##       loop over features within each coalition, like the ApproShapley algorithms.
##       Additionally make sure to not calculate things twice, because  it's always
##       the difference of coalition of features with and without feature j.
##       see Song, E., & Nelson, B. L. (2016). Shapley Effects for Global Sensitivity Analysis : Theory and Computation ∗, 4, 1060–1083.
## TODO: Get some inspiration form the sensitivity package.
## TODO: Implement multi.class
Shapley = R6Class('Shapley', 
  inherit = Experiment,
  public = list(
    x.interest = NULL,
    initialize = function(predictor, sampler, x.interest, sample.size){
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$x.interest = x.interest
    }
  ), 
  private = list(
    
    aggregate = function(){
      agg.df = data.frame(
        y.hat.with.k = private$Q.results[1:(nrow(private$Q.results)/2),1],
        y.hat.without.k = private$Q.results[(nrow(private$Q.results)/2 + 1):nrow(private$Q.results),1],
        features = rep(colnames(private$X.design), times = self$sample.size)
      )
      agg.df %>% group_by(features) %>% summarise(phi = mean(y.hat.with.k - y.hat.without.k))
    },
    intervene = function(){
      n.features = ncol(private$X.sample)
      # The intervention
      runs = lapply(1:self$sample.size, function(m){
        # randomly order features
        new.feature.order = sample(1:n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled = private$X.sample[sample(1:nrow(private$X.sample), 1), new.feature.order]
        x.interest.shuffled = self$x.interest[new.feature.order]
        
        lapply(1:n.features, function(k){
          k.at.index = which(new.feature.order == k)
          instance.with.k = x.interest.shuffled
          if(k.at.index < ncol(self$x.interest)){
            instance.with.k[(k.at.index + 1):ncol(instance.with.k)] =
              sample.instance.shuffled[(k.at.index + 1):ncol(instance.with.k)]
          }
          instance.without.k = instance.with.k
          instance.without.k[k.at.index] = sample.instance.shuffled[k.at.index]
          cbind(instance.with.k[private$sampler$feature.names], instance.without.k[private$sampler$feature.names])
        }) %>% data.table::rbindlist()
        
      }) %>% data.table::rbindlist()
      dat.with.k = data.frame(runs[,1:(ncol(runs)/2)])
      dat.without.k = data.frame(runs[,(ncol(runs)/2 + 1):ncol(runs)])
      
      rbind(dat.with.k, dat.without.k)
    }
  ),
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$flush()
      self$run()
      self
    }
  )
)






