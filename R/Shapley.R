## TODO: instead having an outer loop over features,
##       loop over features within each coalition, like the ApproShapley algorithms.
##       Additionally make sure to not calculate things twice, because  it's always
##       the difference of coalition of features with and without feature j.
##       see Song, E., & Nelson, B. L. (2016). Shapley Effects for Global Sensitivity Analysis : Theory and Computation ∗, 4, 1060–1083.
## TODO: Get some inspiration form the sensitivity package.
Shapley = R6Class('Shapley', 
  inherit = Experiment,
  public = list(
    x.interest = NULL,
    aggregate = function(){
      agg.df = data.frame(
        y.hat.with.k = self$Q.results[1:(length(self$Q.results)/2)],
        y.hat.without.k = self$Q.results[(length(self$Q.results)/2 + 1):length(self$Q.results)],
        features = rep(colnames(self$X.design), times = self$sample.size)
      )
      agg.df %>% group_by(features) %>% summarise(phi = mean(y.hat.with.k - y.hat.without.k))
    },
    intervene = function(){
      n.features = ncol(self$X.sample)
      feature.names = colnames(self$X.sample)
      # The intervention
      runs = lapply(1:self$sample.size, function(m){
        # randomly order features
        new.feature.order = sample(1:n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled = self$X.sample[sample(1:nrow(self$X.sample), 1), new.feature.order]
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
          cbind(instance.with.k[feature.names], instance.without.k[feature.names])
        }) %>% data.table::rbindlist()
        
      }) %>% data.table::rbindlist()
      dat.with.k = data.frame(runs[,1:(ncol(runs)/2)])
      dat.without.k = data.frame(runs[,(ncol(runs)/2 + 1):ncol(runs)])
      
      rbind(dat.with.k, dat.without.k)
    }, 
    set.new.x = function(x.interest){
      self$x.interest = x.interest
      private$finished = FALSE
      private$results = NULL
    },
    summary = function(){
      self$results
    },
    initialize = function(f, X, x.interest, sample.size=100){
      self$sample.size = sample.size
      self$f = f
      self$X = X
      self$x.interest = x.interest
    }
  )
)






