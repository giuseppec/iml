LIME = R6Class('LIME', 
  inherit = Experiment,
  public = list(
    x.interest = NULL, 
    aggregate = function(){
      lm(self$Q.results ~ as.matrix(self$X.design), weights = self$w)
    },
    sampler = function(){
      features.mean = colMeans(self$X)
      features.sd = apply(self$X, 2, sd)
      n.features = length(features.mean)
      random.features = matrix(rnorm(n =  self$sample.size * n.features), ncol = n.features)
      new.samples = data.frame(t(apply(random.features, 1, function(x) {x * features.sd + features.mean})))
      new.samples
    },
    intervene = function(){
      return(self$X.sample)
    }, 
    summary = function(){
      summary(private$results)
    },
    print = function(){self$summary()},
    weight.samples = function(){
      apply(self$X.design, 1, function(x){
        1/sqrt(sum((x - self$x.interest)^2))
      })
    },
    initialize = function(f, X, sample.size=100){
      self$sample.size = sample.size
      self$f = f
      self$X = X
    }
  ), 
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$finished = FALSE
      private$results = NULL
    }
  )
)







