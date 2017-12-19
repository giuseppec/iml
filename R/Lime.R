# TODO: Implement for classification
# TODO: Implement selection of k features
# TODO: Implement generate.plot function
# TODO: Implement full LIME
# TODO: Allow categorical feature (sampler has to be changed also)
LIME = R6Class('LIME', 
  inherit = Experiment,
  public = list(
    x.interest = NULL, 
    k = NULL,
    model = NULL,
    predict = function(X=self$X){
      predict(self$model, newdata=X)
    },
    aggregate = function(){
      best.param = cv.glmnet(x = as.matrix(self$X.design), y = private$Q.results, w=self$weight.samples())
      mod.best = glmnet(x = as.matrix(self$X.design), y = private$Q.results, w=self$weight.samples(), lambda = best.param$lambda.1se)
      self$model = mod.best
      mod.best
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
      coef(private$results)
    },
    print = function(){self$summary()},
    weight.samples = function(){
      apply(self$X.design, 1, function(x){
        1/sqrt(sum((x - self$x.interest)^2))
      })
    },
    initialize = function(f, X, sample.size=100, k = 3){
      if(!require('glmnet')){stop('Please install glmnet')}
      super$initialize(f, X)
      self$sample.size = sample.size
      self$k = k
    }
  ), 
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$flush()
    }
  )
)







