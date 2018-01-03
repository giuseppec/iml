lime = function(f, X, sample.size=100, k = 3, x.interest = NULL){
  LIME$new(f=f, X=X, sample.size=sample.size, k = k, x.interest = x.interest)
}



# TODO: Implement for classification
# TODO: Implement selection of k features
# TODO: Implement generate.plot function
# TODO: Implement full LIME
# TODO: Allow categorical feature (sampler has to be changed also)
# Differences to original LIME: Sample directly from data, not from weird normal distribution per feature
# 
LIME = R6Class('LIME', 
  inherit = Experiment,
  public = list(
    x.interest = NULL, 
    k = NULL,
    model = NULL,
    predict = function(X=self$X){
      predict(self$model, newdata=X)
    },
    get.Q = function(){
      private$Q.results
    },
    aggregate = function(){
      mmat = model.matrix(private$Q.results ~ ., data = self$X.design)
      res = glmnet(x = mmat, y = private$Q.results, w = self$weight.samples())
      best.index = max(which(res$df == self$k))
      res$beta[, best.index]
    },
    intervene = function(){
      return(self$X.sample)
    }, 
    print = function(){
      self$run()
      print(private$results)
    },
    weight.samples = function(){
      require('gower')
      gower_dist(self$X.design, self$x.interest)
    },
    initialize = function(f, X, sample.size, k, x.interest){
      if(!require('glmnet')){stop('Please install glmnet')}
      super$initialize(f, X)
      self$sample.size = sample.size
      self$k = k
      self$x.interest = x.interest
    }
  ), 
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$flush()
    }
  )
)







