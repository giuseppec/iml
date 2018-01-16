lime = function(object, X, sample.size=100, k = 3, x.interest, class=NULL, multi.class=FALSE, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, multi.class = multi.class, ...)
  
  LIME$new(predictor = pred, sampler = samp, sample.size=sample.size, k = k, x.interest = x.interest)$run()
}



# TODO: Implement for classification
# TODO: Implement generate.plot function
# TODO: Implement multi.class
# TODO: Allow categorical feature (sampler has to be changed also)
# Differences to original LIME: 
# - Sample directly from data, not from weird normal distribution per feature
# - Best k features are chosen by Lasso path
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
      res = data.frame(beta = res$beta[, best.index])
      res$x = mmat[1,]
      res$effect = res$beta * res$x
      res$feature = colnames(mmat)
      res
    },
    intervene = function(){
      self$X.sample = rbind(self$x.interest, self$X.sample)
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
    initialize = function(predictor, sampler, sample.size, k, x.interest, class, multi.class, ...){
      if(!require('glmnet')){stop('Please install glmnet')}
      if(predictor$multi.class) stop('multi.class not supported yet. Please choose a class')
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$k = k
      self$x.interest = x.interest
    }
  ),
  private = list(
    generate.plot = function(){
      ggplot(private$results) + geom_point(aes(y = effect, x = feature)) + coord_flip() 
    }
  ),
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$flush()
    }
  )
)







