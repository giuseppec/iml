#' Local models
#' 
#' @export
#' @template args_experiment_wrap
lime = function(object, X, sample.size=100, k = 3, x.interest, class = NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  LIME$new(predictor = pred, sampler = samp, sample.size=sample.size, k = k, x.interest = x.interest)$run()
}


#' @export
predict.LIME = function(object, newdata, ...){
  object$predict(newdata = newdata, ...)
}

# TODO: Implement for classification
# TODO: Implement multi.class
# TODO: Allow categorical feature (sampler has to be changed also)
# Differences to original LIME: 
# - Sample directly from data, not from weird normal distribution per feature
# - Best k features are chosen by Lasso path
LIME = R6::R6Class('LIME', 
  inherit = Experiment,
  public = list(
    x.interest = NULL, 
    k = NULL,
    model = NULL,
    predict = function(X=self$X){
      predict(self$model, newdata=X)
    },
    data = function(){
      cbind(private$X.design, w = private$weight.samples())
    },
    initialize = function(predictor, sampler, sample.size, k, x.interest, class, ...){
      if(!require('glmnet')){stop('Please install glmnet')}
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$k = k
      self$x.interest = x.interest
    }
  ),
  private = list(
    generate.plot = function(){
      ggplot(private$results) + geom_point(aes(y = effect, x = feature)) + coord_flip() 
    },
    aggregate = function(){
      mmat = model.matrix(unlist(private$Q.results[1]) ~ ., data = private$X.design)
      res = glmnet(x = mmat, y = unlist(private$Q.results[1]), w = private$weight.samples())
      best.index = max(which(res$df == self$k))
      res = data.frame(beta = res$beta[, best.index])
      res$x = mmat[1,]
      res$effect = res$beta * res$x
      res$feature = colnames(mmat)
      res
    },
    intervene = function(){
      private$X.sample = rbind(self$x.interest, private$X.sample)
      return(private$X.sample)
    }, 
    weight.samples = function(){
      require('gower')
      gower_dist(private$X.design, self$x.interest)
    }
  ),
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$flush()
    }
  )
)







