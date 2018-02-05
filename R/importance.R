#' Feature importance
#' 
#' @description 
#' imp() computes the feature importance for a machine learning model. 
#' The importance of a feature is the loss in model performance when the feature is shuffled. 
#' 
#' @details
#' #' To learn more about feature importance, read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/permutation-feature-importance.html
#' 
#' @param loss The loss function to use: l(actual, predicted) -> R
#' 
#' @export
#' @importFrom data.table rbindlist
#' @template args_experiment_wrap
importance = function(object, X, y, class=NULL, loss, method = 'shuffle', ...){
  assert_vector(y, any.missing = FALSE)
  if(!inherits(loss, 'function')){
  ## Only allow metrics from Metrics package
   #assert_choice(loss, ls('package:Metrics'))
   loss = getFromNamespace(loss, "Metrics")
  }
  samp = DataSampler$new(X, y = data.frame(y = y))
  pred = prediction.model(object, class = class,...)
  
  Importance$new(predictor = pred, sampler = samp, loss=loss, method=method)$run()
}


## TODO: Use different performance function for regression
## TODO: implement random sampling instead of whole x
## TODO: Implement multi.class
Importance = R6::R6Class('Importance', 
  inherit = Experiment,
  public = list(
    y = NULL,
    loss = NULL,
    error.original = NULL,
    initialize = function(predictor, sampler, loss, method){
      checkmate::assert_choice(method, c('shuffle', 'cartesian'))
      super$initialize(predictor = predictor, sampler = sampler)
      private$loss.string = loss
      self$loss = private$set.loss(loss)
      private$method = method
      private$sample.x = private$sampler$get.xy
      # In case of multi.class, forces labels, otherwise same as normal predict.
      private$predict = function(newdata) private$predictor$predict(newdata, labels = TRUE)
      self$error.original = loss(private$sampler$y[[1]], private$predict(private$sampler$X)[[1]])
    }
  ),
  private = list(
    method = NULL,
    # for printing
    loss.string = NULL,
    shuffle.feature = function(feature.name, method){
      if(method == 'shuffle'){
        X.inter = private$X.sample
        X.inter[feature.name] = X.inter[sample(1:nrow(private$X.sample)), feature.name]
      } else if(method == 'cartesian'){
        n = nrow(private$X.sample)
        row.indices = rep(1:n, times = n)
        replace.indices = rep(1:n, each = n)
        # Indices of instances to keep. Removes those where instance matched with own value
        keep.indices = as.logical(as.vector(1 - diag(n)))
        X.inter = private$X.sample[row.indices, ]
        X.inter[feature.name] = X.inter[replace.indices, feature.name]
        X.inter = X.inter[keep.indices,]
      } else {
        stop(sprintf('%s method not implemented'))
      }
      X.inter$..feature = feature.name
      X.inter 
      
    },
    intervene = function(){
      X.inter.list = lapply(private$sampler$feature.names, function(i) private$shuffle.feature(i, method = private$method))
      data.frame(rbindlist(X.inter.list))
    },
    aggregate = function(){
      y = private$X.design[private$sampler$y.names]
      y.hat = private$Q.results
      # For classification we work with the class labels instead of probs
      result = data.frame(..feature = private$X.design$..feature, ..actual = y[[1]], ..predicted = y.hat[[1]])
      
      result = result %>% group_by(..feature) %>% 
        summarise(error = self$loss(..actual, ..predicted), 
          importance = error / self$error.original)
      result
    },
    generate.plot = function(){
      ggplot(private$results) + geom_bar(aes(x = ..feature, y = importance), stat='identity')
    }, 
    set.loss = function(loss){
      self$loss = loss
    }, 
    print.parameters = function(){
      cat('error function:', private$loss.string)
    }
  ), 
  active = list(
    feature = function(feature.index){
      self$feature.index = feature.index
      private$flush()
    }
  )
)








