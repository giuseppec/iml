#' Permutation feature importance
#' 
#' 
#' @param loss The loss function to use: l(actual, predicted) -> R
#' 
#' @export
#' @importFrom data.table rbindlist
#' @template args_experiment_wrap
perm.imp = function(object, X, y, class=NULL, loss, classif.type = NULL, ...){
  assert_vector(y, any.missing = FALSE)
  loss.fct = getFromNamespace(loss, "Metrics")
  
  samp = DataSampler$new(X, y = data.frame(y = y))
  pred = prediction.model(object, class = class, ...)
  
  PermImp$new(predictor = pred, sampler = samp, loss=loss.fct, classif.type = classif.type)$run()
}


## TODO: Use different performance function for regression
## TODO: implement random sampling instead of whole x
## TODO: Implement multi.class
PermImp = R6::R6Class('PermImp', 
  inherit = Experiment,
  public = list(
    y = NULL,
    loss = NULL,
    error.original = NULL,
    initialize = function(predictor, sampler, loss){
      super$initialize(predictor = predictor, sampler = sampler)
      self$loss = private$set.loss(loss)
      private$sample.x = private$sampler$get.xy
      private$predict = private$predictor$predict.class
      self$error.original = loss(private$sampler$y[[1]], private$predict(private$sampler$X)[[1]])
    }
  ),
  private = list(
    shuffle.feature = function(feature.name){
      X.inter = private$X.sample
      X.inter[feature.name] = X.inter[sample(1:nrow(private$X.sample)), feature.name]
      X.inter$..feature = feature.name
      X.inter
    },
    intervene = function(){
      X.inter.list = lapply(private$sampler$feature.names, function(i) private$shuffle.feature(i))
      data.frame(rbindlist(X.inter.list))
    },
    aggregate = function(){
      y = private$X.design[private$sampler$y.names]
      y.hat = private$Q.results
      # For classification we work with the class labels instead of probs
      
      classes = colnames(y.hat)
      result = data.frame(..feature = private$X.design$..feature, ..actual = y[[1]])
      result = cbind(result, y.hat)
      result = gather(result, "..class", "..predicted", one_of(classes))
      
      # TODO: aggregate by ..class
      result = result %>% group_by(..feature, ..class) %>% 
        summarise(error = self$loss(..actual, ..predicted), importance = error / self$error.original)
      if(!private$multi.class){
        result$..class = NULL
      }
      result
    },
    generate.plot = function(){
      # TODO: implement multi.class
      ggplot(private$results) + geom_bar(aes(x = ..feature, y = importance), stat='identity')
    }, 
    set.loss = function(loss){
      self$loss = loss
    }
  ), 
  active = list(
    feature = function(feature.index){
      self$feature.index = feature.index
      private$flush()
    }
  )
)








