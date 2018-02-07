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
predict.LIME = function(object, newdata = NULL, ...){
  object$predict(newdata = newdata, ...)
}

plot.LIME = function(object){
  object$plot()
}

## TODO: Add binarization option for numerical features
# Differences to original LIME: 
# - Sample directly from data, not from weird normal distribution per feature
# - Best k features are chosen by Lasso path
LIME = R6::R6Class('LIME', 
  inherit = Experiment,
  public = list(
    x.interest = NULL, 
    k = NULL,
    model = NULL,
    sample.size = NULL,
    predict = function(newdata = NULL, ...){
      if(is.null(newdata)) newdata = self$x.interest
      X.recode = recode.data(newdata, self$x.interest)
      X.recode = private$standardise(X.recode)
      prediction = predict(self$model, newx=as.matrix(X.recode))
      if(private$multi.class){
        data.frame(prediction[,,private$best.index])
      } else {
        pred = prediction[,private$best.index, drop=FALSE]
        colnames(pred) = NULL
        data.frame(prediction = pred)
      }
    },
    initialize = function(predictor, sampler, sample.size, k, x.interest, class, ...){
      checkmate::assert_number(k, lower = 1, upper = sampler$n.features)
      checkmate::assert_data_frame(x.interest)
      if(!require('glmnet')){stop('Please install glmnet.')}
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$k = k
      self$x.interest = x.interest
      private$get.data = function(...) private$sampler$sample(n = self$sample.size, ...)
    }
  ),
  private = list(
    Q = function(pred) probs.to.labels(pred),
    best.index = NULL,
    feature.center = NULL,
    feature.scale = NULL,
    standardise = function(dat){
      if(is.null(private$feature.center)) {
        dat.scaled = scale(dat)
        private$feature.center = attr(dat.scaled, "scaled:center")
        private$feature.scale = attr(dat.scaled, "scaled:scale")
        return(dat.scaled)
      }
      sweep(sweep(dat,2,private$feature.center,"-"),2,private$feature.scale,"/")
    },
    aggregate = function(){
      X.recode = recode.data(private$X.design, self$x.interest)
      X.recode = private$standardise(X.recode)
      x.scaled = private$standardise(recode.data(self$x.interest, self$x.interest))
      fam = ifelse(private$multi.class, 'multinomial', 'gaussian')
      self$model = glmnet(x = as.matrix(X.recode), y = unlist(private$Q.results[1]), family = fam, w = private$weight.samples(), 
        intercept = TRUE)
      res = self$model
      ## It can happen, that no n.vars matching k occurs
      if(any(res$df == self$k)){
        best.index = max(which(res$df == self$k))
      } else {
        best.index = max(which(res$df < self$k))
        warning("Had to choose a smaller k")
      }
      private$best.index = best.index
      if(private$multi.class){
        class.results = lapply(res$beta, extract.glment.effects, 
          best.index = best.index, x.scaled = x.scaled, x.original = self$x.interest)
        res = rbindlist(class.results)
        res$..class = rep(names(class.results), each = ncol(X.recode))
      } else {
        res = extract.glment.effects(res$beta, best.index, x.scaled, self$x.interest)
      }
      res[res$beta != 0, ]
    },
    intervene = function(){private$X.sample}, 
    generate.plot = function(){
      p = ggplot(private$results) + geom_point(aes(y = feature.value, x = effect))
      if(private$multi.class) p = p + facet_wrap("..class")
      p
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
      self$run()
    }
  )
)







