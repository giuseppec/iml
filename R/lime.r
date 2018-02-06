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

plot.LIME = function(object){
  object$plot()
}

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
    sample.size = NULL,
    predict = function(X=self$X){
      
      ## TODO: Integrate recoding here.
      
      predict(self$model, newdata=X)
    },
    initialize = function(predictor, sampler, sample.size, k, x.interest, class, ...){
      checkmate::assert_number(k, lower = 1, upper = sampler$n.features)
      checkmate::assert_data_frame(x.interest)
      if(!require('glmnet')){stop('Please install glmnet')}
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$k = k
      self$x.interest = x.interest
      private$get.data = function(...) private$sampler$sample(n = self$sample.size, ...)
    }
  ),
  private = list(
    Q = function(pred) probs.to.labels(pred),
    feature.center = NULL,
    feature.scale = NULL,
    standardise = function(dat){
      if(is.null(private$feature.center)) {
        dat.scaled = scale(dat)
        private$feature.center = attr(dat.scaled, "scaled:center")
        private$feature.scale = attr(dat.scaled, "scaled:scale")
        dat.scaled
      } else {
        sweep(sweep(df,2,private$feature.center,"-"),2,private$feature.scale,"/")
      }
    },
    destandardise = function(dat){
      sweep(sweep(dat,2,private$feature.scale,"*"),2,private$feature.center,"+")
    },
    aggregate = function(){
      browser()
      X.recode = recode.data(private$X.design, self$x.interest)
      X.recode = private$standardise(X.recode)
      fam = ifelse(private$multi.class, 'multinomial', 'gaussian')
      #mmat = model.matrix(unlist(private$Q.results[1]) ~ ., data = X.recode)
      res = glmnet(x = as.matrix(X.recode), y = unlist(private$Q.results[1]), family = fam, w = private$weight.samples(), 
        intercept = FALSE)
      ## It can happen, that no n.vars matching k occurs
      if(any(res$df == self$k)){
        best.index = max(which(res$df == self$k))
      } else {
        best.index = max(which(res$df < self$k))
        warning("Had to choose a smaller k")
      }
      if(private$multi.class){
        class.results = lapply(res$beta, extract.glment.effects, best.index = best.index, model.mat = X.recode)
        res = rbindlist(class.results)
        res$..class = rep(names(class.results), each = ncol(mmat))
      } else {
        res = extract.glment.effects(res$beta, best.index = best.index, model.mat = X.recode)
      }
      X.recode = private$destandardise(X.recode)
      res$x = X.recode[1,]
      res[res$beta != 0, ]
    },
    intervene = function(){
      X.sample = rbind(self$x.interest, private$X.sample)
      return(private$X.sample)
    }, 
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







