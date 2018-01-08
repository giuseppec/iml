


## TODO: combine ICE and ICE.centered, so that active binding can be used for both. 
##       anchoring will then wrapped in if-clause
#' @param center.at The value for the centering of the plot. Numeric for numeric features, and the level name for factors.
ice = function(object, X, feature, grid.size=10, sample.size=100, center.at = NULL, class=NULL, multi.class=FALSE, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, multi.class = multi.class, ...)
  
  if(is.null(center.at)){
    obj = ICE$new(predictor = pred, sampler = samp, feature = feature, grid.size = grid.size, sample.size = sample.size)
  } else {
    obj = ICE.centered$new(predictor = pred, sampler = samp, anchor = center.at,  feature = feature, grid.size = grid.size, sample.size = sample.size)
  }
  obj$run()
  obj
}




ICE = R6Class('ICE',
  inherit = PDP,
  public = list(
    aggregate = function(){
      X.id = apply(self$X.design, 1, function(x) digest(x[-self$feature.index]))
      X.results = self$X.design[self$feature.index]
      X.results$y.hat = private$Q.results
      X.results$group = X.id
      X.results
    }, 
    initialize = function(feature, ...){
      assert_count(feature)
      super$initialize(feature=feature, ...)
    }
  ),
  private = list(
    generate.plot = function(){
      p = ggplot(private$results, mapping = aes_string(x = names(private$results)[1], y = 'y.hat', group = 'group'))
      if(self$feature.type == 'numerical') p + geom_line()
      else if (self$feature.type == 'categorical') p + geom_line(alpha = 0.2) + geom_point()
    }
  )
)



ICE.centered = R6Class('ICE.centered',
  inherit = ICE,
  public = list(
    anchor.value = NULL,
    aggregate = function(){
      X.aggregated = super$aggregate()
      X.aggregated.anchor = X.aggregated[X.aggregated[1] == self$anchor.value, c('y.hat', 'group')]
      names(X.aggregated.anchor) = c('anchor', 'group')
      X.aggregated = left_join(X.aggregated, X.aggregated.anchor, by = 'group')
      X.aggregated$y.hat = X.aggregated$y.hat - X.aggregated$anchor
      X.aggregated$anchor = NULL
      X.aggregated
    },
    intervene = function(){
      X.design = super$intervene()
      X.design.anchor = self$X.sample
      X.design.anchor[self$feature.index] = self$anchor.value
      rbind(X.design, X.design.anchor)
    },
    initialize = function(anchor, ...){
      self$anchor.value = anchor
      super$initialize(...)
    }
  ),
  active = list(
    center.at = function(anchor){
      self$anchor.value = anchor
      private$flush()
    }
  )
)
