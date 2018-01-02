

ice = function(f, X, feature, grid.size=10, sample.size=100){
  ICE$new(f = f, X=X, feature = feature, grid.size = grid.size, sample.size = sample.size)
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
    }),
  private = list(
    generate.plot = function(){
      p = ggplot(private$results, mapping = aes_string(x = names(private$results)[1], y = 'y.hat', group = 'group'))
      if(self$feature.type == 'numerical') p + geom_line()
      else if (self$feature.type == 'categorical') p + geom_line(alpha = 0.2) + geom_point()
    }
  )
)

#' @param anchor The value for the centering of the plot. Numeric for numeric features, and the level name for factors.
ice.c = function(f, X, feature, anchor = 0, grid.size=10, sample.size=100){
  ICE.centered$new(f = f, X=X, anchor = anchor, feature = feature, grid.size = grid.size, sample.size = sample.size)
}

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
    anchor = function(anchor){
      self$anchor.value = anchor
      private$flush()
    }
  )
)
