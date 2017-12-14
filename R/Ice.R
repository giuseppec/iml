ICE = R6Class('ICE', 
  inherit = PDP, 
  public = list(
    aggregate = function(){
      X.id = apply(self$X.design, 1, function(x) digest(x[-self$feature.index]))
      X.results = self$X.design[self$feature.index]
      X.results$y.hat = self$Q.results
      X.results$group = X.id
      X.results
    }, 
    plot = function(){
      ggplot(private$results) + geom_line(aes_string(x = names(private$results)[1], y = 'y.hat', group = 'group'))
    }
  )
)


ICE.centered = R6Class('ICE.centered', 
  inherit = ICE, 
  public = list(
    anchor = NULL,
    aggregate = function(){
      X.aggregated = super$aggregate()
      X.aggregated.anchor = X.aggregated[X.aggregated[1] == self$anchor, c('y.hat', 'group')]
      names(X.aggregated.anchor) = c('anchor', 'group')
      X.aggregated = left_join(X.aggregated, X.aggregated.anchor, by = 'group')
      X.aggregated$y.hat = X.aggregated$y.hat - X.aggregated$anchor
      X.aggregated$anchor = NULL
      X.aggregated
    },
    intervene = function(){
      X.design = super$intervene()
      X.design.anchor = self$X.sample
      X.design.anchor[self$feature.index] = self$anchor
      rbind(X.design, X.design.anchor)
    },
    initialize = function(anchor, ...){
      self$anchor = anchor
      super$initialize(...)
    }
  )
)
