intervene.ice = function(generate.fun, feature.index, grid.size = 10, n = 100, ...){
  n = min(nrow(background), n)
  X = generate.fun(n)
  X = data.frame(data.table::rbindlist(lapply(1:grid.size, function(x){X})))
  grid = seq(from = min(X[feature.index]), to = max(X[feature.index]), length.out = grid.size)
  # The intervention
  X[feature.index] = rep(grid, each = n)
  X
}




aggregate.ice.centered = function(X, y.hat, feature.index, anchor, ...){
  X.aggregated = aggregate.ice(X=X, y.hat=y.hat, feature.index = feature.index, ...)
  anchor.closest = X[which.min((X[,feature.index] - anchor)), feature.index][1]
  X.anchor = aggregate.ice(X = X[X[feature.index] == anchor.closest,], 
    y.hat = y.hat[X[feature.index] == anchor.closest], 
    feature.index = feature.index)
  X.anchor$y.hat.anchor = X.anchor$y.hat
  X.anchor$y.hat = NULL
  X.aggregated = left_join(X.aggregated, X.anchor, by = 'group')
  X.aggregated$y.hat = X.aggregated$y.hat -  X.aggregated$y.hat.anchor 
  X.aggregated
}







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
    display = function(){
      ggplot(self$results) + geom_line(aes_string(x = names(self$results)[1], y = 'y.hat', group = 'group'))
    }
  )
)
