intervene.pdp = function(generate.fun, feature.index, grid.size = 10, n=100, ...){
  X = generate.fun(grid.size * n)
  grid = seq(from = min(X[feature.index]), to = max(X[feature.index]), length.out = grid.size)
  # The intervention
  X[feature.index] = rep(grid, times = n)
  X
}


aggregate.pdp = function(X, w=NULL, y.hat, feature.index, ...){
  X$y.hat = y.hat
  X %>% group_by_at(feature.index) %>% summarise(y.hat = mean(y.hat))
}

display.pdp = function(res){
  ggplot(res) + geom_line(aes_string(x = names(res)[1], y = names(res[2])))
}

## TODO: Think about moving functions into class directly


PDP = R6Class('PDP', inherit = Experiment,
  public = list(
    n = NULL, 
    grid.size = NULL, 
    feature.index = NULL,
    initialize = function(f, sampler, feature.index, grid.size=10, n=100){
      super$initialize('PDP', f, sampler, intervene.pdp, aggregate = aggregate.pdp, display.pdp, id, id)
      self$grid.size = grid.size
      self$n = n
      feature.index = feature.index
    }
  )
)

