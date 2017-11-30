intervene.ice = function(generate.fun, feature.index, grid.size = 10, n = 100, ...){
  n = min(nrow(background), n)
  X = generate.fun(n)
  X = data.frame(data.table::rbindlist(lapply(1:grid.size, function(x){X})))
  grid = seq(from = min(X[feature.index]), to = max(X[feature.index]), length.out = grid.size)
  # The intervention
  X[feature.index] = rep(grid, each = n)
  X
}


aggregate.ice = function(X, w=NULL, y.hat, feature.index, ...){
  X.id = apply(X, 1, function(x) digest(x[-feature.index]))
  X = X[feature.index]
  X$y.hat = y.hat
  X$group = X.id
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

display.ice = function(res){
  ggplot(res) + geom_line(aes_string(x = names(res)[1], y = 'y.hat', group = 'group'))
}
