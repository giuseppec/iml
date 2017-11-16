intervene.permimp = function(generate.fun, feature.index, n = 100, ...){
  X = generate.fun(n=n)
  X.inter = X
  # Shuffle feature
  X.inter[feature.index] = X.inter[sample(1:nrow(X)), feature.index]
  rbind(X, X.inter)
}


aggregate.permimp = function(y.hat, y, ...){
  stopifnot(length(y.hat)/2 == length(y))
  performance = function(y.hat, y){
    mean(log(y.hat + 0.00001)*y + log((1-y.hat) + 0.00001) * (1-y))
  }
  performance(y.hat[1:(length(y.hat)/2)], y) - performance(y.hat[(length(y.hat)/2 + 1):length(y.hat)], y)
}