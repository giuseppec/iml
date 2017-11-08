generate.sobol = function(n, ...){
  background[sample(1:nrow(background), size = n, replace = TRUE), ]
}

intervene.sobol = function(generate.fun, feature.index, grid.size = 10, n = 100, x.interest, ...){
  X1 = generate.fun(n)
  X2 = generate.fun(n)
  
  n.features = ncol(X)
  feature.names = colnames(X)
  
  # The intervention
  Ab = lapply(1:n.features, function(feature.index){
    A = X1
    A[,feature.index] = X2[feature.index]
    A
  }) %>% data.table::rbindlist()
  rbind(X1, X2, Ab)
}


aggregate.sobol.first = function(X, y.hat, n, w=NULL,...){
  y.hat.A = y.hat[1:n]
  y.hat.B = y.hat[(n+1):(2*n)]
  Ab.index = (2*n + 1):nrow(X)
  var.y = var(y.hat.A)
  lapply(1:ncol(X), function(i){
    y.hat.Ab.by.feature = matrix(y.hat[Ab.index], nrow = n)
    S_i = (1/n) * sum(y.hat.B * (y.hat.Ab.by.feature[,i] - y.hat.A))
    S_i/ var.y
  })
}

aggregate.sobol.total = function(X, y.hat, n, w=NULL,...){
  y.hat.A = y.hat[1:n]
  y.hat.B = y.hat[(n+1):(2*n)]
  Ab.index = (2*n + 1):nrow(X)
  var.y = var(y.hat.A)
  lapply(1:ncol(X), function(i){
    y.hat.Ab.by.feature = matrix(y.hat[Ab.index], nrow = n)
    S_i = (1/(2*n)) * sum((y.hat.Ab.by.feature[,i] - y.hat.A)^2)
    S_i / var.y
  })
}
