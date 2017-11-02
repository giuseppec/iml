generate.shapley = function(n){
  background[sample(1:nrow(background), size = n, replace = TRUE), ]
}

intervene.shapley = function(generate.fun, feature.index, grid.size = 10, n = 100, x.interest){
  X = generate.fun(n)
  n.features = ncol(X)
  feature.names = colnames(X)
  # The intervention
  runs = lapply(1:n, function(m){
    # randomly order features
    new.feature.order = sample(1:n.features)
    # randomly choose sample instance from X
    sample.instance.shuffled = X[sample(1:nrow(X), 1), new.feature.order]
    x.interest.shuffled = x.interest[new.feature.order]
    
    lapply(1:n.features, function(k){
      k.at.index = which(new.feature.order == k)
      instance.with.k = x.interest.shuffled
      if(k.at.index < ncol(x.interest)){
        instance.with.k[(k.at.index + 1):ncol(instance.with.k)] = 
          sample.instance.shuffled[(k.at.index + 1):ncol(instance.with.k)]
      }
      instance.without.k = instance.with.k
      instance.without.k[k.at.index] = sample.instance.shuffled[k.at.index]
      cbind(instance.with.k[feature.names], instance.without.k[feature.names])
    }) %>% data.table::rbindlist()
    
  }) %>% data.table::rbindlist()
  dat.with.k = data.frame(runs[,1:(ncol(runs)/2)])
  dat.without.k = data.frame(runs[,(ncol(runs)/2 + 1):ncol(runs)])
  
  rbind(dat.with.k, dat.without.k)
}


aggregate.shapley = function(X, w=NULL, y.hat, n, ...){
  agg.df = data.frame(
    y.hat.with.k = y.hat[1:(length(y.hat)/2)],
    y.hat.without.k = y.hat[(length(y.hat)/2 + 1):length(y.hat)],
    features = rep(colnames(X), times = n)
  )
  agg.df %>% group_by(features) %>% summarise(phi = mean(y.hat.with.k - y.hat.without.k))
}








