

generate.lime = function(n){
  features.mean = colMeans(background)
  features.sd = apply(background, 2, sd)
  n.features = length(features.mean)
  random.features = matrix(rnorm(n =  n * n.features), ncol = n.features)
  new.samples = data.frame(t(apply(random.features, 1, function(x) {x * features.sd + features.mean})))
  new.samples
}


weight.samples.lime = function(X, x.interest){
  apply(X, 1, function(x){
    1/sqrt(sum((x - x.interest)^2))
  })
}


intervene.lime = function(generate.fun, feature.index, n = 100, ...){
  generate.fun(n=n)
}


aggregate.lime = function(X, w=NULL, y.hat, n, ...){
  lm(y.hat ~ as.matrix(X), weights = w)
}

display.lime = function(res){
  summary(res)
}








