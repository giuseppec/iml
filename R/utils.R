weight.samples.generic = function(X, ...){
  rep(1, times = nrow(X))
}



ident = function(X, ...){
  X
}

get.generate.fun.mc = function(dat){
  function(n, ...){
    background[sample(1:nrow(background), size = n, replace = TRUE), ]
  }
}