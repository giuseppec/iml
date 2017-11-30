## Craven, M. W., & Shavlik, J. W. (1996). 
## Extracting tree-structured representations of trained neural networks. 
## Advances in Neural Information Processing Systems, 8, 24–30. 
## Retrieved from citeseer.ist.psu.edu/craven96extracting.html
aggregate.surrogate = function(X, w=NULL, y.hat, n, ...){
  df = data.frame(y.hat = y.hat)
  df = cbind(y.hat, X)
  partykit::ctree(y.hat ~ ., data = df)
}

display.surrogate = function(res){
  plot(res)
}



