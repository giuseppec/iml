explain = function(f, generate.fun, intervene, aggregate, display = id, weight.samples = weight.samples.generic,  ...){
  ## intervention
  ## Maybe generate.fun should not be called within intervene, but before. Then it is easier to generalise 
  ## the Quantity of interest function Q(X.intervention, X, ...) that goes into the aggregation. 
  ## But maybe quantity of interest does not need to be seperate, because mostly its deeply intertwined with aggregate
  X.intervention = intervene(generate.fun, ...)
  ## predict 
  y.hat = f(X = X.intervention)
  w = weight.samples(X.intervention, ...)
  ## aggregate
  res = aggregate(X=X.intervention, y.hat = y.hat, w = w,...)
  ## explain
  display(res)
}