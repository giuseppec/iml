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


Experiment = R6Class("Experiment", 
  public = list(
    name = NULL, 
    f = NULL, 
    sampler = NULL, 
    intervene = NULL, 
    aggregate = NULL, 
    display = NULL, 
    weight.samples = NULL
    , 
    initialize = function(name, f, sampler, intervene, aggregate, display, weight.samples){
      self$name = name
      self$f = f
      self$sampler = sampler
      self$intervene = intervene
      self$aggregate = aggregate
      self$display = display
      self$weight.samples = weight.samples
    },
    conduct = function(...){
      # DESIGN experiment
      X.design = self$intervene(self$sampler, ...)
      # EXECUTE experiment
      y.hat = self$f(X = X.design)
      w = self$weight.samples(X.design, ...)
      # AGGREGATE measurments
      res = self$aggregate(X=X.design, y.hat = y.hat, w = w, ...)
      # PRESENT
      self$display(res)
    }
  )
)


