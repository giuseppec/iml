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

## TODO: add parameter_list for initiation of Experiment, which contains experiment type specific parameters (e.g. grid_size for partial dependence plots)
## Or maybe it makes sense to have subclasses for each interpretability method that inherits from Experiment and has additional parameters
## TODO: Move most parameters to private()
Experiment = R6Class("Experiment", 
  public = list(
    name = NULL, 
    f = NULL, 
    sampler = NULL, 
    intervene = NULL, 
    aggregate = NULL, 
    display = NULL, 
    weight.samples = NULL, 
    Q = NULL,
    Q.results = NULL,
    X.design = NULL,
    results = NULL,
    ## TODO: Only allow name, f and sampler here. The others should have setter and getter. 
    ##       Then it will be easier to create subclasses like pdp.experiment. Alternative: In subclasses use: 
    ##      super$initialize if possible
    initialize = function(name, f, sampler, intervene, aggregate, display, weight.samples, Q=function(x){x}){
      self$name = name
      self$f = f
      self$sampler = sampler
      self$intervene = intervene
      self$aggregate = aggregate
      self$display = display
      self$weight.samples = weight.samples
      self$Q = Q
    },
    conduct = function(...){
      # DESIGN experiment
      self$X.design = self$intervene(self$sampler, ...)
      # EXECUTE experiment
      self$Q.results = self$Q(self$f(X = self$X.design))
      w = self$weight.samples(self$X.design, ...)
      # AGGREGATE measurments
      self$results = self$aggregate(X=self$X.design, y.hat = self$Q.results, w = w, ...)
      self
    }, 
    present = function(){
      self$display(self$results)
    }
  )
)




Interpretation = R6Class("Interpretation", 
  public = list(
    experiments = NULL,
    initialize = function(experiments){
      self$experiments = experiments
    }, 
    conduct = function(){
      self$experiments = lapply(self$experiments, function(x){
        x$conduct()
      })
    }
  )
)







