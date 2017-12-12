## TODO: add parameter_list for initiation of Experiment, which contains experiment type specific parameters (e.g. grid_size for partial dependence plots)
## Or maybe it makes sense to have subclasses for each interpretability method that inherits from Experiment and has additional parameters
## TODO: Move most parameters to private()
Experiment = R6Class("Experiment", 
  public = list(
    name = NULL, 
    f = NULL, 
    sampler = function(x){x}, 
    intervene = function(x){x},
    aggregate = function(x){x}, 
    display = function(x){print(x)}, 
    weight.samples = function(x){1}, 
    Q = function(x){x},
    Q.results = NULL,
    X.design = NULL,
    results = NULL,
    X.sample = NULL,
    m = NULL,
    ## TODO: Only allow name, f and sampler here. The others should have setter and getter. 
    ##       Then it will be easier to create subclasses like pdp.experiment. Alternative: In subclasses use: 
    ##      super$initialize if possible
    initialize = function(name, f, sampler, aggregate, display, weight.samples, Q=function(x){x}, m=1000){
      self$name = name
      self$f = f
      self$sampler = sampler
      self$aggregate = aggregate
      self$display = display
      self$weight.samples = weight.samples
      self$Q = Q
      self$m = m
    },
    conduct = function(...){
      # DESIGN experiment
      self$X.sample = self$sampler()
      self$X.design = self$intervene()
      # EXECUTE experiment
      self$Q.results = self$Q(self$f(self$X.design))
      w = self$weight.samples()
      # AGGREGATE measurments
      self$results = self$aggregate()
      self
    }, 
    present = function(){
      self$display()
    }
  )
)








