## TODO: add parameter_list for initiation of Experiment, which contains experiment type specific parameters (e.g. grid_size for partial dependence plots)
## Or maybe it makes sense to have subclasses for each interpretability method that inherits from Experiment and has additional parameters
## TODO: Move most parameters to private()
Experiment = R6Class("Experiment", 
  public = list(
    X = NULL,
    sample.size = 100,
    sampler = function(){
      replace = self$sample.size > nrow(self$X)
      self$X[sample(1:nrow(self$X), size = self$sample.size, replace = replace), ]
    }, 
    X.sample = NULL,
    intervene = function(){self$X.sample},
    X.design = NULL,
    f = NULL, 
    Q = function(x){x},
    weight.samples = function(){1}, 
    aggregate = function(){cbind(self$X.design, private$Q.results)}, 
    plot = function(){
      self$run()
      private$plot.data = private$generate.plot()
      if(!is.null(private$plot.data)) {private$plot.data} else {warning('no plot data generated')}
    },
    initialize = function(f, X){
      assertDataFrame(X, all.missing = FALSE)
      self$f = f
      self$X = X
      private$feature.names = colnames(X)
    },
    run = function(force = FALSE, ...){
      if(force) private$flush()
      if(!private$finished){
        # DESIGN experiment
        self$X.sample = self$sampler()
        self$X.design = self$intervene()
        # EXECUTE experiment
        private$Q.results = self$Q(self$f(self$X.design))
        w = self$weight.samples()
        # AGGREGATE measurments
        private$results = self$aggregate()
        private$finished = TRUE
      }
      self
    }, 
    data = function(){
      self$run()
      private$results
    }
  ), 
  private = list(
    results = NULL, 
    finished = FALSE, 
    Q.results = NULL,
    plot.data = NULL,
    generate.plot = function(){NULL},
    feature.names = NULL,
    flush = function(){
      self$X.sample = NULL
      self$X.design = NULL
      private$Q.results = NULL
      private$results = NULL
      private$finished = FALSE
    }
  )
)



# TODO: Implement repeated experiment class
RepeatedExperiment = R6Class()








