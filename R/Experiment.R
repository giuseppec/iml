## TODO: Move most parameters to private()
Experiment = R6Class("Experiment",
  public = list(
    X = NULL,
    sample.size = 100,
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
      if(!is.null(private$plot.data)) {return(private$plot.data)} else {warning('no plot data generated')}
    },
    initialize = function(f, X){
      assertDataFrame(X, all.missing = FALSE)
      self$f = f
      self$X = X
      private$feature.names = colnames(X)
      private$sampler = DataSampler$new(X)
      private$sample.x = private$sampler$sample
    },
    run = function(force = FALSE, ...){
      if(force) private$flush()
      if(!private$finished){
        # DESIGN experiment
        self$X.sample = private$sample.x(self$sample.size)
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
    },
    print = function(){
      print(self$plot())
    }
  ),
  private = list(
    results = NULL,
    finished = FALSE,
    Q.results = NULL,
    plot.data = NULL,
    generate.plot = function(){NULL},
    feature.names = NULL,
    sampler = NULL,
    sample.x = NULL, 
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
