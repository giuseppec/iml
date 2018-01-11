## TODO: Move most parameters to private()
Experiment = R6Class("Experiment",
  public = list(
    sample.size = 100,
    X.sample = NULL,
    intervene = function(){self$X.sample},
    X.design = NULL,
    predictor = NULL, 
    # The sampling object for sampling from X
    sampler = NULL,
    Q = function(x){x},
    weight.samples = function(){1},
    aggregate = function(){cbind(self$X.design, private$Q.results)},
    plot = function(){
      self$run()
      private$plot.data = private$generate.plot()
      if(!is.null(private$plot.data)) {return(private$plot.data)} else {warning('no plot data generated')}
    },
    summary = function(){
      summary(self$results)
    },
    initialize = function(predictor, sampler){
      assert_class(predictor, 'Prediction')
      assert_class(sampler, 'DataSampler')
      self$predictor = predictor
      private$predict = predictor$predict
      self$sampler = sampler
      private$sample.x = self$sampler$sample
    },
    run = function(force = FALSE, ...){
      if(force) private$flush()
      if(!private$finished){
        
        # DESIGN experiment
        self$X.sample = private$sample.x(self$sample.size)
        self$X.design = self$intervene()
        # EXECUTE experiment
        private$Q.results = self$Q(private$predict(self$X.design))
        # AGGREGATE measurements
        private$results = self$aggregate()
        private$finished = TRUE
      }
      self
    },
    data = function(){
      private$results
    }
  ),
  private = list(
    # The aggregated results of the experiment
    results = NULL,
    # Flag if the experiment is finished
    finished = FALSE,
    # The quantity of interest from black box model prediction
    Q.results = NULL,
    # The data need for plotting of results
    plot.data = NULL,
    # Function to generate the plot
    generate.plot = function(){NULL},
    # Feature names of X
    feature.names = NULL,
    # Wrapper for sampler
    sample.x = NULL,
    # Removes experiment results as preparation for running experiment again
    flush = function(){
      self$X.sample = NULL
      self$X.design = NULL
      private$Q.results = NULL
      private$results = NULL
      private$finished = FALSE
    }, 
    predict = NULL
  )
)



RepeatedExperiment = R6Class('RepeatedExperiment', 
  public = list(
    experiments = NULL,
    results = NULL,
    initialize = function(experiments){
      self$experiments = experiments
    }, 
    run = function(force = FALSE){
      if(!private$finished){
        self$experiments = lapply(self$experiments, function(experiment){
          experiment$run(force = force)
        })
        self$results = self$data()
        private$finished = TRUE
      }
      self
    },
    flush = function(){
      lapply(self$experiments, function(experiment){
        experiment$flush()
      })
    }, 
    data = function(){
      dfs = lapply(self$experiments, function(experiment){
        experiment$data()
      })
      data.table::rbindlist(dfs)
    }
  ), 
  private = list(
    finished = FALSE
  )
)






































