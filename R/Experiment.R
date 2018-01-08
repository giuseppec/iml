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
        private$Q.results = self$Q(self$predictor$predict(self$X.design))
        w = self$weight.samples()
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
    }
  )
)



# TODO: Implement repeated experiment class
# TODO: Implement option run=TRUE in Experiment
RepeatedExperiment = R6Class('RepeatedExperiment', 
  public = list(
    experiments = NULL,
    initialize = function(experiments){
      self$experiments = experiments
    }, 
    run = function(){
      self$experiments = lapply(self$experiments, function(experiment){
        experiment$run()
      })
    },
    flush = function(){
       lapply(self$experiments, function(experiment){
        experiment$flush()
      })
    }
  ), 
  private = list()
)


PermImps = R6Class('PermImps', 
  inherit = RepeatedExperiment,
  public = list(
    initialize = function(predictor, sampler, y){
      experiments = lapply(1:sampler$n.features, function(feature.index){
        x = PermImp$new(predictor = predictor, sampler = sampler, y = y, feature.index = feature.index)
        x
      })
      super$initialize(experiments)
    }, 
    data = function(){
      dfs = lapply(self$experiments, function(experiment){
        experiment$data()
      })
      data.table::rbindlist(dfs)
    }
  )
)



































