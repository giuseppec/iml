


## TODO: Move most parameters to private()
Experiment = R6Class("Experiment",
  public = list(
    sample.size = 100,
    plot = function(){
      self$run()
      private$plot.data = private$generate.plot()
      if(!is.null(private$plot.data)) {return(private$plot.data)} else {warning('no plot data generated')}
    },
    initialize = function(predictor, sampler){
      assert_class(predictor, 'Prediction')
      assert_class(sampler, 'DataSampler')
      private$predictor = predictor
      private$predict = predictor$predict
      private$sampler = sampler
      private$sample.x = private$sampler$sample
    },
    data = function(){
      private$results
    },
    print = function(){
      cat("Interpretation method: ", class(self)[1], "\n")
      private$print.parameters()
      cat("\n\nAnalysed model: \n")
      private$predictor$print()
      cat("\n\nAnalysed data:\n")
      print(private$sampler)
      cat("\n\nHead of results:\n")
      if(private$finished){
        print(head(self$data()))
      }
    },
    run = function(force = FALSE, ...){
      if(force) private$flush()
      if(!private$finished){
        
        # DESIGN experiment
        private$X.sample = private$sample.x(self$sample.size)
        private$X.design = private$intervene()
        # EXECUTE experiment
        private$Q.results = private$Q(private$predict(private$X.design))
        # AGGREGATE measurements
        private$results = private$aggregate()
        private$finished = TRUE
      }
      self
    }
  ),
  private = list(
    # The sampling object for sampling from X
    sampler = NULL,
    # Wrapper for sampler
    sample.x = NULL,
    # The sampled data
    X.sample = NULL,
    # The intervention on the sample
    intervene = function(){private$X.sample},
    # The design matrix after intervention
    X.design = NULL,
    # The predictor
    predictor = NULL,
    # The wrapper for the predictor
    predict = NULL,
    # The quantity of interest from black box model prediction
    Q = function(x){x},
    Q.results = NULL,
    # Weights for the aggregation step
    weight.samples = function(){1},
    # The aggregation function for the results
    aggregate = function(){cbind(private$X.design, private$Q.results)},
    # The aggregated results of the experiment
    results = NULL,
    # Flag if the experiment is finished
    finished = FALSE,
    # Removes experiment results as preparation for running experiment again
    flush = function(){
      private$X.sample = NULL
      private$X.design = NULL
      private$Q.results = NULL
      private$results = NULL
      private$finished = FALSE
    }, 
    # The data need for plotting of results
    plot.data = NULL,
    # Function to generate the plot
    generate.plot = function(){NULL},
    # Feature names of X
    feature.names = NULL, 
    print.parameters = function(){}
  )
)



RepeatedExperiment = R6Class('RepeatedExperiment', 
  inherit = Experiment,
  public = list(
    initialize = function(predictor, sampler, experiments){
      private$predictor = predictor
      private$sampler = sampler
      private$experiments = experiments
    }, 
    run = function(force = FALSE){
      if(!private$finished){
        private$experiments = lapply(private$experiments, function(experiment){
          experiment$run(force = force)
        })
        private$results = self$data()
        private$finished = TRUE
      }
      self
    }, 
    data = function(){
      dfs = lapply(private$experiments, function(experiment){
        experiment$data()
      })
      data.table::rbindlist(dfs)
    }
  ), 
  private = list(
    experiments = NULL,
    results = NULL,
    finished = FALSE,
    flush = function(){
      lapply(private$experiments, function(experiment){
        experiment$flush()
      })
    }
  )
)






































