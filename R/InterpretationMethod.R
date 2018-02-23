
#' Generic InterpretationMethod class
#' 
#' 
NULL

InterpretationMethod = R6::R6Class("InterpretationMethod",
  public = list(
    plot = function(...) {
      private$plot.data = private$generate.plot(...)
      if (!is.null(private$plot.data)) {
        return(private$plot.data)} else {warning("no plot data generated")
        }
    },
    initialize = function(predictor, data) {
      checkmate::assert_class(predictor, "Predictor")
      if(inherits(data, "data.frame")){
        data = Data$new(data)
      }      
      private$predictor = predictor
      private$predict = predictor$predict
      private$sampler = data
      private$get.data = private$sampler$get.x
    },
    data = function() {
      private$results
    },
    print = function() {
      cat("Interpretation method: ", class(self)[1], "\n")
      private$print.parameters()
      cat("\n\nAnalysed model: \n")
      private$predictor$print()
      cat("\n\nAnalysed data:\n")
      print(private$sampler)
      cat("\n\nHead of results:\n")
      if (private$finished) {
        print(head(self$data()))
      }
    },
    run = function(force = FALSE, ...) {
      if (force) private$flush()
      if (!private$finished) {
        # DESIGN experiment
        private$X.sample = private$get.data()
        private$X.design = private$intervene()
        # EXECUTE experiment
        predict.results = private$predict(private$X.design)
        private$multi.class = ifelse(ncol(predict.results) > 1, TRUE, FALSE)
        private$Q.results = private$Q(predict.results)
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
    get.data = NULL,
    # The sampled data
    X.sample = NULL,
    # The intervention on the sample
    intervene = function() private$X.sample,
    # The design matrix after intervention
    X.design = NULL,
    # The predictor
    predictor = NULL,
    # The wrapper for the predictor
    predict = NULL,
    # The quantity of interest from black box model prediction
    Q = function(x) x,
    Q.results = NULL,
    # Flag if the prediction is multi.class (more than one column)
    multi.class = NULL,
    # Weights for the aggregation step
    weight.samples = function() 1,
    # The aggregation function for the results
    aggregate = function() cbind(private$X.design, private$Q.results),
    # The aggregated results of the experiment
    results = NULL,
    # Flag if the experiment is finished
    finished = FALSE,
    # Removes experiment results as preparation for running experiment again
    flush = function() {
      private$X.sample = NULL
      private$X.design = NULL
      private$Q.results = NULL
      private$results = NULL
      private$finished = FALSE
    }, 
    # The data need for plotting of results
    plot.data = NULL,
    # Function to generate the plot
    generate.plot = function() NULL,
    # Feature names of X
    feature.names = NULL, 
    print.parameters = function() {}
  )
)
