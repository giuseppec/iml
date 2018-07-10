allowedLosses = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae", 
  "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape")


Loss = R6::R6Class("Loss", 
  public = list(
    compute = function(truth, response){
      private$flush()
      update(truth.response)
    },
    compute.partial = function(truth, response){},
    result = NULL
  ),
  private = list(
    flush = function(){}
  )
)

Ce = R6::R6Class("Ce",
  inherit = Loss,
  public = list(
    update = function(truth, response){
      private$sum.error = private$sum.error + sum(truth != response)
      private$sum.total = private$sum.total + length(response)
      self$result = private$sum.error / private$sum.total
    },
    result = NULL
  ),
  private = list(
    sum.error = 0,
    sum.total = 0
  )
)


ce = Ce$new()
ce$update(c(1,1,0), c(1,2,0))
ce$update(1,0)
ce$result

Prec = R6::R6Class("Prec",
  inherit = Loss,
  public = list(
    initialize = function(target.class = NULL){
      
    },
    update = function(truth, response){
      private$ce$update(truth, response)
      self$result = 1 - private$ce$result
    },
    target.class = NULL,
    result = NULL
  ),
  private = list(
    ce = Ce$new()
  )
)

ce = Prec$new()
ce$update(c(1,1,0), c(1,2,0))
ce$result
ce$update(1,0)
ce$result
ce$update(1,0)
ce$result

F1 = R6::R6Class("F1",
  inherit = Loss,
  public = list(
    update = function(truth, response){
      private$sum.error = private$sum.error + sum(truth != response)
      private$sum.total = private$sum.total + length(response)
      self$result = private$sum.error / private$sum.total
    },
    result = NULL
  ),
  private = list(
    sum.error = 0,
    sum.total = 0
  )
)
