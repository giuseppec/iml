#' Predictor subclass for mlr3
#'
#' @description TDB
#' @export
Predictor_mlr3 <- R6::R6Class("Predictor_mlr3",
  inherit = Predictor,
  public = list(
    initialize = function(learner, task, class = NULL, batch.size = 1000) {
      # FIXME: I dont think we can call super$initialize() due to some specific
      # funs there, e.g. infterTaskFromModel()

      self$model = learner$model
      self$data = task$data()
      self$task = task$task_type
      # not sure about this one?
      self$class = class
      self$batch.size = batch.size
      
      self$task = switch(self$task,
        "classif" = "classification",
        "regr" = "regression"
      )
      
      private$.learner = learner
      
    }
  ),
  private = list(
    
    .learner = NULL,
    
    .predict = function(task, row_ids = NULL) {
      prediction = private$.learner$predict(task, row_ids)$data$tab[, 2:3]
      return(prediction)
    }
  )
)
