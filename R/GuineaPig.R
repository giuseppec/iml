GuineaPig = R6Class("GuineaPig", 
                    public = list(
                      experiments = NULL,
                      mod = NULL,
                      X = NULL,
                      initialize = function(mod, X, experiments){
                        self$experiments = experiments
                        self$X = X
                        slef$mod = mod
                      }, 
                      conduct = function(){
                        self$experiments = lapply(self$experiments, function(x){
                          x$conduct()
                        })
                      }
                    )
)