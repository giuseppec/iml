Sobol = R6Class('Sobol', 
  inherit = Experiment,
  public = list(
    type = NULL, 
    sampler = function(){
      list(X1 = super$sampler(), X2 = super$sampler())
    },
    intervene = function(){
      n.features = ncol(self$X)
      feature.names = colnames(self$X)
      # The intervention
      Ab = lapply(1:n.features, function(feature.index){
        A = self$X.sample$X1
        A[,feature.index] = self$X.sample$X2[feature.index]
        A
      }) %>% data.table::rbindlist()
      rbind(self$X.sample$X1, self$X.sample$X2, Ab)
    },
    plot = function(){
      plot(private$results)
    }, 
    aggregate = function(){
      y.hat.A = self$Q.results[1:self$sample.size]
      y.hat.B = self$Q.results[(self$sample.size+1):(2*self$sample.size)]
      Ab.index = (2*self$sample.size + 1):nrow(self$X.design)
      var.y = var(y.hat.A)
      lapply(1:ncol(X), function(i){
        y.hat.Ab.by.feature = matrix(self$Q.results[Ab.index], nrow = self$sample.size)
        if(self$type == 'first'){
          S_i = (1/self$sample.size) * sum(y.hat.B * (y.hat.Ab.by.feature[,i] - y.hat.A))
        } else {
          S_i = (1/(2*self$sample.size)) * sum((y.hat.Ab.by.feature[,i] - y.hat.A)^2)
        }
        S_i / var.y
        
      })
    },
    initialize = function(f, X, sample.size = 100, type = 'first'){
      self$f = f
      self$X = X
      self$sample.size = sample.size
      self$type = type
    }
  )
)




