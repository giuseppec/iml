sobol = function(object, X, sample.size = 100, type = 'first', class = NULL, multi.class = FALSE){
  Sobol$new(object = object, X=X, sample.size = sample.size, type = type, class = class, multi.class = multi.class)$run()
}




Sobol = R6Class('Sobol', 
  inherit = Experiment,
  public = list(
    type = NULL, 
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
    aggregate = function(){
      y.hat.A = private$Q.results[1:self$sample.size]
      y.hat.B = private$Q.results[(self$sample.size+1):(2*self$sample.size)]
      Ab.index = (2*self$sample.size + 1):nrow(self$X.design)
      var.y = var(y.hat.A)
      res = lapply(1:ncol(X), function(i){
        y.hat.Ab.by.feature = matrix(private$Q.results[Ab.index], nrow = self$sample.size)
        if(self$type == 'first'){
          S_i = (1/self$sample.size) * sum(y.hat.B * (y.hat.Ab.by.feature[,i] - y.hat.A))
        } else {
          S_i = (1/(2*self$sample.size)) * sum((y.hat.Ab.by.feature[,i] - y.hat.A)^2)
        }
        S_i / var.y
      }) 
      data.frame(value = unlist(res), feature = private$feature.names)
    },
    print = function(){
      print(self$data())
    },
    initialize = function(object, X, sample.size, type, class, multi.class){
      if(multi.class) stop('multi.class not supported yet for sobol')
      super$initialize(object, X, class, multi.class)
      self$sample.size = sample.size
      self$type = type
      private$sample.x = function(size){
        list(X1 = private$sampler$sample(size), X2 = private$sampler$sample(size))
      }
    }
  )
)




