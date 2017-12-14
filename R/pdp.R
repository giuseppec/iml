PDP = R6Class('PDP', 
  inherit = Experiment,
  public = list(
    n = NULL, 
    grid.size = NULL, 
    feature.index = NULL,
    X = NULL,
    aggregate = function(){
      self$X.design['y.hat']= self$Q.results
      self$X.design %>% group_by_at(self$feature.index) %>% summarise(y.hat = mean(y.hat))
    },
    sampler = function(){
      self$X[sample(1:nrow(self$X), size = self$n, replace = TRUE), ]
    },
    intervene = function(){
      X.design = data.frame(data.table::rbindlist(lapply(1:self$grid.size, function(x){self$X.sample})))
      grid = seq(from = min(self$X.sample[self$feature.index]), to = max(self$X.sample[self$feature.index]), length.out = self$grid.size)
      X.design[self$feature.index] = rep(grid, each = self$n)
      X.design
    }, 
    plot = function(){
      ggplot(private$results) + geom_line(aes_string(x = names(private$results)[1], y = names(private$results[2])))
    },

    initialize = function(f, X, feature.index, grid.size=10, n=100){
      self$name = 'PDP'
      self$grid.size = grid.size
      self$n = n
      self$feature.index = feature.index
      self$f = f
      self$X = X
    }
  )
)

