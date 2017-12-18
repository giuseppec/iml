PDP = R6Class('PDP', 
  inherit = Experiment,
  public = list(
    grid.size = NULL, 
    feature.index = NULL,
    aggregate = function(){
      results = self$X.design
      results['y.hat']= private$Q.results
      results %>% 
        group_by_at(self$feature.index) %>% 
        summarise(y.hat = mean(y.hat))
    },
    sampler = function(){
      self$X[sample(1:nrow(self$X), size = self$sample.size, replace = TRUE), ]
    },
    intervene = function(){
      X.design = data.frame(data.table::rbindlist(lapply(1:self$grid.size, function(x){self$X.sample})))
      grid = seq(from = min(self$X.sample[self$feature.index]), 
        to = max(self$X.sample[self$feature.index]), length.out = self$grid.size)
      X.design[self$feature.index] = rep(grid, each = nrow(self$X.sample))
      X.design
    }, 
    initialize = function(f, X, feature.index, grid.size=10, sample.size=100){
      super$initialize(f, X)
      self$sample.size = sample.size
      self$grid.size = grid.size
      self$feature.index = feature.index
    }
  ), 
  privat = list(
    generate.plot = function(){
      ggplot(private$results) + 
        geom_line(aes_string(x = names(private$results)[1], y = names(private$results[2])))
    }
  )
)

