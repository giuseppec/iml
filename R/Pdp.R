


pdp  = function(f, X, feature, grid.size=10, sample.size=100){
  PDP$new(f=f, X=X, feature = feature, grid.size = grid.size, sample.size = sample.size)
}


# TODO: Allow categorical features
# TODO: Allow multiclass
PDP = R6Class('PDP', 
  inherit = Experiment,
  public = list(
    grid.size = NULL, 
    feature.index = NULL, 
    feature.type= NULL,
    aggregate = function(){
      results = self$X.design
      results['y.hat']= private$Q.results
      results %>% 
        group_by_at(self$feature.index) %>% 
        summarise(y.hat = mean(y.hat))
    },
    intervene = function(){
      X.design = data.frame(data.table::rbindlist(lapply(1:self$grid.size, function(x){self$X.sample})))
      if(self$feature.type == 'numerical'){
        grid = seq(from = min(self$X.sample[self$feature.index]), 
          to = max(self$X.sample[self$feature.index]), length.out = self$grid.size)
      } else if(self$feature.type == 'categorical') {
        grid = unique(self$X[[self$feature.index]])
      }
      X.design[self$feature.index] = rep(grid, each = nrow(self$X.sample))
      X.design
    }, 
    initialize = function(f, X, feature, grid.size, sample.size){
      super$initialize(f, X)
      self$sample.size = sample.size
      self$feature.index = feature
      self$feature.type = get.feature.type(self$X[[self$feature.index]])
      private$grid.size.numerical = grid.size
      private$set.grid.size()
    }
  ), 
  privat = list(
    generate.plot = function(){
      p = ggplot(private$results, mapping = aes_string(x = names(private$results)[1], y = names(private$results[2]))) 
      if(self$feature.type == 'numerical') p + geom_path() 
      else if (self$feature.type == 'categorical') p + geom_point()
    }, 
    grid.size.numerical = NULL, 
    set.grid.size = function(){
      self$grid.size = ifelse(self$feature.type == 'factor', 
        length(levels(X[[self$feature.index]])), private$grid.size.numerical)
    }
  ), 
  active = list(
    feature = function(feature){
      private$flush()
      self$feature.index = feature
      self$feature.type = get.feature.type(self$X[[self$feature.index]])
      private$set.grid.size()
    }
  )
)

