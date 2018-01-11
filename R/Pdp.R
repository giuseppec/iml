


pdp  = function(object, X, feature, grid.size = 10, sample.size=100, class=NULL, multi.class=FALSE, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, multi.class = multi.class, ...)
  
  PDP$new(predictor = pred, sampler = samp, feature = feature, grid.size = grid.size, 
    sample.size = sample.size)$run()
}

# TODO: Allow empty grid size, where grid points are drawn from X. 
PDP = R6Class('PDP', 
  inherit = Experiment,
  public = list(
    grid.size = NULL, 
    feature.index = NULL, 
    feature.names = NULL,
    n.features = NULL, 
    feature.type= NULL,
    aggregate = function(){
      results = self$X.design[self$feature.index]
      
      if(self$predictor$multi.class){
        y.hat.names = colnames(private$Q.results)
        results = cbind(results, private$Q.results)
        results = tidyr::gather(results, key = "class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        results['y.hat']= private$Q.results
        results['class.name'] = 1
      }
      results = results %>% 
        group_by_at(self$feature.names) %>% 
        group_by(class.name, add = TRUE) %>%
        summarise(y.hat = mean(y.hat)) %>% data.frame
      results 
    },
    intervene = function(){
      grid = private$get.1D.grid(1)
      
      private$X.design.ids = rep(1:nrow(self$X.sample), times = length(grid))
      X.design = self$X.sample[private$X.design.ids,]
      X.design[self$feature.index[1]] = rep(grid, each = nrow(self$X.sample))
      
      if(self$n.features == 2) {
        grid2 = private$get.1D.grid(2)
        private$X.design.ids = rep(private$X.design.ids, times = length(grid))
        X.design2 = X.design[rep(1:nrow(X.design), times = length(grid)), ]
        X.design2[self$feature.index[2]] = rep(grid, each = nrow(X.design))
        return(X.design2)
      }
      X.design
    }, 
    initialize = function(predictor, sampler, feature, grid.size, sample.size){
      assert_numeric(feature, lower=1, upper=ncol(X), min.len=1, max.len=2)
      if(length(feature)==2) assert_false(feature[1] == feature[2])
      #if(predictor$multi.class) stop('partial dependence plot does not support multi class yet')
      super$initialize(predictor, sampler)
      self$sample.size = sample.size
      private$set.feature(feature)
      private$set.grid.size(grid.size)
      private$grid.size.original = grid.size
    }
  ), 
  private = list(
    X.design.ids = NULL, 
    grid.size.original = NULL,
    set.feature = function(feature.index){
      self$feature.index = feature.index
      self$n.features = length(feature.index)
      self$feature.type = self$sampler$feature.types[self$feature.index]
      self$feature.names = self$sampler$feature.names[feature.index]
    },
    generate.plot = function(){
      if(self$n.features == 1){
        p = ggplot(private$results, mapping = aes_string(x = self$feature.names,"y.hat"))
        if(self$feature.type == 'numerical') p = p + geom_path() 
        else if (self$feature.type == 'categorical') p = p + geom_point()
      } else if (self$n.features == 2){
        if(all(self$feature.type %in% 'numerical') | all(self$feature.type %in% 'categorical')) {
          p = ggplot(private$results) + 
            geom_tile(aes_string(x = self$feature.names[1], 
              y = self$feature.names[2], 
              fill = "y.hat"))
        } else {
          categorical.feature = self$feature.names[self$feature.type=='categorical']
          numerical.feature = setdiff(self$feature.names, categorical.feature)
          p = ggplot(private$results) + 
            geom_line(aes_string(x = numerical.feature, y = "y.hat", 
              group = categorical.feature, color = categorical.feature))
        }
      }
      if(self$predictor$multi.class){
        p + facet_wrap("class.name")
      } else {
        p
      }
    }, 
    set.grid.size = function(size){
      self$grid.size = numeric(length=self$n.features)
      names(self$grid.size) = self$sampler$feature.names[self$feature.index]
      private$set.grid.size.single(size, 1)
      if(self$n.features > 1) private$set.grid.size.single(size, 2)
    }, 
    set.grid.size.single = function(size, feature.number){
      self$grid.size[feature.number] = ifelse(self$feature.type[feature.number] == 'numerical', 
        size, unique(self$X[[self$feature.index[feature.number]]]))
    }, 
    get.1D.grid = function(feature.number){
      if(self$feature.type[feature.number] == 'numerical'){
        grid = seq(from = min(self$X.sample[self$feature.index[feature.number]]), 
          to = max(self$X.sample[self$feature.index[feature.number]]), 
          length.out = self$grid.size[feature.number])
      } else if(self$feature.type[feature.number] == 'categorical') {
        grid = unique(self$X[[self$feature.index[feature.number]]])
      }
    }
  ), 
  active = list(
    feature = function(feature){
      private$flush()
      private$set.feature(feature)
      private$set.grid.size(private$grid.size.original)
    }
  )
)

