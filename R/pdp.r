#' Partial Dependence
#' 
#' @description 
#' \code{pdp()} computes partial dependence functions of prediction models. 
#' 
#' @details
#' Machine learning model try to learn the relationship \eqn{y = f(X)}. We can't visualize 
#' the learned \eqn{\hat{f}} directly for high-dimensional X. 
#' But we can split it into parts:
#' \deqn{f(X) = f_1(X_1) + \ldots + f_p(X_p) + f_{1, 2}(X_1, X_2) + \ldots + f_{p-1, p}(X_{p-1}, X_p) + \ldots + f_{1\ldotsp}(X_1\ldotsX_p)}, 
#' 
#' And we can isolate the partial dependence of \eqn{y} on a single \eqn{X_j}: \eqn{f_j(X_j)} and plot it. 
#' We can even do this for higher dimensions, but a maximum of 2 features makes sense: \eqn{f_j(X_j) + f_k(X_k) + f_{jk}(X_{jk})}
#' 
#' The partial dependence for a feature \eqn{X_j} is estimated by spanning a grid over the feature space. 
#' For each value of the grid, we replace in the whole dataset the \eqn{X_j}-value with the grid value, 
#' predict the outcomes \eqn{\hat{y}} with the machine learning models and average the predictions. 
#' This generate one point of the partial dependence curve. After doing this for the whole grid, 
#' the outcome is a curve (or 2D plane), that then can be plotted. 
#' 
#' To learn more about partial dependence plot, read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/pdp.html
#' 
#' 
#' @seealso 
#' \link{plot.PDP}
#' 
#' \code{\link{ice}} for individual conditional expectation plots. 
#' 
#' @references 
#' Friedman, J.H. 2001. “Greedy Function Approximation: A Gradient Boosting Machine.” Annals of Statistics 29: 1189–1232.
#' @return 
#' A PDP object (R6). Its methods and variables can be accessed with the \code{$}-operator:
#' \item{feature}{The feature names for which the partial dependence was computed.}
#' \item{feature.type}{The detected types of the features, either "categorical" or "numerical".}
#' \item{feature.index}{The index of the features for which the partial dependence was computed.}
#' \item{grid.size}{The size of the grid.}
#' \item{sample.size}{The number of instances sampled from data X.}
#' \item{feature(index)}{method to get/set feature(s) (by index) fpr  which to compute pdp. See examples for usage.}
#' \item{data()}{method to extract the results of the partial dependence plot. 
#' Returns a data.frame with the grid of feature of interest and the predicted \eqn{\hat{y}}. 
#' Can be used for creating custom partial dependence plots.}
#' \item{plot()}{method to plot the partial dependence function. See \link{plot.PDP}}
#' @template args_internal_methods
#' @template arg_grid.size 
#' 
#' @importFrom tidyr gather
#' @importFrom dplyr one_of group_by group_by_at summarise
#' @import ggplot2
#' @export
#' @examples
#' 
#' # We train a random forest on the Boston dataset:
#' library("randomForest")
#' data("Boston", package  = "MASS")
#' mod = randomForest(medv ~ ., data = Boston, ntree = 50)
#' 
#' # Compute the partial dependence for the first feature
#' pdp.obj = pdp(mod, Boston, feature = 1)
#' 
#' # Plot the results directly
#' plot(pdp.obj)
#' 
#' 
#' # Since the result is a ggplot object, you can extend it: 
#' library("ggplot2")
#' plot(pdp.obj) + theme_bw()
#' 
#' # If you want to do your own thing, just extract the data: 
#' pdp.dat = pdp.obj$data()
#' head(pdp.dat)
#' 
#' # You can reuse the pdp object for other features: 
#' pdp.obj$feature = 2
#' plot(pdp.obj)
#' 
#' # Partial dependence plots support up to two features: 
#' pdp.obj = pdp(mod, Boston, feature = c(1,2))  
#' 
#' # Partial dependence plots also works with multiclass classification
#' library("randomForest")
#' mod = randomForest(Species ~ ., data= iris, ntree=50)
#' 
#' # For some models we have to specify additional arguments for the predict function
#' plot(pdp(mod, iris, feature = 1, predict.args = list(type = 'prob')))
#' 
#' # For multiclass classification models, you can choose to only show one class:
#' plot(pdp(mod, iris, feature = 1, class = 1, predict.args = list(type = 'prob')))
#' 
#' # Partial dependence plots support up to two features: 
#' pdp.obj = pdp(mod, iris, feature = c(1,3), predict.args = list(type = 'prob'))
#' pdp.obj$plot()  
#' 
pdp  = function(object, X, feature, grid.size = 10, class=NULL,  ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  PDP$new(predictor = pred, sampler = samp, feature = feature, grid.size = grid.size)$run()
}


#' Partial dependence plot
#' 
#' plot.PDP() plots a line for a single feature and a tile plot for 2 features.
#' 
#' For examples see \link{pdp}
#' @param object The partial dependence. A PDP R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{pdp}
plot.PDP = function(object){
  object$plot()
}

# TODO: Allow empty grid size, where grid points are drawn from X. 
PDP = R6::R6Class('partial dependence plot', 
  inherit = Experiment,
  public = list(
    grid.size = NULL, 
    feature.index = NULL, 
    feature.names = NULL,
    n.features = NULL, 
    feature.type= NULL,
    initialize = function(predictor, sampler, feature, grid.size){
      checkmate::assert_numeric(feature, lower = 1, upper = sampler$n.features, min.len = 1, max.len = 2)
      checkmate::assert_numeric(grid.size, min.len = 1, max.len = length(feature))
      if(length(feature) == 2) checkmate::assert_false(feature[1] == feature[2])
      super$initialize(predictor, sampler)
      private$set.feature(feature)
      private$set.grid.size(grid.size)
      private$grid.size.original = grid.size
      private$sample.x = private$sampler$get.x
    }
  ), 
  private = list(
    aggregate = function(){
      results = private$X.design[self$feature.index]
      
      if(ncol(private$Q.results) > 1){
        y.hat.names = colnames(private$Q.results)
        results = cbind(results, private$Q.results)
        results = gather(results, key = "..class.name", value = "y.hat", one_of(y.hat.names))
      } else {
        results['y.hat']= private$Q.results
        results['..class.name'] = private$predictor$class
      }
      results.grouped = group_by_at(results, self$feature.names)
      if('..class.name' %in% colnames(results)) results.grouped = group_by(results.grouped, ..class.name, add = TRUE)
      results = data.frame(summarise(results.grouped, y.hat = mean(y.hat)))
      results 
    },
    intervene = function(){
      grid = get.1D.grid(private$X.sample[[self$feature.index[1]]], self$feature.type[1], self$grid.size[1])
      
      private$X.design.ids = rep(1:nrow(private$X.sample), times = length(grid))
      X.design = private$X.sample[private$X.design.ids,]
      X.design[self$feature.index[1]] = rep(grid, each = nrow(private$X.sample))
      
      if(self$n.features == 2) {
        grid2 = get.1D.grid(private$X.sample[[self$feature.index[2]]], self$feature.type[2], self$grid.size[2])
        private$X.design.ids = rep(private$X.design.ids, times = length(grid2))
        X.design2 = X.design[rep(1:nrow(X.design), times = length(grid2)), ]
        X.design2[self$feature.index[2]] = rep(grid2, each = nrow(X.design))
        return(X.design2)
      }
      X.design
    }, 
    X.design.ids = NULL, 
    grid.size.original = NULL,
    set.feature = function(feature.index){
      self$feature.index = feature.index
      self$n.features = length(feature.index)
      self$feature.type = private$sampler$feature.types[self$feature.index]
      self$feature.names = private$sampler$feature.names[feature.index]
    },
    print.parameters = function(){
      cat('features:', paste(sprintf("%s[%s]", self$feature.names, self$feature.type), collapse = ", "))
      cat('\ngrid size:', paste(self$grid.size, collapse = "x"))
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
      if(ncol(private$Q.results) > 1){
        p = p + facet_wrap("..class.name")
      } 
      p
    }, 
    set.grid.size = function(size){
      self$grid.size = numeric(length=self$n.features)
      names(self$grid.size) = private$sampler$feature.names[self$feature.index]
      private$set.grid.size.single(size[1], 1)
      if(self$n.features > 1) {
        if(length(size) == 1) {
          # If user only provided 1D grid size
          private$set.grid.size.single(size[1], 2)
        } else {
          # If user provided 2D grid.size
          private$set.grid.size.single(size[2], 2)
        }
      }
    }, 
    set.grid.size.single = function(size, feature.number){
      self$grid.size[feature.number] = ifelse(self$feature.type[feature.number] == 'numerical', 
        size, length(unique(private$sampler$get.x()[[self$feature.index[feature.number]]])))
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


## TODO:document
## TODO: Write test

get.1D.grid = function(feature, feature.type, grid.size){
  checkmate::assert_vector(feature)
  if(feature.type == 'numerical'){
    grid = seq(from = min(feature), 
      to = max(feature), 
      length.out = grid.size)
  } else if(feature.type == 'categorical') {
    grid = unique(feature)
  }
}







