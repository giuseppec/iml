#' Explain predictions
#' 
#' @description 
#' shapley() computes feature contributions for single predictions with the Shapley value, an approach from cooperative game theory approach. 
#' The features values of an instance cooperate to achieve the prediction. 
#' shapley() fairly distributes the difference of the instance's prediction and the datasets average prediction among the features. 
#' A features contribution can be negative. 
#' 
#' @details
#' See TODO: BOOK REFERENCE
#' 
#' @seealso 
#' A different way to explain predictions: \link{lime}
#' @template args_experiment_wrap
#' @template args_x.interest
#' @param sample.size Number of samples to be drawn to estimate the Shapley value. The higher the more accurate the estimations.
#' 
#' @return 
#' A Shapley object (R6). Its methods and variables can be accessed with the \code{$}-operator:
#' \item{sample.size}{The number of times coalitions/marginals are sampled from data X. The higher the more accurate the explanations become.}
#' \item{x.interest}{data.frame with the instance of interest}
#' \item{y.hat.interest}{predicted value for instance of interest}
#' \item{y.hat.averate}{average predicted value for data \code{X}} 
#' \item{x}{method to get/set the instance. See examples for usage.}
#' \item{data()}{method to extract the results of the shapley estimations. 
#' Returns a data.frame with the feature names (\code{feature}) and contributions to the prediction (\code{phi})}
#' \item{plot()}{method to plot the Shapley value. See \link{plot.Shapley}}
#' @template args_internal_methods
#' @references 
#' Strumbelj, E., Kononenko, I., Štrumbelj, E., & Kononenko, I. (2014). Explaining prediction models and individual predictions with feature contributions. Knowledge and Information Systems, 41(3), 647–665. https://doi.org/10.1007/s10115-013-0679-x
#' @seealso 
#' \link{lime}
#' @export
#' @examples 
#' # First we fit a machine learning model on the Boston housing data
#' library("randomForest")
#' data("Boston", package  = "MASS")
#' mod = randomForest(medv ~ ., data = Boston, ntree = 50)
#' X = Boston[-which(names(Boston) == "medv")]
#' 
#' # Then we explain the first instance of the dataset with the shapley() method:
#' x.interest = X[1,]
#' shap = shapley(mod, X, x.interest = x.interest)
#' shap
#' 
#' # Look at the results in a table
#' shap$data()
#' # Or as a plot
#' plot(shap)
#' 
#' # shapley() also works with multiclass classification
#' library("randomForest")
#' mod = randomForest(Species ~ ., data= iris, ntree=50)
#' X = iris[-which(names(iris) == 'Species')]
#' 
#' # Then we explain the first instance of the dataset with the shapley() method:
#' shap = shapley(mod, X, x.interest = X[1,], predict.args = list(type='prob'))
#' shap$data()
#' plot(shap) 
#' 
#'# You can also focus on one class
#' shap = shapley(mod, X, x.interest = X[1,], class = 2, predict.args = list(type='prob'))
#' shap$data()
#' plot(shap) 
#' 
shapley = function(object, X, x.interest, sample.size=100, class=NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  Shapley$new(predictor = pred, sampler = samp, x.interest=x.interest, sample.size=sample.size)$run()
}


#' Shapley plot
#' 
#' plot.Shapley() plots the Shapley values - the contributions of feature values to the prediction. 
#' 
#' For examples see \link{shapley}
#' @param object  A Shapley R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{shapley}
plot.Shapley = function(object){
  object$plot()
}

Shapley = R6::R6Class('Shapley', 
  inherit = Experiment,
  public = list(
    x.interest = NULL,
    y.hat.interest = NULL,
    y.hat.average = NULL,
    sample.size = NULL,
    initialize = function(predictor, sampler, x.interest, sample.size){
      checkmate::assert_data_frame(x.interest)
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      private$set.x.interest(x.interest)
      private$get.data = function(...) private$sampler$sample(n = self$sample.size, ...)
    }
  ), 
  private = list(
    aggregate = function(){
      y.hat.with.k = private$Q.results[1:(nrow(private$Q.results)/2), , drop = FALSE]
      y.hat.without.k = private$Q.results[(nrow(private$Q.results)/2 + 1):nrow(private$Q.results), , drop = FALSE]
      y.hat.diff = y.hat.with.k - y.hat.without.k
      cnames = colnames(y.hat.diff)
      y.hat.diff = cbind(data.frame(feature = rep(colnames(private$X.design), times = self$sample.size)), 
        y.hat.diff)
      y.hat.diff = gather(y.hat.diff, key = "class", "value", one_of(cnames))
      y.hat.diff = y.hat.diff %>% group_by(feature, class) %>% summarise(phi = mean(value), phi.var = var(value))
      if(!private$multi.class) y.hat.diff$class = NULL
      y.hat.diff
    },
    intervene = function(){
      # The intervention
      runs = lapply(1:self$sample.size, function(m){
        # randomly order features
        new.feature.order = sample(1:private$sampler$n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled = private$X.sample[sample(1:nrow(private$X.sample), 1), new.feature.order]
        x.interest.shuffled = self$x.interest[new.feature.order]
        
        lapply(1:private$sampler$n.features, function(k){
          k.at.index = which(new.feature.order == k)
          instance.with.k = x.interest.shuffled
          if(k.at.index < ncol(self$x.interest)){
            instance.with.k[(k.at.index + 1):ncol(instance.with.k)] =
              sample.instance.shuffled[(k.at.index + 1):ncol(instance.with.k)]
          }
          instance.without.k = instance.with.k
          instance.without.k[k.at.index] = sample.instance.shuffled[k.at.index]
          cbind(instance.with.k[private$sampler$feature.names], instance.without.k[private$sampler$feature.names])
        }) %>% data.table::rbindlist()
        
      }) %>% data.table::rbindlist()
      dat.with.k = data.frame(runs[,1:(ncol(runs)/2)])
      dat.without.k = data.frame(runs[,(ncol(runs)/2 + 1):ncol(runs)])
      
      rbind(dat.with.k, dat.without.k)
    }, 
    set.x.interest = function(x.interest){
      self$x.interest = x.interest
      self$y.hat.interest = private$predict(x.interest)[1,]
      self$y.hat.average = colMeans(private$predict(private$sampler$get.x()))
    },
    generate.plot = function(){
      p = ggplot(private$results) + geom_point(aes(y = feature, x = phi))
      if(private$multi.class) p = p + facet_wrap("class")
      p
    },
    print.parameters = function(){
      cat(sprintf('Predicted value: %f, Average prediction: %f (diff = %f)', 
        self$y.hat.interest, self$y.hat.average, self$y.hat.interest - self$y.hat.average))
    }
  ),
  active = list(
    x = function(x.interest){
      private$flush()
      private$set.x.interest(x.interest)
      self$run()
      self
    }
  )
)






