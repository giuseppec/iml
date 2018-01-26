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
#' @param x.interest data.frame with a single row for the instance to be explained.
#' @param sample.size Number of samples to be drawn to estimate the Shapley value. The higher the more accurate the estimations.
#' 
#' @return 
#' A PDP object (R6). Its methods and variables can be accessed with the \code{$}-operator:
#' \item{sample.size}{The number of times coalitions/marginals are sampled from data X. The higher the more accurate the explanations become.}
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
#' # TODO
shapley = function(object, X, x.interest, sample.size=100, class=NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  Shapley$new(predictor = pred, sampler = samp, x.interest=x.interest, sample.size=sample.size)$run()
}

## TODO: instead having an outer loop over features,
##       loop over features within each coalition, like the ApproShapley algorithms.
##       Additionally make sure to not calculate things twice, because  it's always
##       the difference of coalition of features with and without feature j.
##       see Song, E., & Nelson, B. L. (2016). Shapley Effects for Global Sensitivity Analysis : Theory and Computation ∗, 4, 1060–1083.
## TODO: Implement multi.class
Shapley = R6::R6Class('Shapley', 
  inherit = Experiment,
  public = list(
    x.interest = NULL,
    initialize = function(predictor, sampler, x.interest, sample.size){
      checkmate::assert_data_frame(x.interest)
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$x.interest = x.interest
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
      n.features = ncol(private$X.sample)
      # The intervention
      runs = lapply(1:self$sample.size, function(m){
        # randomly order features
        new.feature.order = sample(1:n.features)
        # randomly choose sample instance from X
        sample.instance.shuffled = private$X.sample[sample(1:nrow(private$X.sample), 1), new.feature.order]
        x.interest.shuffled = self$x.interest[new.feature.order]
        
        lapply(1:n.features, function(k){
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
    generate.plot = function(){
      p = ggplot(private$results) + geom_point(aes(y = feature, x = phi))
      if(private$multi.class) p = p + facet_wrap("class")
      p
    }
  ),
  active = list(
    x = function(x.interest){
      self$x.interest = x.interest
      private$flush()
      self$run()
      self
    }
  )
)






