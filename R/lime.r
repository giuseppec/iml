#' LIME
#' 
#' @description 
#' \code{lime()} fits a locally weighted linear regression model (logistic for classification) to explain a single machine learning prediction.
#' 
#' @details 
#' Data points are sampled and weighted by their proximity to the instance to be explained. 
#' A weighted glm is fitted with the machine learning model prediction as target. 
#' L1-regularisation is used to make the results sparse. 
#' The resulting model can be seen as a surrogate for the machine learning model, which is only valid for that one point.
#' Categorical features are binarized, depending on the category of the instance to be explained: 1 if the category is the same, 0 otherwise.
#' 
#' Differences to the original LIME implementation: 
#' \itemize{
#' \item Distance measure: Uses gower proximity (= 1 - gower distance) instead of a kernel based on the Euclidean distance. Has the advantage to have a meaningful neighbourhood and no kernel width to tune.
#' \item Sampling: Sample from X instead of from normal distributions. 
#' Has the advantage to follow the original data distribution. 
#' \item Visualisation: Plots effects instead of betas. Is the same for binary features, but makes a difference for numerical features. 
#' For numerical features, plotting the betas makes no sense, 
#' because a negative beta might still increase the prediction when the feature value is also negative.
#' }
#' To learn more about local models, read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/lime.html
#' 
#' @references 
#' Ribeiro, M. T., Singh, S., & Guestrin, C. (2016). “Why Should I Trust You?”: Explaining the Predictions of Any Classifier. Retrieved from http://arxiv.org/abs/1602.04938
#' 
#' @seealso 
#' \code{\link{plot.Lime}} and \code{\link{predict.Lime}}
#' 
#' \code{\link{shapley}} can also be used to explain single predictions
#' 
#' \code{\link[lime]{lime}}, the original implementation
#' @export
#' @template args_experiment_wrap
#' @template arg_sample.size
#' @template args_x.interest
#' @param k the (maximum) number of features to be used for the surrogate model
#' @param x.interest data.frame with the instance to be explained
#' @return 
#' A Lime object (R6). Its methods and variables can be accessed with the \code{$}-operator:
#' \item{sample.size}{The number of samples from data X. The higher the more accurate the explanations become.}
#' \item{model}{the glmnet object.}
#' \item{best.fit.index}{the index of the best glmnet fit}
#' \item{k}{The number of features as set by the user.}
#' \item{x.interest}{method to get/set the instance. See examples for usage.}
#' \item{data()}{method to extract the results of the local feature effects 
#' Returns a data.frame with the feature names (\code{feature}) and contributions to the prediction}
#' \item{plot()}{method to plot the Lime feature effects. See \link{plot.Lime}}
#' \item{predict()}{method to predict new data with the local model See also \link{predict.Lime}}
#' @template args_internal_methods
#' @examples 
#' # First we fit a machine learning model on the Boston housing data
#' library("randomForest")
#' data("Boston", package  = "MASS")
#' mod = randomForest(medv ~ ., data = Boston, ntree = 50)
#' X = Boston[-which(names(Boston) == "medv")]
#' 
#' # Then we explain the first instance of the dataset with the lime() method:
#' x.interest = X[1,]
#' lemon = lime(mod, X, x.interest = x.interest, k = 2)
#' lemon
#' 
#' # Look at the results in a table
#' lemon$data()
#' # Or as a plot
#' plot(lemon)
#'
#' # Reuse the object with a new instance to explain
#' lemon$x.interest = X[2,]
#' plot(lemon)
#'   
#' # lime() also works with multiclass classification
#' library("randomForest")
#' mod = randomForest(Species ~ ., data= iris, ntree=50)
#' X = iris[-which(names(iris) == 'Species')]
#' 
#' # Then we explain the first instance of the dataset with the lime() method:
#' lemon = lime(mod, X, x.interest = X[1,], predict.args = list(type='prob'), k = 3)
#' lemon$data()
#' plot(lemon) 
#' 
#'# You can also focus on one class
#' lemon = lime(mod, X, x.interest = X[1,], class = 2, predict.args = list(type='prob'), k = 2)
#' lemon$data()
#' plot(lemon) 
#' 
lime = function(object, X, sample.size=100, k = 3, x.interest, class = NULL, ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  LIME$new(predictor = pred, sampler = samp, sample.size=sample.size, k = k, x.interest = x.interest)$run()
}



#' LIME prediction
#' 
#' Predict the response for newdata with the LIME model.
#' 
#' This function makes the LIME object call 
#' its iternal object$predict() method. 
#' For examples see \link{lime}
#' @param object A LIME R6 object
#' @param newdata A data.frame for which to predict
#' @return A data.frame with the predicted outcome. 
#' @seealso 
#' \link{lime}
#' @export
predict.LIME = function(object, newdata = NULL, ...){
  object$predict(newdata = newdata, ...)
}

#' LIME plot
#' 
#' plot.LIME() plots the feature effects of the LIME model.
#' 
#' For examples see \link{lime}
#' @param object  A LIME R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{lime}
plot.LIME = function(object){
  object$plot()
}

## TODO: Add binarization option for numerical features
# Differences to original LIME: 
# - Sample directly from data, not from weird normal distribution per feature
# - Best k features are chosen by Lasso path
LIME = R6::R6Class('LIME', 
  inherit = Experiment,
  public = list(
    x = NULL, 
    k = NULL,
    model = NULL,
    best.fit.index = NULL,
    sample.size = NULL,
    predict = function(newdata = NULL, ...){
      if(is.null(newdata)) newdata = self$x
      X.recode = recode(newdata, self$x)
      prediction = predict(self$model, newx=as.matrix(X.recode))
      if(private$multi.class){
        data.frame(prediction[,,self$best.fit.index])
      } else {
        pred = prediction[,self$best.fit.index, drop=FALSE]
        colnames(pred) = NULL
        data.frame(prediction = pred)
      }
    },
    initialize = function(predictor, sampler, sample.size, k, x.interest, class, ...){
      checkmate::assert_number(k, lower = 1, upper = sampler$n.features)
      checkmate::assert_data_frame(x.interest)
      if(!require('glmnet')){stop('Please install glmnet.')}
      super$initialize(predictor = predictor, sampler = sampler)
      self$sample.size = sample.size
      self$k = k
      self$x = x.interest
      private$get.data = function(...) private$sampler$sample(n = self$sample.size, ...)
    }
  ),
  private = list(
    Q = function(pred) probs.to.labels(pred),
    best.index = NULL,
    aggregate = function(){
      X.recode = recode(private$X.design, self$x)
      x.scaled = recode(self$x, self$x)
      fam = ifelse(private$multi.class, 'multinomial', 'gaussian')
      self$model = glmnet(x = as.matrix(X.recode), y = unlist(private$Q.results[1]), 
        family = fam, w = private$weight.samples(), 
        intercept = TRUE, standardize = TRUE, type.multinomial = "grouped")
      res = self$model
      ## It can happen, that no n.vars matching k occurs
      if(any(res$df == self$k)){
        best.index = max(which(res$df == self$k))
      } else {
        best.index = max(which(res$df < self$k))
        warning("Had to choose a smaller k")
      }
      self$best.fit.index = best.index
      if(private$multi.class){
        class.results = lapply(res$beta, extract.glmnet.effects, 
          best.index = best.index, x.scaled = x.scaled, x.original = self$x)
        res = data.table::rbindlist(class.results)
        res$..class = rep(names(class.results), each = ncol(X.recode))
      } else {
        res = extract.glmnet.effects(res$beta, best.index, x.scaled, self$x)
      }
      res[res$beta != 0, ]
    },
    intervene = function(){private$X.sample}, 
    generate.plot = function(){
      p = ggplot(private$results) + geom_point(aes(y = feature.value, x = effect))
      if(private$multi.class) p = p + facet_wrap("..class")
      p
    },
    weight.samples = function(){
      require('gower')
      1 - gower_dist(private$X.design, self$x)
    }
  ),
  active = list(
    x.interest = function(x.interest){
      self$x = x.interest
      private$flush()
      self$run()
    }
  )
)







