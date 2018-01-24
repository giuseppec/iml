#' Decision tree surrogate model
#' 
#' @description 
#' Fit a decision tree model on another machine learning models predictions to replace or explain the other model.
#' 
#' @details  
#' A CART tree is fitted on the predicted \eqn{\hat{y}} from the machine learning model and the data \eqn{X}.
#' The \code{rpart} package and function are used to fit the tree. 
#' By default a tree of maximum depth of 2 is fitted to improve interpretability.
#' 
#' @return 
#' A decision tree surrogate object
#' @template args_experiment_wrap
#' @template arg_sample.size
#' @return
#' \itemize{
#' \item \code{obj$predict(newdata)} can be used to get predictions from the decision tree
#' }
#' @examples 
#' library("randomForest")
#' data("Boston", package  = "MASS")
#' mod = randomForest(medv ~ ., data = Boston, ntree = 50)
#' 
#' dt = tree.surrogate(mod, Boston, 200)
#' 
#' # Predict newdata
#' predict(dt, Boston[1,])
#' 
#' # 
#' 
#' @param tree.args A list with further arguments for rpart
#' @importFrom rpart rpart
#' @importFrom rpart path.rpart
#' @export
tree.surrogate = function(object, X, sample.size=100, class = NULL, tree.args = list(maxdepth=2), ...){
  samp = DataSampler$new(X)
  pred = prediction.model(object, class = class, ...)
  
  TreeSurrogate$new(predictor = pred, sampler = samp, sample.size = sample.size, tree.args = tree.args)$run()
}

#' @export
predict.TreeSurrogate = function(object, newdata, ...){
  object$predict(newdata = newdata, ...)
}

## Craven, M. W., & Shavlik, J. W. (1996).
## Extracting tree-structured representations of trained neural networks.
## Advances in Neural Information Processing Systems, 8, 24–30.
## Retrieved from citeseer.ist.psu.edu/craven96extracting.html
# TODO: Implement multi.class 
TreeSurrogate = R6::R6Class('TreeSurrogate',
  inherit = Experiment,
  public = list(
    summary = function(){
      self$run()
      summary(private$results)
    },
    initialize = function(predictor, sampler, sample.size, tree.args){
      super$initialize(predictor, sampler)
      self$sample.size = sample.size
      private$tree.args = tree.args
    }, 
    predict = function(newdata, type = 'prob'){
      assert_choice(type, c('prob', 'class'), null.ok = TRUE)
      if(private$multi.class){
        res = data.frame(predict(private$model, newdata = newdata, type = type[1]))
        if(type == 'class') colnames(res) = '..class'
        res
      } else {
        data.frame(..y.hat = predict(private$model, newdata = newdata))
      }
    }
  ), 
  private = list(
    model = NULL, 
    tree.args = NULL,
    intervene = function(){private$X.sample},
    aggregate = function(){
      y.hat = private$Q.results
      if(private$multi.class){
        classes = colnames(y.hat)
        y.hat  = classes[apply(y.hat, 1, which.max)]
      } else {
        y.hat = unlist(y.hat[1])
      }
      dat = cbind(y.hat = y.hat, private$X.design)
      tree.args = c(list(formula = y.hat ~ ., data = dat), private$tree.args)
      private$model = do.call(rpart::rpart, tree.args)
      result = data.frame(..node = rownames(private$model$frame)[private$model$where])
      path = rpart::path.rpart(private$model, nodes = unique(result$..node), print.it = FALSE)
      path = unlist(lapply(path, function(x) paste(x[2:length(x)], collapse = ' &\n ')))
      result$..path = path[as.character(result$..node)]
      if(private$multi.class){
        outcome = private$Q.results
        cnames = colnames(outcome)
        result = cbind(result, outcome)
        result = gather(result, key = "..class", value = "..y.hat", one_of(cnames))
        ..y.hat.tree = self$predict(private$X.design, type = 'prob')
        ..y.hat.tree = gather(..y.hat.tree, '..class.tree', '..y.hat.tree')
        result = cbind(result, ..y.hat.tree['..y.hat.tree'])
      } else {
        result$..y.hat = private$Q.results[[1]]
        result$..y.hat.tree = self$predict(private$X.design)
      }
      design = private$X.design
      rownames(design) = NULL
      cbind(design, result)
    }, 
    generate.plot = function(){
      p = ggplot(private$results) + 
        geom_boxplot(aes(y = ..y.hat, x = "")) + 
        facet_wrap("..path")
      if(private$multi.class){
        p = ggplot(private$results) + 
          geom_boxplot(aes(y = ..y.hat, x = ..class)) + 
          facet_wrap("..path")
      }
      p
    }
    
  )
)
