#' Decision tree surrogate model
#' 
#' \code{TreeSurrogate} fits a decision tree on the predictions of a prediction model.
#' 
#' @format \code{\link{R6Class}} object.
#' @name TreeSurrogate
#' @section Usage:
#' \preformatted{
#' tree = TreeSurrogate$new(predictor, maxdepth = 2, tree.args = NULL)
#' 
#' plot(tree)
#' predict(tree, newdata)
#' tree$results
#' print(tree)
#' }
#' 
#' @section Arguments:
#' 
#' For TreeSurrogate$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{maxdepth: }{(`numeric(1)`)\cr The maximum depth of the tree. Default is 2.}
#' \item{tree.args: }{(named list)\cr Further arguments for \code{ctree}.}
#' }
#' 
#' @section Details:  
#' A conditional inference tree is fitted on the predicted \eqn{\hat{y}} from the machine learning model and the data.
#' The \code{partykit} package and function are used to fit the tree. 
#' By default a tree of maximum depth of 2 is fitted to improve interpretability.
#' 
#' To learn more about global surrogate models, read the Interpretable Machine Learning book: 
#' \url{https://christophm.github.io/interpretable-ml-book/global.html}
#' 
#' @section Fields:
#' \describe{
#' \item{maxdepth: }{(`numeric(1)`)\cr The maximum tree depth.}
#' \item{predictor: }{(Predictor)\cr The prediction model that was analysed.}
#' \item{r.squared: }{(`numeric(1|n.classes)`)\cr R squared measures how well the decision tree approximates the underlying model. 
#' It is calculated as 1 - (variance of prediction differences / variance of black box model predictions).
#' For the multi-class case, r.squared contains one measure per class.}
#' \item{results: }{(data.frame)\cr Data.frame with sampled feature X together with the leaf node information (columns .node and .path) 
#' and the predicted \eqn{\hat{y}} for tree and machine learning model (columns starting with .y.hat).}
#' \item{tree: }{(party)\cr The fitted tree. See also \link[partykit]{ctree}.}
#' }
#'  
#' @section Methods:
#' \describe{
#' \item{plot()}{method to plot the leaf nodes of the surrogate decision tree. See \link{plot.TreeSurrogate}.}
#' \item{predict()}{method to predict new data with the tree. See also \link{predict.TreeSurrogate}}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' @references 
#' Craven, M., & Shavlik, J. W. (1996). Extracting tree-structured representations of trained networks. In Advances in neural information processing systems (pp. 24-30).
#' @examples 
#' if (require("randomForest")) {
#' # Fit a Random Forest on the Boston housing data set
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' # Create a model object
#' mod = Predictor$new(rf, data = Boston[-which(names(Boston) == "medv")]) 
#' 
#' # Fit a decision tree as a surrogate for the whole random forest
#' dt = TreeSurrogate$new(mod)
#' 
#' # Plot the resulting leaf nodes
#' plot(dt) 
#' 
#' # Use the tree to predict new data
#' predict(dt, Boston[1:10,])
#' 
#' # Extract the results
#' dat = dt$results
#' head(dat)
#' 
#' 
#' # It also works for classification
#' rf = randomForest(Species ~ ., data = iris, ntree = 50)
#' X = iris[-which(names(iris) == "Species")]
#' mod = Predictor$new(rf, data = X, type = "prob")
#' 
#' # Fit a decision tree as a surrogate for the whole random forest
#' dt = TreeSurrogate$new(mod, maxdepth=2)
#'
#' # Plot the resulting leaf nodes
#' plot(dt) 
#' 
#' # If you want to visualise the tree directly:
#' plot(dt$tree)
#' 
#' # Use the tree to predict new data
#' set.seed(42)
#' iris.sample = X[sample(1:nrow(X), 10),]
#' predict(dt, iris.sample)
#' predict(dt, iris.sample, type = "class")
#' 
#' # Extract the dataset
#' dat = dt$results
#' head(dat)
#' }
#' @seealso 
#' \link{predict.TreeSurrogate}
#' \link{plot.TreeSurrogate}
#' 
#' For the tree implementation
#' \link[partykit]{ctree}
#' @export
#' @import partykit
NULL

#' @export
TreeSurrogate = R6::R6Class("TreeSurrogate",
  inherit = InterpretationMethod,
  public = list(
    # The fitted tree
    tree = NULL,
    # Maximal depth as set by the user
    maxdepth = NULL,
    r.squared = NULL,
    initialize = function(predictor, maxdepth = 2, tree.args = NULL) {
      super$initialize(predictor)
      private$tree.args = tree.args
      self$maxdepth = maxdepth
      private$run()
    }, 
    predict = function(newdata, type = "prob", ...) {
      assert_choice(type, c("prob", "class"))
      newdata = private$match_cols(newdata)
      res = data.frame(predict(self$tree, newdata = newdata, type = "response", ...))
      if (private$multiClass) {
        if (type == "class") {
          res = data.frame(.class = colnames(res)[apply(res, 1, which.max)])
        }
      } else {
        res = data.frame(.y.hat = predict(self$tree, newdata = newdata, ...))
      }
      res
    }
  ), 
  private = list(
    tree.args = NULL,
    # Only relevant in multiClass case
    tree.predict.colnames = NULL,
    # Only relevant in multiClass case
    object.predict.colnames = NULL,
    intervene = function() private$dataSample,
    match_cols = function(newdata) self$predictor$data$match_cols(data.frame(newdata)),
    aggregate = function() {
      y.hat = private$qResults
      if (private$multiClass) {
        classes = colnames(y.hat)
        form = formula(sprintf("%s ~ .", paste(classes, collapse = "+")))       
      } else {
        y.hat = unlist(y.hat[1])
        form = y.hat ~ .
      }
      dat = cbind(y.hat, private$dataDesign)
      tree.args = c(list(formula = form, data = dat, maxdepth = self$maxdepth), private$tree.args)
      self$tree = do.call(partykit::ctree, tree.args)
      result = data.frame(.node = predict(self$tree, type = "node"), 
        .path = pathpred(self$tree))
      if (private$multiClass) {
        outcome = private$qResults
        colnames(outcome) = paste(".y.hat.", colnames(outcome), sep="")
        private$object.predict.colnames = colnames(outcome)
        
        # result = gather(result, key = ".class", value = ".y.hat", one_of(cnames))
        .y.hat.tree = self$predict(private$dataDesign, type = "prob")
        colnames(.y.hat.tree) = paste(".y.hat.tree.", colnames(.y.hat.tree), sep="")
        private$tree.predict.colnames = colnames(.y.hat.tree)
        self$r.squared = private$compute_r2(.y.hat.tree, outcome)
        #.y.hat.tree = gather(.y.hat.tree, ".class.tree", ".y.hat.tree")
        result = cbind(result, outcome, .y.hat.tree)
      } else {
        result$.y.hat = private$qResults[[1]]
        result$.y.hat.tree = self$predict(private$dataDesign)[[1]]
        self$r.squared = private$compute_r2(result$.y.hat.tree, result$.y.hat)
      }
      design = private$dataDesign
      rownames(design) = NULL
      cbind(design, result)
    }, 
    generatePlot = function() {
      p = ggplot(self$results) + 
        geom_boxplot(aes(y = .y.hat, x = "")) + 
        scale_x_discrete("") + 
        facet_wrap(".path") + 
        scale_y_continuous(expression(hat(y)))
      if (private$multiClass) {
        plotData = self$results
        # max class for model
        plotData$.class = private$object.predict.colnames[apply(plotData[private$object.predict.colnames], 1, which.max)]
        plotData$.class = gsub(".y.hat.", "", plotData$.class)
        plotData = plotData[setdiff(names(plotData), private$object.predict.colnames)]
        p = ggplot(plotData) + 
          geom_bar(aes(x = .class)) + 
          facet_wrap(".path")
      }
      p
    }, 
    compute_r2 = function(predict.tree, predict.model) {
      r.squared = function(pred.tree, pred.mod) {
        SST = var(pred.mod)
        SSE = var(pred.tree - pred.mod)
        1 - SSE/SST
      }
      if (private$multiClass) {
       sapply(1:ncol(predict.tree), function(ind) r.squared(predict.tree[ind], predict.model[ind]))
      } else {
        r.squared(predict.tree, predict.model)
      }
      
    }
  )
)



#' Predict Tree Surrogate
#' 
#' Predict the response for newdata of a TreeSurrogate object.
#' 
#' This function makes the TreeSurrogate object call 
#' its iternal object$predict() method. 
#' @param object The surrogate tree. A TreeSurrogate R6 object
#' @param newdata A data.frame for which to predict
#' @param type Either "prob" or "class". Ignored if the surrogate tree does regression. 
#' @param ... Further argumets for \code{predict_party}
#' @return A data.frame with the predicted outcome. 
#' In case of regression it is the predicted \eqn{\hat{y}}. 
#' In case of classification it is either the class probabilities (for type "prob") or the class label (type "class")
#' @seealso 
#' \link{TreeSurrogate}
#' @importFrom stats predict
#' @export
#' @examples 
#' if (require("randomForest")) {
#' # Fit a Random Forest on the Boston housing data set
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' # Create a model object
#' mod = Predictor$new(rf, data = Boston[-which(names(Boston) == "medv")]) 
#' 
#' # Fit a decision tree as a surrogate for the whole random forest
#' dt = TreeSurrogate$new(mod)
#' 
#' # Plot the resulting leaf nodes
#' predict(dt, newdata = Boston)
#' } 
predict.TreeSurrogate = function(object, newdata, type = "prob", ...) {
  object$predict(newdata = newdata, type, ...)
}


#' Plot Tree Surrogate
#' 
#' Plot the response for newdata of a TreeSurrogate object.
#' Each plot facet is one leaf node and visualises the distribution of the \eqn{\hat{y}}
#' from the machine learning model. 
#' 
#' @param object A TreeSurrogate R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{TreeSurrogate}
#' @examples 
#' if (require("randomForest")) {
#' # Fit a Random Forest on the Boston housing data set
#' data("Boston", package  = "MASS")
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' # Create a model object
#' mod = Predictor$new(rf, data = Boston[-which(names(Boston) == "medv")]) 
#' 
#' # Fit a decision tree as a surrogate for the whole random forest
#' dt = TreeSurrogate$new(mod)
#' 
#' # Plot the resulting leaf nodes
#' plot(dt) 
#' }
plot.TreeSurrogate = function(object) {
  object$plot()
}

