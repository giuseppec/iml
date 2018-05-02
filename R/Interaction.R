#' Interaction strength
#' 
#' \code{TreeSurrogate} fits a decision tree on the predictions of a prediction model.
#' 
#' @format \code{\link{R6Class}} object.
#' @name TreeSurrogate
#' @section Usage:
#' \preformatted{
#' tree = TreeSurrogate$new(predictor, maxdepth = 2, tree.args = NULL, run = TRUE)
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
#' \item{run: }{(`logical(1)`)\cr Should the Interpretation method be run?}
#' \item{tree.args: }{(named list)\cr Further arguments for \code{ctree}.}
#' }
#' 
#' @section Details:  
#' A conditional inference tree is fitted on the predicted \eqn{\hat{y}} from the machine learning model and the data.
#' The \code{partykit} package and function are used to fit the tree. 
#' By default a tree of maximum depth of 2 is fitted to improve interpretability.
#' 
#' @section Fields:
#' \describe{
#' \item{maxdepth: }{(`numeric(1)`)\cr The maximum tree depth.}
#' \item{predictor: }{(Predictor)\cr The prediction model that was analysed.}
#' \item{r.squared}{(`numeric(1|n.classes)`)\cr R squared measures how well the decision tree approximates the underlying model. 
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
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
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
#' predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
#' mod = Predictor$new(rf, data = X, predict.fun = predict.fun)
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
Interaction = R6::R6Class("Interaction",
  inherit = InterpretationMethod,
  public = list(
    # The fitted tree
    features = NULL,
    initialize = function(predictor, features, run = TRUE) {
      super$initialize(predictor)
      private$features = features
      # TODO: accept only 0,1 or 2 features:
      # 0 features: Compute interaction between 1 vs rest test statistics for all features
      # 1 feature: Compute interaction between 1 vs rest test statistics for chosen feature
      # 2 features: Compute interaction between 2 features.
      # TODO: create 3 Partial objects: for first, second and both features (run=FALSE)
      if(run) self$run()
    }, 
  ), 
  private = list(
    intervene = function() {
      # TODO: Super.intervene on all three Partial objects
    },
    aggregate = function() {
      # TODO: 3 times super.aggregate
      # TODO: test statistic from rulefit paper
    }, 
    generatePlot = function() {
    }
  )
)



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

