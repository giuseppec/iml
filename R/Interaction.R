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
#' n = 1000
#' dat = data.table(x1 = rnorm(n),
#' x2 = rnorm(n), x3 = rnorm(n))
#' f = function(dat) {
#' dat$x1 * dat$x3 + dat$x1
#' }
#' mod = Predictor$new(f, dat, predict.fun = function(ff, newdata) {ff(newdata)})
#' inter = Interaction$new(mod, "x1", grid.size = 100)
#' inter$results
#' 
#' intermediate = cbind(inter$.__enclos_env__$private$dataDesign, data.frame(pred = inter$.__enclos_env__$private$qResults))
#' 
#' head(intermediate)
#' 
#' mean(intermediate[intermediate$.type == 'j', pred])
#' mean(intermediate[intermediate$.type == 'no.j', pred])
#' mean(intermediate[intermediate$.type == 'f', pred])
#' }
#' 
#' inter = Interaction$new(mod, features = c("x1"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod, features = c("x2"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod, features = c("x3"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod, features = c("x1", "x2"), grid.size = 100)
#' inter$results
#' inter = Interaction$new(mod, features = c("x1", "x3"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod, features = c("x2", "x3"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod,  grid.size = 100)
#' inter$results
#' 
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
    grid.size = NULL,
    initialize = function(predictor, features = NULL, grid.size = 20, run = TRUE) {
      self$features = features
      self$grid.size = grid.size
      super$initialize(predictor)
      # TODO: accept only 0,1 or 2 features:
      # 0 features: Compute interaction between 1 vs rest test statistics for all features
      # 1 feature: Compute interaction between 1 vs rest test statistics for chosen feature
      # 2 features: Compute interaction between 2 features.
      # TODO: create 3 Partial objects: for first, second and both features (run=FALSE)
      if(run) self$run()
    }
  ), 
  private = list(
    intervene = function() {
      if (is.null(self$features)) {
        intervene.interaction.multi(private$dataSample, grid.size = self$grid.size)
      } else {
        intervene.interaction(private$dataSample, self$features, grid.size = self$grid.size)
      }
    },
    aggregate = function() {
      aggregate.interaction(private$dataDesign, private$qResults, features = self$features)
    }, 
    generatePlot = function() {
    }
  )
)


# n.sample is 2D: first is sampling of grid points, 2nd is sampling of points for evaluation
generate.mc.dat = function(grid.dat, dist.dat, features) {
  n.sample = nrow(dist.dat)
  features.rest = setdiff(colnames(grid.dat), features)
  grid.index = rep(1:nrow(grid.dat), each = n.sample)
  partial_j1 = grid.dat[grid.index, ..features]
  partial_j2 = data.table::rbindlist(lapply(1:nrow(grid.dat), 
    function(x) dist.dat[sample(1:nrow(dist.dat), size = n.sample), ..features.rest]))
  partial_j = cbind(partial_j1, partial_j2)
  partial_j$.id = grid.index
  partial_j
}



h.test = function(f.all, f.j, f.no.j) { 
  # center
  f.all = scale(f.all, center = TRUE, scale = FALSE)
  f.j =  scale(f.j, center = TRUE, scale = FALSE)
  f.no.j =  scale(f.no.j, center = TRUE, scale = FALSE)
  # statistics
  sum((f.all  - (f.j + f.no.j))^2) / sum(f.all^2)
}  

aggregate.interaction = function(partial_dat, prediction, features) {
  partial_dat$pred = prediction
  if (length(features) == 2) {
    pd.jk = partial_dat[.type == "jk",  mean(pred), by = .id]
    pd.j = partial_dat[.type == "j",  mean(pred), by = .id]
    pd.k = partial_dat[.type == "k",  mean(pred), by = .id]
    h.test(pd.jk$V1, pd.j$V1, pd.k$V1) 
  } else {
    if (length(features) == 1) {
      partial_dat$.feature = features
    }
    partial_dat = partial_dat[, c(".id", ".feature", ".type", "pred")]
    pd = dcast(partial_dat, .id + .feature ~ .type, value.var = "pred", fun.aggregate = mean)
    pd[,h.test(f, j, no.j), by = .feature]
  }
}


intervene.interaction = function(dataSample, feature.name, grid.size) {
  if (length(feature.name) == 1) {
    grid.dat = dataSample[sample(1:nrow(dataSample), size = grid.size),]
    dist.dat = dataSample
    partial_j = generate.mc.dat(grid.dat, dist.dat, feature.name)
    partial_j$.type = "j"
    partial_noj  = generate.mc.dat(grid.dat, dist.dat, setdiff(colnames(dataSample),feature.name))
    partial_noj$.type = "no.j"
    grid.dat$.type = "f"
    grid.dat$.id = 1:nrow(grid.dat)
    partial_dat = rbind(partial_j, partial_noj, grid.dat)
    partial_dat
  } else if (length(feature.name) == 2) {
    # TODO Implement 1v1
    grid.dat = dataSample[sample(1:nrow(dataSample), size = grid.size),]
    dist.dat = dataSample
    partial_jk = generate.mc.dat(grid.dat, dist.dat, feature.name)
    partial_jk$.type = "jk"
    partial_j  = generate.mc.dat(grid.dat, dist.dat, feature.name[1])
    partial_j$.type = "j"
    partial_k  = generate.mc.dat(grid.dat, dist.dat, feature.name[2])
    partial_k$.type = "k"
    grid.dat$.id = 1:nrow(grid.dat)
    partial_dat = rbind(partial_jk, partial_j, partial_k)
    partial_dat
  }
}


intervene.interaction.multi = function(dataSample, grid.size) {
  features = colnames(dataSample)
  res = lapply(features, function (feature) {
    dt = intervene.interaction(dataSample = dataSample, 
      feature.name = feature, 
      grid.size = grid.size)
    dt$.feature = feature
    dt
  })
  rbindlist(res)
}

