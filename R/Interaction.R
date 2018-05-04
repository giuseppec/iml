#' Interaction strength
#' 
#' \code{Interaction} measures feature interactions in a prediction model.
#' 
#' @format \code{\link{R6Class}} object.
#' @name Interaction
#' @section Usage:
#' \preformatted{
#' tree = Interaction$new(predictor, maxdepth = 2, tree.args = NULL, run = TRUE)
#' 
#' plot(tree)
#' predict(tree, newdata)
#' tree$results
#' print(tree)
#' }
#' 
#' @section Arguments:
#' 
#' For Interaction$new():
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
#' dt = Interaction$new(mod)
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
#' n = 100
#' dat = data.table(x1 = rnorm(n),
#' x2 = rnorm(n), x3 = rnorm(n), 
#' x4 = rnorm(n), x5 = rnorm(n), x6 = rnorm(n))
#' f = function(dat) {
#' dat$x1 * dat$x3 + dat$x1 +  dat$x3 + dat$x6 * dat$x1
#' }
#' mod = Predictor$new(f, dat, predict.fun = function(ff, newdata) {ff(newdata)})
#' inter = Interaction$new(mod, "x1")
#' inter$results
#' 
#' 
#' inter = Interaction$new(mod, features = c("x1"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod, features = c("x2"), grid.size = 100)
#' inter$results
#' plot(inter)
#' 
#' inter = Interaction$new(mod, features = c("x3"), grid.size = 100)
#' inter$results
#'
#' inter = Interaction$new(mod, features = c("x4"), grid.size = 100)
#' inter$results
#'  
#' inter = Interaction$new(mod, features = c("x1", "x2"), grid.size = 100)
#' inter$results
#' inter = Interaction$new(mod, features = c("x1", "x3"), grid.size = 100)
#' inter$results
#' 
#' inter = Interaction$new(mod, features = c("x2", "x3"), grid.size = 100)
#' inter$results
#' plot(inter)
#' inter = Interaction$new(mod,  grid.size = 100)
#' inter$results
#' plot(inter)
#' 
#' @seealso 
#' \link{Partial}
#' 
#' @importFrom data.table dcast
#' 
#' @export
NULL

#' @export
Interaction = R6::R6Class("Interaction",
  inherit = InterpretationMethod,
  public = list(
    # The fitted tree
    features = NULL,
    grid.size = NULL,
    initialize = function(predictor, features = NULL, grid.size = 20, run = TRUE) {
      assert_vector(features, max.len = 2, null.ok = TRUE)
      assert_number(grid.size, lower = 2)
      assert_logical(run)
      
      if (!is.null(features) && is.numeric(features)) {
        self$features = predictor$data$feature.names[features]
      } else {
        self$features = features
      }
      self$grid.size = min(grid.size, predictor$data$n.rows)
      super$initialize(predictor)
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
      ggplot(self$results) + 
        geom_col(aes(x = .feature, y = .interaction)) + 
        scale_y_continuous("Interaction strength") + 
        scale_x_discrete("Features")
    }
  )
)


# For a grid (grid.dat) of features (param features) creates a blown up
# dataset with the marginals of features not in 'features'. 
# The samples drawn for the marginals are coming from dist.dat
generate.marginals = function(grid.dat, dist.dat, features) {
  assert_data_table(grid.dat)
  assert_data_table(dist.dat)
  assert_true(all(features %in% colnames(grid.dat)))
  assert_true(all(colnames(grid.dat) %in% colnames(dist.dat)))
  
  assert_character(features, unique = TRUE)
  n.sample = nrow(dist.dat)
  features.rest = setdiff(colnames(grid.dat), features)
  grid.index = rep(1:nrow(grid.dat), each = n.sample)
  partial_j1 = grid.dat[grid.index, ..features]
  partial_j2 = data.table::rbindlist(lapply(1:nrow(grid.dat), 
    function(x) dist.dat[sample(1:nrow(dist.dat), size = n.sample), ..features.rest]), use.names = TRUE)
  partial_j = cbind(partial_j1, partial_j2)
  partial_j$.id = grid.index
  partial_j
}


# The test statistic as defined in:
# Friedman H. F, & Popescu, B. E. (n.d.). Predictive Learning via 
# Rules Ensembles, 25(9), 1682–1690. http://doi.org/10.1007/s13398-014-0173-7.2
# Measures the variance explained by the interaction
h.test = function(f.all, f.j, f.no.j) { 
  assert_numeric(f.all, any.missing = FALSE)
  assert_numeric(f.j, any.missing = FALSE)
  assert_numeric(f.no.j, any.missing = FALSE)
  # center
  f.all = scale(f.all, center = TRUE, scale = FALSE)
  f.j =  scale(f.j, center = TRUE, scale = FALSE)
  f.no.j =  scale(f.no.j, center = TRUE, scale = FALSE)
  # statistics
  sum((f.all  - (f.j + f.no.j))^2) / sum(f.all^2)
}  

aggregate.interaction = function(partial_dat, prediction, features) {
  assert_data_table(partial_dat)
  assert_data_frame(prediction)
  assert_character(features, null.ok = TRUE)
  assert_true(all(features %in% colnames(partial_dat)))
  
  partial_dat$pred = prediction
  if (length(features) == 2) {
    pd.jk = partial_dat[.type == "jk",  mean(pred), by = .id]
    pd.j = partial_dat[.type == "j",  mean(pred), by = .id]
    pd.k = partial_dat[.type == "k",  mean(pred), by = .id]
    res = h.test(pd.jk$V1, pd.j$V1, pd.k$V1) 
    data.frame(.feature = paste(features, collapse = ":"),
      .interaction = res)
  } else {
    if (length(features) == 1) {
      partial_dat$.feature = features
    }
    partial_dat = partial_dat[, c(".id", ".feature", ".type", "pred")]
    pd = dcast(partial_dat, .feature + .id~ .type, value.var = "pred", fun.aggregate = mean)
    data.frame(pd[, list(.interaction = h.test(f, j, no.j)), by = .feature])
  }
}


# The intervention function for the Interaction class
# Depending on the type of interaction (1v1 or 1 vs all) creates the marginals
intervene.interaction = function(dataSample, feature.name, grid.size) {
  assert_data_table(dataSample)
  assert_character(feature.name, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_number(grid.size)
  
  grid.dat = dataSample[sample(1:nrow(dataSample), size = grid.size),]
  dist.dat = dataSample
  if (length(feature.name) == 1) {
    partial_j = generate.marginals(grid.dat, dist.dat, feature.name)
    partial_j$.type = "j"
    partial_noj  = generate.marginals(grid.dat, dist.dat, setdiff(colnames(dataSample),feature.name))
    partial_noj$.type = "no.j"
    grid.dat$.type = "f"
    grid.dat$.id = 1:nrow(grid.dat)
    rbind(partial_j, partial_noj, grid.dat, use.names = TRUE)
  } else if (length(feature.name) == 2) {
    partial_jk = generate.marginals(grid.dat, dist.dat, feature.name)
    partial_jk$.type = "jk"
    partial_j  = generate.marginals(grid.dat, dist.dat, feature.name[1])
    partial_j$.type = "j"
    partial_k  = generate.marginals(grid.dat, dist.dat, feature.name[2])
    partial_k$.type = "k"
    rbind(partial_jk, partial_j, partial_k, use.names = TRUE)
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
  rbindlist(res, use.names = TRUE)
}

