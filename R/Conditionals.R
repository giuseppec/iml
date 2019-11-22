# For a grid (grid.dat) of features (param features) creates a blown up
# dataset with the marginals of features not in 'features'. 
# The samples (n.sample.dist number of samples) for the marginals are drawn from dist.dat.
#            If n.sample.dist is not set, the whole cartesian product between grid.dat and dist.dat is built
# grid.dat only needs to contain the columns which are fixed. Decide here which grid points should be used.
# dist.dat needs to contain all columns
Conditionals = R6Class(
  public = list(
    features = NULL,
    data = NULL,
    models = NULL,
    initialize = function(data) {
      assert_class(data, "Data")
      self$data = data
      private$fit_conditionals()
    },
    csample = function(X, feature, size){
      assert_number(size, lower = 1)
      assert_character(feature)
      assert_data_table(X)
      cmodel = self$models[[feature]]
      data_nodes = self$cnode(self$data$X, feature)
      X_nodes = self$cnode(X, feature)
      xj_samples = lapply(1:nrow(X), function(i) {
        node = X_nodes[i, "node"]
        data_ids = which(data_nodes$node == node)
        data_ids = setdiff(data_ids, i)
        data_ids_sample = sample(data_ids, size = size, replace = TRUE)
        xj = self$data$X[data_ids_sample, feature, with = FALSE]
        data.frame(t(xj))
      })
      rbindlist(xj_samples)
    },
    cdens = function(X, feature, xgrid = NULL){
      cmodel = self$models[[feature]]
      if(inherits(cmodel, "trafotree")) {
        conditionals = predict(cmodel, newdata = X, type = "density", q = xgrid)
        densities = melt(conditionals)$value
        densities = data.table(.dens = densities, .id.dist = rep(1:nrow(X), each = length(xgrid)), 
                   feature = rep(xgrid, times = nrow(X)))
      } else if (self$data$feature.types[feature] == "categorical") {
        probs = predict(cmodel, newdata = X, type = "prob")
        probs.m = melt(probs)$value
        densities = data.table(.dens = probs.m, .id.dist = rep(1:nrow(X), each = ncol(probs)),
                   feature = factor(rep(colnames(probs), times = nrow(X)), levels = levels(self$data[[feature]])))
      } else {
        pr = predict(cmodel, newdata = X, type = "density")
        at = unique(X[[feature]])
 	res = sapply(pr, function(pr) pr(at) / sum(pr(at)))
        res = data.table(t(res))
	colnames(res) = as.character(at)
	res.m = melt(res, measure.vars = as.character(at))
	densities = data.table(.dens = res.m$value, .id.dist = rep(1:nrow(X), times = ncol(X)), feature = rep(at, each = nrow(X)))
      }
      colnames(densities) = c(".dens", ".id.dist", feature)
      densities
  },
  cnode = function(X, feature, prob = c(0.05, 0.95)) {
    cmodel = self$models[[feature]]
    node = predict(cmodel, newdata = X, type = "node")
    node_df = data.frame(node = factor(node), .id = names(node), .path = pathpred(cmodel, X))
    if(inherits(cmodel, "trafotree")) {
      # case of numerical feature
      quants = predict(cmodel, newdata = X, type = "quantile", prob = prob)
      quants = data.frame(t(quants))
      colnames(quants) = paste0("q", prob)
    } else if (self$data$feature.types[[feature]] == "numerical") {
      # case of numerical features with few unique values
      quants = predict(cmodel, newdata = X, type = "quantile", at = prob)
      colnames(quants) = paste0("q", prob)
    } else {
      # case of categorical feature
      quants = predict(cmodel, newdata = X, type = "prob")
      names(quants) = levels(X[[feature]])
    }
    cbind(node_df, quants)
  }
  ), 
  private = list(
  fit_conditional = function(feature) {
    require("trtf")
    y = self$data$X[[feature]]
    if ((self$data$feature.types[feature] == "numerical") & (length(unique(y)) > 2)) {
      yvar = numeric_var(feature, support = c(min(y), max(y)))
      By  =  Bernstein_basis(yvar, order = 5, ui = "incr")
      m = ctm(response = By,  todistr = "Normal", data = self$data$X )
      form = as.formula(sprintf("%s ~ 1 | .", feature))
      part_cmod = trafotree(m, formula = form,  data = self$data$X)
    } else {
      form = as.formula(sprintf("%s ~ .", feature))
      part_cmod = ctree(form, data = self$data$X)
    }
      self$models[[feature]] = part_cmod
    },
  fit_conditionals = function(){
    lapply(self$data$feature.names, function(feat) {
               private$fit_conditional(feat)
	  })
    }
  )
)
