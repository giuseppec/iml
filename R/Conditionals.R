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
      if (inherits(cmodel, "trafotree")) {
        probs = seq(from = 0, to = 1, length.out = 50)
        quants = quantile(self$data$X[[feature]], probs = probs, type = 1)
        qq = predict(cmodel,
                   newdata = X,
	           type = "distribution",
                   q = quants)
        pfuns = apply(qq, 2, function(obs) {
          last0 = rev(which(obs == 0))[1]
	  if(is.na(last0)) last0 = 1
          first1 = which(obs == 1)[1]
	  if (is.na(first1)) first1 = length(qq) 
          approxfun(x = obs[last0:first1], y = quants[last0:first1],
		    yleft = 0, yright = 0)
         })
         v = sapply(pfuns, function(x) x(runif(size)))
         if(inherits(v, "matrix")){
           data.frame(t(v))
         } else {
           data.frame(matrix(v))
         }
      } else if(self$data$feature.types[feature] == "categorical") {
        preds = predict(cmodel, newdata = X, type = "prob")
        cls = colnames(preds)
        sample_prob = function(p, x){
          if(any(p == 1)) return(rep(x[p==1], times = size))
          sample(x, size = size, prob = p, replace = TRUE)
        }
        res = apply(preds, 1, sample_prob, x = cls)
        if(size == 1) {
          res = data.frame(res = res)
        } else {
          res = data.frame(t(res))
        }
        res
      } else {
	preds = predict(cmodel, newdata = X, type = "density")
        at = unique(X[[feature]])
	# data.table of probabilities for unique outcomes
 	res = sapply(preds, function(preds) preds(at) / sum(preds(at)))
        res = data.frame(t(res))
	colnames(res) = as.character(at)
        sample_prob = function(p, x) {
          if(any(p == 1)) return(rep(x[p==1], times = size))
          base::sample(x, size = size, prob = p, replace = TRUE)
        }
        res = apply(res, 1, sample_prob, x = at)
        if(size == 1) {
          res = data.frame(res = res)
        } else {
          res = data.frame(t(res))
        }
        res
      } 
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
  }), 
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
