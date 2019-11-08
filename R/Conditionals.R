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
      self$features = data$feature.names
      private$fit_conditionals()
    },
    csample = function(X, feature, size){
      assert_number(size, lower = 1)
      assert_character(feature)
      assert_data_table(X)
      cmodel = self$models[[feature]]
      if (inherits(cmodel, "trafotree")) {
        # When model did not split, we can sample marginal
        if (length(nodeids(cmodel)) == 1) {
          return(data.frame(feature = sample(X[[feature]], size = size * nrow(X), replace = TRUE)))
        }
        xrange = c(min(self$data$X[[feature]]), max(self$data$X[[feature]]))
        # type = 1 so that only actual X values are used
        quants = quantile(self$data$X[[feature]], probs = seq(from = 0, to = 1, length.out = 50), type = 1)
        qq = predict(cmodel,
                   newdata = data.frame(X),
	           type = "distribution",
                   q = quants)
        pfuns = apply(qq, 2, function(obs) {
                        approxfun(x = obs, y = quants,
                                  yleft = xrange[1],
                                  yright = xrange[2])
                     })
        v = sapply(pfuns, function(x) x(runif(size)))
        if(inherits(v, "matrix")){
          data.frame(t(v))
        } else {
          data.frame(matrix(v))
        }
      } else {
        preds = predict(cmodel, newdata = X, type = "prob")
        cls = colnames(preds)
        sample_prob = function(p, x){
          if(any(p == 1)) return(rep(x[p==1], times = size))
          sapply(1:size, function(i) sample(x, prob = p))
        }
        res = apply(preds, 1, sample_prob, x = cls)
        if(size == 1) {
          res = data.frame(res)
        } else {
          res = data.frame(t(res))
        }
        res = data.frame(res = res)
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
      } else {
        probs = predict(cmodel, newdata = X, type = "prob")
        probs.m = melt(probs)$value
        densities = data.table(.dens = probs.m, .id.dist = rep(1:nrow(X), each = ncol(probs)),
                   feature = factor(rep(colnames(probs), times = nrow(X)), levels = levels(self$data[[feature]])))
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
