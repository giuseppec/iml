# For a grid (grid.dat) of features (param features) creates a blown up
# dataset with the marginals of features not in 'features'. 
# The samples (n.sample.dist number of samples) for the marginals are drawn from dist.dat.
#            If n.sample.dist is not set, the whole cartesian product between grid.dat and dist.dat is built
# grid.dat only needs to contain the columns which are fixed. Decide here which grid points should be used.
# dist.dat needs to contain all columns
# TODO: Speed up computation by sampling from Conditionals with size > 1
ConditionalGenerator = R6Class(
  public = list(
    finished = FALSE,
    n_total = NULL,
    sample_type = NULL,
    initialize = function(dist.dat, feature, cmodel, n.sample.dist = 1, y = NULL, sample_type) {
      assert_data_table(dist.dat)
      assert_true(all(feature %in% colnames(dist.dat)))
      assert_character(feature, len = 1)
      assert_data_frame(y, null.ok = TRUE, nrows = nrow(dist.dat))
      assert_class(cmodel, "R6")
      assert_choice(sample_type, c("parametric", "data"))

      self$sample_type = sample_type
      private$cmodel = cmodel
      private$dist.dat = dist.dat
      private$feature = feature
      private$features.rest = setdiff(colnames(dist.dat), feature)
      private$n.sample.dist = n.sample.dist
      private$y = y
      self$n_total = n.sample.dist * nrow(dist.dat)
      private$dist.index = rep(1:nrow(dist.dat), each = n.sample.dist)
    },
    # Return the next n samples
    next.batch = function(n, y = FALSE) {
      if (!self$finished) {
        pointer = private$counter
        if (n > (self$n_total - pointer)) self$finished = TRUE
        
        n.left = self$n_total - pointer
        step = min(n - 1, n.left)
        batch.index =  pointer:(pointer + step)
        
        data.slice = private$dist.index[batch.index]
        X = data.table(private$dist.dat[data.slice, ])
        # All other features
        partial_j2 = private$dist.dat[data.slice, private$features.rest, with = FALSE] 
        partial_j1 = private$cmodel$csample(X, size = 1, type = self$sample_type)
        partial_j = cbind(partial_j1, partial_j2)
        colnames(partial_j)[1] = private$feature
        
        partial_j$.id = data.slice
        private$counter = private$counter + n
        
        if(y) partial_j = cbind(partial_j, private$y[data.slice,])
        data.table(partial_j)
      }
    },
    all = function() {
      private$counter = 1
      self$next.batch(length(private$dist.index))
    }
  ), 
  private = list(
    counter = 1,
    cmodel = NULL,
    dist.dat = NULL,
    feature = NULL,
    features.rest = NULL,
    n.sample.dist = NULL,
    dist.index = NULL,
    y = NULL,
    quants = seq(from = 0, to = 1, length.out = 101)
  )
)
