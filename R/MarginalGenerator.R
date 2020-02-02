# For a grid (grid.dat) of features (param features) creates a blown up dataset
# with the marginals of features not in 'features'. The samples (n.sample.dist
# number of samples) for the marginals are drawn from dist.dat. If n.sample.dist
# is not set, the whole cartesian product between grid.dat and dist.dat is built
# grid.dat only needs to contain the columns which are fixed. Decide here which
# grid points should be used.
# dist.dat needs to contain all columns
# id.dist: should an id column for the samples drawn from dist.dat be added to
# results data.table?
MarginalGenerator <- R6Class(
  public = list(
    initialize = function(grid.dat, dist.dat, features, n.sample.dist = NULL,
                          y = NULL, id.dist = FALSE, cartesian = FALSE) {
      assert_data_table(grid.dat)
      assert_data_table(dist.dat)
      if (!is.null(n.sample.dist) && n.sample.dist > nrow(dist.dat)) {
        message("Number of repetitions larger than number of unique permutations per row. 
          Switching to cartesian = TRUE")
        cartesian <- TRUE
      }
      if (cartesian) {
        n.sample.dist <- nrow(dist.dat)
      }
      stopifnot(cartesian | !is.null(n.sample.dist))
      assert_true(all(features %in% colnames(grid.dat)))
      assert_true(all(colnames(grid.dat) %in% colnames(dist.dat)))
      assert_character(features, unique = TRUE)
      assert_data_frame(y, null.ok = TRUE, nrows = nrow(dist.dat))

      private$grid.dat <- grid.dat
      private$dist.dat <- dist.dat
      private$features <- features
      private$features.rest <- setdiff(colnames(dist.dat), features)
      private$n.sample.dist <- n.sample.dist
      private$id.dist <- id.dist
      private$y <- y
      self$n_total <- n.sample.dist * nrow(grid.dat)

      private$grid.index <- rep(1:nrow(grid.dat), each = n.sample.dist)

      if (!cartesian && (nrow(dist.dat) == nrow(grid.dat)) &&
        (n.sample.dist == 1)) {
        # special case which amounts to shuffling
        private$dist.index <- sample(1:nrow(dist.dat), size = nrow(dist.dat))
      } else {
        private$dist.index <- unlist(lapply(
          1:nrow(grid.dat),
          function(x) sample(1:nrow(dist.dat), size = n.sample.dist)
        ))
      }
    },

    finished = FALSE,
    n_total = NULL,

    # Return the next n samples
    next.batch = function(n, y = FALSE) {
      if (!self$finished) {
        pointer <- private$counter
        if (n > (length(private$grid.index) - pointer)) self$finished <- TRUE

        n.left <- length(private$grid.index) - pointer
        step <- min(n - 1, n.left)
        batch.index <- pointer:(pointer + step)

        partial_j1 <- private$grid.dat[private$grid.index[batch.index],
          private$features,
          with = FALSE
        ]

        partial_j2 <- private$dist.dat[private$dist.index[batch.index],
          private$features.rest,
          with = FALSE
        ]

        partial_j <- cbind(partial_j1, partial_j2)

        partial_j$.id <- private$grid.index[batch.index]
        if (private$id.dist) {
          partial_j$.id.dist <- private$dist.index[batch.index]
        }
        private$counter <- private$counter + n

        if (y) partial_j <- cbind(partial_j, private$y[partial_j$.id.dist, ])
        partial_j
      }
    },

    all = function() {
      private$counter <- 1
      self$next.batch(length(private$grid.index))
    }
  ),

  private = list(
    counter = 1,
    grid.dat = NULL,
    dist.dat = NULL,
    features = NULL,
    features.rest = NULL,
    n.sample.dist = NULL,
    id.dist = NULL,
    grid.index = NULL,
    dist.index = NULL,
    y = NULL
  )
)
