# For a grid (grid.dat) of features (param features) creates a blown up
# dataset with the marginals of features not in 'features'. 
# The samples (n.sample.dist number of samples) for the marginals are drawn from dist.dat
# grid.dat only needs to contain the columns which are fixed. 
# dist.dat needs to contain all columns
# id.dist: should an id column for the samples drawn from dist.dat be added to results data.table?
generate.marginals = function(grid.dat, dist.dat, features, n.sample.dist = NULL, id.dist = FALSE) {
  assert_data_table(grid.dat)
  assert_data_table(dist.dat)
  assert_true(all(features %in% colnames(grid.dat)))
  assert_true(all(colnames(grid.dat) %in% colnames(dist.dat)))
  
  assert_character(features, unique = TRUE)
  if (is.null(n.sample.dist)) {
    n.sample.dist = nrow(dist.dat)
  }
  features.rest = setdiff(colnames(dist.dat), features)
  grid.index = rep(1:nrow(grid.dat), each = n.sample.dist)
  partial_j1 = grid.dat[grid.index, features, with = FALSE]
  if ((nrow(dist.dat) == nrow(grid.dat)) & (n.sample.dist == 1)) {
    # special case which amounts to shuffling
    dist.index = sample(1:nrow(dist.dat), size = nrow(dist.dat))
  } else {
    dist.index = unlist(lapply(1:nrow(grid.dat), function(x) sample(1:nrow(dist.dat), size = n.sample.dist)))
  }
  partial_j2 = dist.dat[dist.index, features.rest, with = FALSE]
  partial_j = cbind(partial_j1, partial_j2)
  partial_j$.id = grid.index
  if (id.dist) {
    partial_j$.id.dist = dist.index
  }
  partial_j
}