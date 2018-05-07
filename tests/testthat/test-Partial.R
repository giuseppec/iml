context("Partial()")

test_that("Partial (pdp only) works for single output and single feature", {
  grid.size = 10
  pdp.obj = Partial$new(predictor1, ice = FALSE, feature = 1, grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
  
  expect_equal(pdp.obj$feature.name, "a")
  pdp.obj$set.feature(3)
  expect_equal(pdp.obj$feature.name, "c")
  pdp.obj$set.feature("b")
  expect_equal(pdp.obj$feature.name, "b")
  
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  
  # Centering 
  pdp.obj$center(0)
  checkPlot(pdp.obj)
  dat = pdp.obj$results
  expect_equal(min(dat$.y), 0)
})

test_that("Partial (pdp only) works for single output and 2 features, 2D grid.size", {
  ## two numerical features with 2 grid.sizes
  grid.size = c(10,2)
  pdp.obj = Partial$new(predictor1, ice = FALSE, feature = c("a", "b"), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size[1] * grid.size[2])  
  expect_equal(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(pdp.obj)
  pdp.obj2 = Partial$new(predictor1, ice = TRUE, feature = c("a", "b"), grid.size = grid.size)
  expect_equal(pdp.obj$results, pdp.obj2$results)
  pdp.obj3 = Partial$new(predictor1, center.at = 0, feature = c("a", "b"), grid.size = grid.size)
  expect_equal(pdp.obj$results, pdp.obj3$results)
})

test_that("Partial (pdp only) works for single output and 2 numerical features, 1D grid.size", {
  ## Two numerical with same grid.size
  grid.size = 7
  pdp.obj = Partial$new(predictor1, feature = c(1,2), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size * grid.size)  
  expect_equal(nrow(unique(dat)), grid.size * grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})

test_that("Partial (pdp only) works for single output and numerical + categorical feature", {
  
  ## One feature categorical
  grid.size = 11
  pdp.obj = Partial$new(predictor1, feature = c(1,3), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "c", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size[1] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size * length(unique(X[,3])))  
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(unique(dat$c), unique(X$c))  
  checkPlot(pdp.obj)
  
  ## One feature categorical
  grid.size = c(7,9)
  pdp.obj = Partial$new(predictor1, feature = c(3,2), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", "b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size[2] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size[2] * length(unique(X[,3])))  
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  expect_equal(unique(dat$c), unique(X$c))  
  checkPlot(pdp.obj)
})

test_that("Partial (pdp) works for multiple output", {
  grid.size = 10
  pdp.obj = Partial$new(predictor2, ice = FALSE, feature = "a", grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size * 2)  
  expect_equal(nrow(unique(dat)), grid.size * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})

test_that("Partial (pdp+ice) works for multiple output", {
  grid.size = 10
  pdp.obj = Partial$new(predictor2, feature = "a", grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * 2 + grid.size * nrow(X) * 2)  
  expect_equal(nrow(unique(dat)), grid.size * 2 + grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})



test_that("Partial (ice) works for single output and single feature", {
  grid.size = 10
  ice.obj = Partial$new(predictor1, aggregation = "none", feature = 1, grid.size = grid.size)
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * nrow(X))  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
})

test_that("Partial (ice) works for multiple output", {
  
  grid.size = 10
  ice.obj = Partial$new(predictor2, feature = "a", grid.size = grid.size, aggregation = "none")
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type",  ".id"))
  expect_equal(nrow(dat), grid.size * nrow(X) * 2)  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
})


test_that("centered Partial (ice) works for multiple output", {
  
  grid.size = 10
  ice.obj = Partial$new(predictor2, feature = "a", grid.size = grid.size, center = 10, ice = TRUE)
  dat = ice.obj$results
  expect_equal(ice.obj$center.at, 10)
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), (grid.size + 1) * nrow(X) * 2 + (grid.size + 1) * 2)  
  expect_equal(nrow(unique(dat)), (grid.size + 1) * nrow(X) * 2 + (grid.size + 1) * 2)  
  expect_equal(max(dat$a), 10)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
  
  ice.obj$center(-1)
  expect_equal(ice.obj$center.at, -1)
  
  expect_warning({ice.obj$center.at = 10})
  expect_equal(ice.obj$center.at, -1)
  
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), -1)
  
})

