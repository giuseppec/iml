context("PartialDependence()")

test_that("PartialDependence works for single output and single feature", {
  
  grid.size = 10
  pdp.obj = PartialDependence$new(predictor1, feature = 1, grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "y.hat"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  
  expect_equal(pdp.obj$feature.name, "a")
  pdp.obj$set.feature(3)
  expect_equal(pdp.obj$feature.name, "c")
  pdp.obj$set.feature("b")
  expect_equal(pdp.obj$feature.name, "b")

  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("b", "y.hat"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
})

test_that("PartialDependence works for single output and 2 features, 2D grid.size", {
  
  ## two numerical features with 2 grid.sizes
  grid.size = c(10,2)
  pdp.obj = PartialDependence$new(predictor1, feature = c("a", "b"), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", "y.hat"))
  expect_equal(nrow(dat), grid.size[1] * grid.size[2])  
  expect_equal(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("PartialDependence works for single output and 2 numerical features, 1D grid.size", {
  
  
  ## Two numerical with same grid.size
  grid.size = 10
  pdp.obj = PartialDependence$new(predictor1, feature = c(1,2), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", "y.hat"))
  expect_equal(nrow(dat), grid.size * grid.size)  
  expect_equal(nrow(unique(dat)), grid.size * grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that("PartialDependence works for single output and numerical + categorical feature", {
  
  ## One feature categorical
  grid.size = 11
  pdp.obj = PartialDependence$new(predictor1, feature = c(1,3), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "c", "y.hat"))
  expect_equal(nrow(dat), grid.size[1] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size * length(unique(X[,3])))  
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(unique(dat$c), unique(X$c))  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  
  ## One feature categorical
  grid.size = c(7,9)
  pdp.obj = PartialDependence$new(predictor1, feature = c(3,2), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", "b", "y.hat"))
  expect_equal(nrow(dat), grid.size[2] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size[2] * length(unique(X[,3])))  
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  expect_equal(unique(dat$c), unique(X$c))  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  
})

test_that("PartialDependence works for multiple output", {
  
  grid.size = 10
  pdp.obj = PartialDependence$new(predictor2, feature = "a", grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "..class.name", "y.hat"))
  expect_equal(nrow(dat), grid.size * 2)  
  expect_equal(nrow(unique(dat)), grid.size * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  })
