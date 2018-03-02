context("Ice()")

test_that("Ice works for single output and single feature", {
  
  grid.size = 10
  ice.obj = Ice$new(predictor1, feature = 1, grid.size = grid.size)
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "..individual", "y.hat"))
  expect_equal(nrow(dat), grid.size * nrow(X))  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  p = plot(ice.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that("Ice works for multiple output", {
  
  grid.size = 10
  ice.obj = Ice$new(predictor2, feature = "a", grid.size = grid.size)
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "..individual", "..class.name", "y.hat"))
  expect_equal(nrow(dat), grid.size * nrow(X)*2)  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  
  p = plot(ice.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})


test_that("centered Ice works for multiple output", {
  
  grid.size = 10
  ice.obj = Ice$new(predictor2, feature = "a", grid.size = grid.size, center = 10)
  dat = ice.obj$results
  expect_equal(ice.obj$center.at, 10)
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "..individual","..class.name", "y.hat"))
  expect_equal(nrow(dat), (grid.size + 1) * nrow(X) * 2)  
  expect_equal(nrow(unique(dat)), (grid.size + 1) * nrow(X) * 2)
  expect_equal(max(dat$a), 10)
  expect_equal(min(dat$a), 1)
  p = plot(ice.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  
  ice.obj$center(-1)
  expect_equal(ice.obj$center.at, -1)
  
  expect_warning({ice.obj$center.at = 10})
  expect_equal(ice.obj$center.at, -1)
  
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), -1)
  
})
