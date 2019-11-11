context("Conditionals")


test_that("Conditionals work",{
  set.seed(123)
  X = data.frame(x1 = rnorm(30))
  X$x2 = X$x1 + rnorm(30, sd = 0.1)
  X$x3 = as.factor((X$x1 + X$x2) > 0)
  datx = Data$new(X)
  X = data.table(X)
  cond = Conditionals$new(data = datx) 
  expect_true(inherits(cond$models[["x1"]], "constparty")) 
  expect_true(inherits(cond$models[["x2"]], "constparty")) 
  expect_true(inherits(cond$models[["x3"]], "constparty")) 

  # Sampling
  samp = cond$csample(X[1:4,], feature = "x1", size = 5)
  expect_equal(dim(samp), c(4,5))
  expect_equal(class(samp), "data.frame") 
  expect_equal(class(samp[[1]]), "numeric")
  samp = cond$csample(X[1:4,], feature = "x3", size = 5)
  expect_equal(dim(samp), c(4,5))
  expect_equal(class(samp), "data.frame")
  expect_equal(class(samp[[1]]), "factor")

  # Weighting
  ww = cond$cdens(X, feature = "x1", xgrid = quantile(X$x1))
  expect_equal(nrow(ww), nrow(X) * 5)
  expect_equal(class(ww$x1), "numeric")

  ww = cond$cdens(X, feature = "x3")
  expect_equal(nrow(ww), nrow(X) * 2)
  expect_equal(class(ww$x3), "factor")
})
