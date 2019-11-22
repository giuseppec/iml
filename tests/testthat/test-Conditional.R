context("Conditional")


test_that("Conditional works",{
  set.seed(123)
  X = data.frame(x1 = rnorm(30))
  X$x2 = X$x1 + rnorm(30, sd = 0.1)
  X$x3 = as.factor((X$x1 + X$x2) > 0)
  datx = Data$new(X)
  X = data.table(X)
  cond = Conditional$new(data = datx, feature = "x1") 
  expect_true(inherits(cond$model, "constparty")) 
  cond2 = Conditional$new(data = datx, feature = "x3") 
  expect_true(inherits(cond$model, "constparty")) 

  # Sampling
  samp = cond$csample(X[1:4,], size = 5)
  expect_equal(dim(samp), c(4,5))
  expect_class(samp, "data.table") 
  expect_equal(class(samp[[1]]), "numeric")
  samp = cond2$csample(X[1:4,],  size = 5)
  expect_equal(dim(samp), c(4,5))
  expect_class(samp, "data.frame")
  expect_class(samp[[1]], "factor")

  # Weighting
  ww = cond$cdens(X, xgrid = quantile(X$x1))
  expect_equal(nrow(ww), nrow(X) * 5)
  expect_equal(class(ww$x1), "numeric")

  ww = cond2$cdens(X)
  expect_equal(nrow(ww), nrow(X) * 2)
  expect_equal(class(ww$x3), "factor")
})
