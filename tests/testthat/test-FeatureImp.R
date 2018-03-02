context("FeatureImp()")

#set.seed(42)

mse = function(actual, predicted) {
  mean((actual - predicted)^2)
}
expectedColnames = c("feature", "original.measure", "permutation.measure", "importance")

test_that("FeatureImp works for single output", {
  
  var.imp = FeatureImp$new(predictor1,  measure = "mse")
  dat = var.imp$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), expectedColnames)
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  

  var.imp = FeatureImp$new(predictor1,  measure = "mse", method = "cartesian")
  dat = var.imp$results
  # Making sure the result is sorted by decreasing importance
  expect_class(dat, "data.frame")
  expect_equal(dat$importance, dat[order(dat$importance, decreasing = TRUE),]$importance)
  expect_equal(colnames(dat), expectedColnames)
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  X.exact = data.frame(x1 = c(1,2,3), x2 = c(9,4,2))
  y.exact = c(2,3,4)
  f.exact = Predictor$new(predict.fun = function(newdata) newdata[[1]], data = X.exact, y = y.exact)
  model.measure = mse(y.exact, f.exact$predict(X.exact))
  cart.indices = c(1,1,2,2,3,3)
  cartesian.measure = mlr::measureMSE(y.exact[cart.indices], c(2,3,1,3,1,2))
  
  var.imp = FeatureImp$new(f.exact, measure = "mse", method = "cartesian")
  dat = var.imp$results
  expect_class(dat, "data.frame")
  expect_equal(dat$importance, c(cartesian.measure, 1))
  expect_equal(colnames(dat), expectedColnames)
  expect_equal(model.measure, var.imp$original.measure)
  expect_equal(nrow(dat), ncol(X.exact))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  p = plot(var.imp, sort = FALSE)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  p = var.imp$plot()
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that("FeatureImp works for single output and function as measure", {
    

  var.imp = FeatureImp$new(predictor1, measure = mse)
  dat = var.imp$results
  expect_class(dat, "data.frame")
  # Making sure the result is sorted by decreasing importance
  expect_equal(dat$importance, dat[order(dat$importance, decreasing = TRUE),]$importance)
  expect_equal(colnames(dat), expectedColnames)
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that("FeatureImp works for multiple output",{
  var.imp = FeatureImp$new(predictor2, measure = "mmce")
  dat = var.imp$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), expectedColnames)
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})


test_that("FeatureImp fails without target vector",{
  predictor2 = Predictor$new(f, data = X, predict.fun = predict.fun)
  expect_error(FeatureImp$new(predictor2, measure = "mmce"))
})