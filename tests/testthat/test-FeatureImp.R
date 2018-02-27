context("FeatureImp()")

#set.seed(42)
y = f(X) + rnorm(nrow(X))
y2 = factor(ifelse(X$b + X$a < 20, "pred", "pred2"))

test_that("FeatureImp works for single output", {
  
  var.imp = FeatureImp$new(predictor1, X, y = y, loss = "mse")
  dat = var.imp$results
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  

  var.imp = FeatureImp$new(predictor1, X, y = y, loss = "mse", method = "cartesian")
  dat = var.imp$results
  # Making sure the result is sorted by decreasing importance
  expect_equal(dat$importance, dat[order(dat$importance, decreasing = TRUE),]$importance)
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  X.exact = data.frame(x1 = c(1,2,3), x2 = c(9,4,2))
  f.exact = Model$new(function(x) x[[1]])
  y.exact = c(2,3,4)
  # creates a problem on win builder
  # model.error = Metrics::mse(y.exact, f.exact$predict(X.exact))
  model.error = 1
  cart.indices = c(1,1,2,2,3,3)
  cartesian.error = Metrics::mse(y.exact[cart.indices], c(2,3,1,3,1,2))
  
  var.imp = FeatureImp$new(f.exact, X.exact, y = y.exact, loss = "mse", method = "cartesian")
  dat = var.imp$results
  expect_equal(dat$importance, c(cartesian.error, 1))
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(model.error, var.imp$error.original)
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

test_that("FeatureImp works for single output and function as loss", {
    
  var.imp = FeatureImp$new(predictor1, X, y = y, loss = Metrics::mse)
  dat = var.imp$results
  # Making sure the result is sorted by decreasing importance
  expect_equal(dat$importance, dat[order(dat$importance, decreasing = TRUE),]$importance)
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that("FeatureImp works for multiple output",{
  
  var.imp = FeatureImp$new(predictor2, X, y = y2, loss = "ce")
  dat = var.imp$results
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})