context("LocalModel()")

set.seed(12)
expected.colnames = c("beta", "x.recoded", "effect", "x.original", "feature", "feature.value")

test_that("LocalModel works for single output and single feature", {
  
  x.interest = X[2,]
  k = 2
  set.seed(42)
  LocalModel1 = LocalModel$new(predictor1, x.interest=x.interest, k = k)
  dat = LocalModel1$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)
  p = plot(LocalModel1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  x.interest2 = X[4,]
  LocalModel1$explain(x.interest2)
  dat = LocalModel1$results
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)  

  pred = predict(LocalModel1, newdata = X[3:4,])
  expect_data_frame(pred, nrows = 2)
  expect_equal(colnames(pred), "prediction")
  
  LocalModel1 = LocalModel$new(predictor1, x.interest=x.interest, k = k, 
    dist.fun = "euclidean", kernel.width = 1)
  LocalModel1$explain(x.interest2)
  dat = LocalModel1$results
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)  
  
  pred = predict(LocalModel1, newdata = X[3:4,])
  expect_data_frame(pred, nrows = 2)
  expect_equal(colnames(pred), "prediction")
})

test_that("LocalModel works for multiple output", {
  
  library('rpart')
  clf = rpart(Species ~ ., data = iris)
  mod = Predictor$new(clf, data = iris)
  x.interest = iris[1,]
  k = 1
  set.seed(42)
  # rbind X a few times to eliminate glm warning
  LocalModel1 = LocalModel$new(mod, x.interest, k = k)
  dat = LocalModel1$results
  expect_equal(colnames(dat), c(expected.colnames, ".class"))
  expect_lte(nrow(dat), k * 3)  
  pred2 = predict(LocalModel1, iris[c(2,3),])
  expect_class(dat, "data.frame")
  expect_data_frame(pred2, nrows=2)
  expect_equal(colnames(pred2), c("setosa", "versicolor", "virginica"))
  
  p = plot(LocalModel1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})



