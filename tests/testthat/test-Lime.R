context("Lime()")

set.seed(12)
expected.colnames = c("beta", "x.recoded", "effect", "x.original", "feature", "feature.value")

test_that("Lime works for single output and single feature", {
  
  x.interest = X[2,]
  k = 2
  set.seed(42)
  lime1 = Lime$new(predictor1, X, x.interest=x.interest, k = k)
  dat = lime1$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)
  p = plot(lime1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  x.interest2 = X[4,]
  lime1$explain(x.interest2)
  dat = lime1$results
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)  

  pred = predict(lime1, newdata = X[3:4,])
  expect_data_frame(pred, nrows = 2)
  expect_equal(colnames(pred), "prediction")
})

test_that("Lime works for multiple output", {
  
  library('rpart')
  clf = rpart(Species ~ ., data = iris)
  mod = Predictor$new(clf)
  x.interest = iris[1,]
  k = 1
  set.seed(42)
  # rbind X a few times to eliminate glm warning
  lime1 = Lime$new(mod, iris, x.interest, k = k)
  dat = lime1$results
  expect_equal(colnames(dat), c(expected.colnames, "..class"))
  expect_lte(nrow(dat), k * 3)  
  pred2 = predict(lime1, iris[c(2,3),])
  expect_class(dat, "data.frame")
  expect_data_frame(pred2, nrows=2)
  expect_equal(colnames(pred2), c("setosa", "versicolor", "virginica"))
  
  p = plot(lime1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})



