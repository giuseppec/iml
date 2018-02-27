context("Lime()")

set.seed(1)
n = 100
expected.colnames = c("beta", "x.recoded", "effect", "x.original", "feature", "feature.value")

test_that("Lime works for single output and single feature", {
  
  x.interest = X[2,]
  k = 2
  set.seed(42)
  lime1 = Lime$new(predictor1, X, x.interest=x.interest, k = k)
  dat = lime1$results
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
  
  x.interest = X[1,]
  
  k = 1
  set.seed(42)
  # rbind X a few times to eliminate glm warning
  lime1 = Lime$new(predictor2, rbind(X, X, X, X), x.interest, k = k)
  dat = lime1$results
  expect_equal(colnames(dat), c(expected.colnames, "..class"))
  expect_lte(nrow(dat), k * 2)  
  pred2 = predict(lime1, X[c(2,3),])
  expect_data_frame(pred2, nrows=2)
  expect_equal(colnames(pred2), c("pred", "pred2"))
  
  p = plot(lime1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})



