context("Shapley()")

test_that("Shapley works for single output and single feature",{
  
  x.interest = X[1,]
  
  set.seed(42)
  shap = Shapley$new(predictor1, X, x.interest, sample.size = 400)
  dat = shap$data()
  expect_equal(colnames(dat), c("feature", "phi", "phi.var"))
  expect_equal(nrow(dat), ncol(X))  
  expect_equal(sum(dat$phi), unlist(f(x.interest) - mean(f(X)$pred), use.names = FALSE), tolerance = 0.02)
  p = plot(shap)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  x.interest2 = X[2,]
  shap$explain(x.interest2)
  dat = shap$data()
  expect_equal(colnames(dat), c("feature", "phi", "phi.var"))
  expect_equal(nrow(dat), ncol(X))  
  expect_equal(sum(dat$phi), unlist(f(x.interest2) - mean(f(X)$pred), use.names = FALSE), tolerance = 0.02)
  
  shap = Shapley$new(predictor1, X,  sample.size = 400)
  shap$explain(x.interest)
  
})

test_that("Shapley works for multiple output",{
  
  x.interest = X[1,]
  
  set.seed(42)
  shap = Shapley$new(predictor2, X, x.interest, sample.size = 400)
  dat = shap$data()
  expect_equal(colnames(dat), c("feature", "class", "phi", "phi.var"))
  expect_equal(sum(dat$phi[dat$class == "pred"]), unlist(f(x.interest) - mean(f(X)$pred), use.names = FALSE), tolerance = 0.02)
  p = plot(shap)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})