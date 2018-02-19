context("makeShapley()")


f = function(x, multi = FALSE) {
  pred = unlist(x[1] + x[2] + 100 * (x[3] == "a")) / (155)
  dat = data.frame(pred = pred)
  if (multi) dat$pred2 = 1 - dat$pred
  dat
}

f2 = function(x) f(x, multi = TRUE)
X = data.frame(a = c(1, 2, 3, 4, 5), 
  b = c(10, 20, 30, 40, 50), 
  c = factor(c("a", "b", "c", "a", "b")), 
  d = factor(c("A", "A", "B", "B", "B")))


test_that("makeShapley works for single output and single feature",{
  
  x.interest = X[1,]
  
  set.seed(42)
  shap = makeShapley(f, X, x.interest, sample.size = 400)
  dat = shap$data()
  expect_equal(colnames(dat), c("feature", "phi", "phi.var"))
  expect_equal(nrow(dat), ncol(X))  
  expect_equal(sum(dat$phi), unlist(f(x.interest) - mean(f(X)$pred), use.names = FALSE), tolerance = 0.02)
  p = plot(shap)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  x.interest2 = X[2,]
  shap$x = x.interest2
  dat = shap$data()
  expect_equal(colnames(dat), c("feature", "phi", "phi.var"))
  expect_equal(nrow(dat), ncol(X))  
  expect_equal(sum(dat$phi), unlist(f(x.interest2) - mean(f(X)$pred), use.names = FALSE), tolerance = 0.02)
  
})

test_that("makeShapley works for multiple output",{
  
  x.interest = X[1,]
  
  set.seed(42)
  shap = makeShapley(f2, X, x.interest, sample.size = 400)
  dat = shap$data()
  expect_equal(colnames(dat), c("feature", "class", "phi", "phi.var"))
  expect_equal(sum(dat$phi[dat$class == "pred"]), unlist(f(x.interest) - mean(f(X)$pred), use.names = FALSE), tolerance = 0.02)
  p = plot(shap)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})