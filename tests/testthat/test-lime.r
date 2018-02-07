context('lime()')


f = function(x, multi = FALSE){
  pred = unlist(5  + 20 * x[1] - 10 * x[2] + 100 * (x[3] == 'a')) 
  dat = data.frame(pred = pred)
  if(multi) dat$pred2 = 1 - dat$pred
  dat
}

f2 = function(x) f(x, multi = TRUE)
set.seed(123)
n = 100
X = data.frame(
  x1 = rnorm(n), 
  x2 = runif(n), 
  x3 = sample(c('a', 'b','c'), size = n, replace=TRUE), 
  x4 = sample(c('A', 'B', 'C'), size = n, replace=TRUE))

y = f(X)[[1]]

expected.colnames = c("beta", "x.scaled", "effect", "x.original", "feature", "feature.value")

test_that('lime works for single output and single feature',{
  
  x.interest = X[2,]
  k = 2
  set.seed(42)
  lime1 = lime(f, X, x.interest=x.interest, sample.size = 100, k = k)
  dat = lime1$data()
  expect_equal(colnames(dat), expected.colnames)
  expect_true("x3=a" %in% dat$feature)
  expect_equal(nrow(dat), k)
  p = plot(lime1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  x.interest2 = X[4,]
  lime1$x = x.interest2
  dat = lime1$data()
  expect_equal(colnames(dat), expected.colnames)
  expect_equal(nrow(dat), k)  

  pred = predict(lime1, newdata = X[3:4,])
  expect_data_frame(pred, nrows = 2)
  expect_equal(colnames(pred), 'prediction')
})

test_that('lime works for multiple output',{
  
  x.interest = X[1,]
  
  k = 3
  set.seed(42)
  lime1 = lime(f2, X, x.interest, sample.size = 400, k = k)
  dat = lime1$data()
  expect_equal(colnames(dat), c(expected.colnames, '..class'))
  expect_equal(nrow(dat), k * 2)  
  pred2 = predict(lime1, X[c(2,3),])
  expect_data_frame(pred2, nrows=2)
  expect_equal(colnames(pred2), c('pred', 'pred2'))
  
  p = plot(lime1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

