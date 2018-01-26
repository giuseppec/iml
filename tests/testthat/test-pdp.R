context('pdp()')


f = function(x, multi = FALSE){
  pred = unlist(x[1] + x[2] + 100 * (x[3] == 'a'))
  dat = data.frame(pred = pred)
  if(multi) dat$pred2 = 0.2 * dat$pred + 7 
  dat
}
X = data.frame(a = c(1, 2, 3, 4, 5), 
  b = c(10, 20, 30, 40, 50), 
  c = factor(c("a", "b", "c", "a", "b")), 
  d = factor(c("A", "A", "B", "B", "B")))


test_that('pdp works for single output and single feature',{
  
  grid.size = 10
  pdp.obj = pdp(f, X, feature = 1, grid.size = grid.size)
  dat = pdp.obj$data()
  expect_equal(colnames(dat), c("a", "y.hat"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that('pdp works for single output and 2 features, 2D grid.size',{
  
  ## two numerical features with 2 grid.sizes
  grid.size = c(10,2)
  pdp.obj = pdp(f, X, feature = c(1,2), grid.size = grid.size)
  dat = pdp.obj$data()
  expect_equal(colnames(dat), c("a", "b", "y.hat"))
  expect_equal(nrow(dat), grid.size[1] * grid.size[2])  
  expect_equal(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that('pdp works for single output and 2 numerical features, 1D grid.size',{
  
  
  ## Two numerical with same grid.size
  grid.size = 10
  pdp.obj = pdp(f, X, feature = c(1,2), grid.size = grid.size)
  dat = pdp.obj$data()
  expect_equal(colnames(dat), c("a", "b", "y.hat"))
  expect_equal(nrow(dat), grid.size * grid.size)  
  expect_equal(nrow(unique(dat)), grid.size * grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})
  
test_that('pdp works for single output and numerical + categorical feature',{
  
  ## One feature categorical
  grid.size = 11
  pdp.obj = pdp(f, X, feature = c(1,3), grid.size = grid.size)
  dat = pdp.obj$data()
  expect_equal(colnames(dat), c("a", "c", "y.hat"))
  expect_equal(nrow(dat), grid.size[1] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size * length(unique(X[,3])))  
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(unique(dat$c), unique(X$c))  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  
  ## One feature categorical
  grid.size = c(7,9)
  pdp.obj = pdp(f, X, feature = c(3,2), grid.size = grid.size)
  dat = pdp.obj$data()
  expect_equal(colnames(dat), c("c", "b", "y.hat"))
  expect_equal(nrow(dat), grid.size[2] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size[2] * length(unique(X[,3])))  
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  expect_equal(unique(dat$c), unique(X$c))  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  
})

test_that('pdp works for multiple output',{
  
  grid.size = 10
  pdp.obj = pdp(function(x){f(x, multi=TRUE)}, X, feature = c(1), grid.size = grid.size)
  dat = pdp.obj$data()
  expect_equal(colnames(dat), c("a", "..class.name", "y.hat"))
  expect_equal(nrow(dat), grid.size * 2)  
  expect_equal(nrow(unique(dat)), grid.size * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  
  p = plot(pdp.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})