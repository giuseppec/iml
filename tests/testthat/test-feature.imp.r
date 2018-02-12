context('feature.imp()')


f = function(x, multi = FALSE){
  pred = unlist(x[1] + x[2] + 100 * (x[3] == 'a'))
  dat = data.frame(pred = pred)
  if(multi) dat$pred2 = 0.2 * dat$pred + 30
  dat
}
X = data.frame(a = c(1, 2, 3, 4, 5), 
  b = c(10, 20, 30, 40, 50), 
  c = factor(c("a", "b", "c", "a", "b")), 
  d = factor(c("A", "A", "B", "B", "B")))
f2 = function(x){f(x, TRUE)}
set.seed(42)
y = f(X) + rnorm(nrow(X))
y2 = factor(ifelse(X$b + X$a < 20, 'pred', 'pred2'))

test_that('feature.imp works for single output',{
  
  var.imp = feature.imp(f, X, y = y, loss = 'mse')
  dat = var.imp$data()
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  var.imp = feature.imp(f, X, y = y, loss = 'mse', method = 'cartesian')
  dat = var.imp$data()
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  X.exact = data.frame(x1 = c(1,2,3), x2 = c(9,4,2))
  f.exact = function(x){x[[1]]}
  y.exact = c(2,3,4)
  model.error = Metrics::mse(y.exact, f.exact(X.exact))
  cart.indices = c(1,1,2,2,3,3)
  cartesian.error = Metrics::mse(y.exact[cart.indices], c(2,3,1,3,1,2))
  
  var.imp = feature.imp(f.exact, X.exact, y = y.exact, loss = 'mse', method = 'cartesian')
  dat = var.imp$data()
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

test_that('feature.imp works for single output and function as loss',{
    
  var.imp = feature.imp(f, X, y = y, loss = Metrics::mse)
  dat = var.imp$data()
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that('feature.imp works for multiple output',{
  
  var.imp = feature.imp(f2, X, y = y2, loss = 'ce')
  dat = var.imp$data()
  expect_equal(colnames(dat), c("..feature", "error", "importance"))
  expect_equal(nrow(dat), ncol(X))  
  p = plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})