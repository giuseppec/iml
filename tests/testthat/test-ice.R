context('ICE')


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


test_that('ice works for single output and single feature',{
  
  grid.size = 10
  ice.obj = ice(f, X, feature = 1, grid.size = grid.size)
  dat = ice.obj$data()
  expect_equal(colnames(dat), c("a", "..individual", "y.hat", "..class.name"))
  expect_equal(nrow(dat), grid.size * nrow(X))  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  p = plot(ice.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})

test_that('ice works for multiple output',{
  
  grid.size = 10
  ice.obj = ice(f, X, feature = c(1), grid.size = grid.size, predict.args = list(multi = TRUE))
  dat = ice.obj$data()
  expect_equal(colnames(dat), c("a", "..individual", "y.hat", "..class.name"))
  expect_equal(nrow(dat), grid.size * nrow(X))  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  
  p = plot(ice.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})


test_that('centered ice works for multiple output',{
  
  grid.size = 10
  ice.obj = ice(f, X, feature = c(1), center = 10, grid.size = grid.size, predict.args = list(multi = TRUE))
  dat = ice.obj$data()
  expect_equal(colnames(dat), c("a", "..individual", "y.hat", "..class.name"))
  expect_equal(nrow(dat), (grid.size + 1) * nrow(X))  
  expect_equal(nrow(unique(dat)), (grid.size + 1) * nrow(X))
  expect_equal(max(dat$a), 10)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$y.hat), 0)
  p = plot(ice.obj)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
})
