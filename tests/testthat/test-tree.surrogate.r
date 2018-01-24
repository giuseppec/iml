context('Tree Surrogate')


f = function(x, multi = FALSE){
  pred = unlist(x[1] + x[2] + 100 * (x[3] == 'a')) / (155)
  dat = data.frame(pred = pred)
  if(multi) dat$pred2 = 1 - dat$pred
  dat
}

f2 = function(x) f(x, multi = TRUE)
X = data.frame(a = c(1, 2, 3, 4, 5), 
  b = c(10, 20, 30, 40, 50), 
  c = factor(c("a", "b", "c", "a", "b")), 
  d = factor(c("A", "A", "B", "B", "B")))


test_that('tree.surrogate works for single output and single feature',{
  
  sample.size = 100
  tree = tree.surrogate(f, X, sample.size = sample.size)
  dat = tree$data()
  expect_equal(colnames(dat), c(colnames(X), "..node", "..path", "..y.hat", "..y.hat.tree"))
  expect_equal(nrow(dat), sample.size)  
  p = plot(tree)
  expect_s3_class(p, c("gg", "ggplot"))
  expect_s3_class(tree$predict(X), c('data.frame'))
  p
  
})

test_that('tree.surrogate works for multiple output',{
  sample.size = 50
  tree = tree.surrogate(f2, X, sample.size = sample.size)
  dat = tree$data()
  expect_equal(colnames(dat), c(colnames(X), "..node", "..path", "..class",  "..y.hat", "..y.hat.tree"))
  expect_equal(nrow(dat), sample.size * 2)  
  p = plot(tree)
  expect_s3_class(p, c("gg", "ggplot"))
  expect_s3_class(tree$predict(X), c('data.frame'))
  p
  expected.prediction = data.frame(pred = c(1, 0, 0, 1, 0), pred2 = c(0,1,1,0,1))
  actual.prediction = predict(tree, X)

  expect_s3_class(actual.prediction, 'data.frame')
  expect_equal(expected.prediction, actual.prediction, check.attributes = FALSE)
  
  actual.classification = predict(tree, X[1:2,], type = 'class')
  expect_equal(colnames(actual.classification), '..class')
  expect_equal(actual.classification[[1]], factor(c('pred', 'pred2')))
  
})

test_that('tree.surrogate works for multiple output with selected class',{
  sample.size = 50
  tree = tree.surrogate(f2, X, sample.size = sample.size, class = 1)
  dat = tree$data()
  expect_equal(colnames(dat), c(colnames(X), "..node", "..path",  "..y.hat", "..y.hat.tree"))
  expect_equal(nrow(dat), sample.size )  
  p = plot(tree)
  expect_s3_class(p, c("gg", "ggplot"))
  expect_s3_class(tree$predict(X), c('data.frame'))
  p
  pred = predict(tree, newdata = X[1:10, ])
  expect_equal(dim(pred), c(10, 1))
  expect_s3_class(pred, 'data.frame')
})
