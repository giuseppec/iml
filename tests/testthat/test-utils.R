context('utility functions')


test_that('get.feature.type function works', {
  
  f1 = c('a', 'b')
  expect_equal(get.feature.type(class(f1)), c('character'='categorical'))
  expect_error(get.feature.type(data.frame(f1)))
  expect_equal(get.feature.type(class(factor(f1))), c('factor'='categorical'))
  
  f2 = 1:10
  expect_equal(get.feature.type(c(class(f1), class(f2))), 
    c('character'='categorical', 'integer'='numerical'))
})



test_that('probs.to.labels',{
  
  probs = data.frame(a = c(0.1, 0.2, 0.7), b = c(0.9, 0.8, 0.3), c = c(0,1,0))
  labels = data.frame(..class = c('b', 'c', 'a'))
  expect_equal(probs.to.labels(probs), labels)
  pred = data.frame(prediction = 1:10)
  expect_equal(probs.to.labels(pred), pred)
})




test_that('recode.data',{
  X = data.frame(x1=1:10, x2=letters[c(1,1,2,2,2,2,2,2,1,1)], x3=rep(0, 10), x4=factor('c'))
  X.recode = recode.data(X, X[1,])
  expect_equal(dim(X), dim(X.recode))
  expect_equal(rownames(X), rownames(X.recode))
  expect_equal(X.recode$x1, X$x1)
  expect_equal(X.recode$x3, X$x3)
  expect_equal(X.recode$x2, c(1,1,0,0,0,0,0,0,1,1))
  expect_equal(X.recode$x4, rep(1,10))
  expect_equal(colnames(X.recode), c('x1', 'x2=a', 'x3', 'x4=c'))
})
