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