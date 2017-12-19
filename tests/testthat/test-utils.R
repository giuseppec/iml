context('Test utility functions')


test_that('get.feature.type function works', {
  
  f1 = c('a', 'b')
  expect_equal(get.feature.type(f1), 'categorical')
  expect_error(get.feature.type(data.frame(f1)))
  expect_equal(get.feature.type(factor(f1)), 'categorical')
  
  f2 = 1:10
  expect_equal(get.feature.type(f2), 'numerical')
})