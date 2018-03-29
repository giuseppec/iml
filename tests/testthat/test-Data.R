context("Data")
ds = Data$new(iris)
sample1 = ds$sample(3)
n = 100
set.seed(42)
sample1.weighted = ds$sample(n, prob = c(2,1, rep(0, times = nrow(iris)- 2)))

ds.weighted = Data$new(iris, prob = c(2,1, rep(0, times = nrow(iris)- 2)))
set.seed(42)

sample2 = ds.weighted$sample(n)

test_that("samplings works",{
  expect_class(sample1, "data.frame")
  expect_equal(colnames(sample1), colnames(iris))
  expect_equal(ds$get.x(), data.table(iris))
  expect_equal(ds$feature.names, colnames(iris))
  expect_equal(ds$feature.types, c(Sepal.Length="numerical", 
    Sepal.Width = "numerical", Petal.Length = "numerical", Petal.Width =  "numerical", 
    Species = "categorical"))
  expect_equal(ds$n.features, 5)

})

test_that("weighted sampling works", {
  expect_equal(nrow(unique(sample1.weighted)), 2)
  expect_equal(nrow(unique(sample2)), 2)
  expect_equal(sample1.weighted, sample2)
})

