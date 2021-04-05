ds <- Data$new(iris)
sample1 <- ds$sample(3)
n <- 100
set.seed(42)
sample1.weighted <- ds$sample(n, prob = c(2, 1, rep(0, times = nrow(iris) - 2)))

ds.weighted <- Data$new(iris, prob = c(2, 1, rep(0, times = nrow(iris) - 2)))
set.seed(42)

sample2 <- ds.weighted$sample(n)

test_that("samplings works", {
  expect_class(sample1, "data.frame")
  expect_equal(colnames(sample1), colnames(iris))
  expect_equal(ds$get.x(), data.table(iris))
  expect_equal(ds$feature.names, colnames(iris))
  expect_equal(ds$feature.types, c(
    Sepal.Length = "numerical",
    Sepal.Width = "numerical", Petal.Length = "numerical", Petal.Width = "numerical",
    Species = "categorical"
  ))
  expect_equal(ds$n.features, 5)
})

test_that("weighted sampling works", {
  expect_equal(nrow(unique(sample1.weighted)), 2)
  expect_equal(nrow(unique(sample2)), 2)
  expect_equal(sample1.weighted, sample2)
})


test_that("y as character works", {
  ds <- Data$new(iris, y = "Species")
  expect_equal(ds$y, iris["Species"])
  expect_equal(ds$X, data.table::data.table(iris[names(iris) != "Species"]))
  expect_equal(ds$get.xy(), data.table::data.table(iris))
  expect_equal(ds$get.x(), data.table::data.table(iris[names(iris) != "Species"]))
  expect_equal(ds$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(unname(ds$feature.types), rep("numerical", times = 4))
})


test_that("only first class used", {
  data("diamonds")
  x = Data$new(X = diamonds)
  expect_true(!any(is.na(names(x$feature.types))))
})
