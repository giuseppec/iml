test_that("get.feature.type", {
  f1 <- c("a", "b")
  expect_equal(get.feature.type(class(f1)), c("character" = "categorical"))
  expect_error(get.feature.type(data.frame(f1)))
  expect_equal(get.feature.type(class(factor(f1))), c("factor" = "categorical"))

  f2 <- 1:10
  expect_equal(
    get.feature.type(c(class(f1), class(f2))),
    c("character" = "categorical", "integer" = "numerical")
  )
})



test_that("probs.to.labels", {
  probs <- data.frame(a = c(0.1, 0.2, 0.7), b = c(0.9, 0.8, 0.3), c = c(0, 1, 0))
  labels <- data.frame(..class = factor(c("b", "c", "a")))
  expect_equal(probs.to.labels(probs), labels)
  pred <- data.frame(prediction = 1:10)
  expect_equal(probs.to.labels(pred), pred)
})




test_that("recode", {
  X <- data.frame(x1 = 1:10, x2 = letters[c(1, 1, 2, 2, 2, 2, 2, 2, 1, 1)], x3 = rep(0, 10), x4 = factor("c"))
  X.recode <- recode(X, X[1, ])
  expect_equal(dim(X), dim(X.recode))
  expect_equal(rownames(X), rownames(X.recode))
  expect_equal(X.recode$x1, X$x1)
  expect_equal(X.recode$x3, X$x3)
  expect_equal(X.recode$x2, c(1, 1, 0, 0, 0, 0, 0, 0, 1, 1))
  expect_equal(X.recode$x4, rep(1, 10))
  expect_equal(colnames(X.recode), c("x1", "x2=a", "x3", "x4=c"))
})


test_that("get.grid.1D", {
  # testing some with explicit feature type, some without
  # type equidist
  expect_equal(get.grid.1D(1:10, feature.type = "numerical", grid.size = 4), c(1, 4, 7, 10))
  expect_equal(length(get.grid.1D(1:10, feature.type = "numerical", 4)), 4)
  expect_equal(get.grid.1D(1:10, feature.type = "categorical", 1), 1:10)
  expect_equal(get.grid.1D(letters, feature.type = "categorical", 4), letters)
  expect_error(get.grid.1D(letters, feature.type = "numerical", 4))
  expect_equal(get.grid.1D(1:10, 2), c(1, 10))
  expect_equal(get.grid.1D(c(NA, 1:10), feature.type = "numerical", 2), c(1, 10))
  expect_equal(get.grid.1D(c(NaN, 1:10), feature.type = "numerical", 2), c(1, 10))
  expect_equal(get.grid.1D(c(Inf, 1:10), feature.type = "numerical", 2), c(1, 10))
  expect_equal(get.grid.1D(c(-Inf, 1:10), feature.type = "numerical", 2), c(1, 10))

  # type quantile
  expect_equal(get.grid.1D(1:10, feature.type = "numerical", type = "quantile", grid.size = 2), c(1, 10))
  expect_equal(get.grid.1D(rep(0, times = 100), feature.type = "numerical", type = "quantile", grid.size = 2), c(0, 0))
  expect_equal(get.grid.1D(c(rep(0, times = 90), 1:10), type = "quantile", grid.size = 10), c(rep(0, 9), 10))
  expect_equal(get.grid.1D(c(rep(0, times = 90), 1:10), type = "quantile", grid.size = 20), c(rep(0, 18), c(5, 10)))
  expect_equal(get.grid.1D(1:10, feature.type = "numerical", type = "quantile", grid.size = 3), c(1, 5, 10))
  expect_equal(get.grid.1D(c(-Inf, 1:10), type = "quantile", 2), c(1, 10))
  # type should be ignored when feature is categorical
  expect_equal(get.grid.1D(c("a", "b", "a"), type = "quantile", 2), c("a", "b"))
  expect_equal(get.grid.1D(factor(c("a", "b", "a")), type = "quantile", 2), factor(c("a", "b")))
})

test_that("get.grid", {
  # testing some with explicit feature type, some without
  # type equidist

  grid1d.numerical1 <- get.grid(cars[, 1, drop = FALSE], grid.size = 10)
  expect_equal(grid1d.numerical1, data.table(speed = seq(from = min(cars$speed), to = max(cars$speed), length.out = 10)))
  grid1d.numerical2 <- get.grid(cars[, 2, drop = FALSE], grid.size = 5)
  expect_equal(grid1d.numerical2, data.table(dist = seq(from = min(cars$dist), to = max(cars$dist), length.out = 5)))
  grid2d <- get.grid(cars, grid.size = c(10, 5))

  grid1d.numerical1 <- get.grid(cars[, 1, drop = FALSE], grid.size = 10, anchor.value = -100)
  expect_equal(grid1d.numerical1, data.table(speed = c(-100, seq(from = min(cars$speed), to = max(cars$speed), length.out = 10))))
})

test_that("is_label", {
  expect_equal(is_label(letters), TRUE)
  expect_equal(is_label(as.factor(letters)), TRUE)
  expect_equal(is_label(1:10), FALSE)
  expect_equal(is_label(c(NA, 1:10)), FALSE)
  expect_equal(is_label(iris), FALSE)
})


test_that("order_levels", {
  set.seed(42)
  n <- 100
  x1 <- factor(sample(c("a", "b", "c", "d"), size = n, replace = TRUE))
  # a and c similar, b and d and slightly a and d
  dat <- data.table(
    x1 = x1,
    x2 = rnorm(n, mean = 1 * x1 %in% c("a", "c")),
    x3 = rnorm(n, mean = 1 * x1 %in% c("b", "d")),
    x4 = sample(1:4, size = n, replace = TRUE)
  )
  ord <- order_levels(dat, "x1")
  expect_equal(ord, c(4, 2, 1, 3))
  expect_error(order_levels(cars, "dist"))
  expect_error(order_levels(dat, "x10"))
})

test_that("get_layout", {
  expect_equal(get_layout(10), list(nrows = 4, ncols = 3))
  expect_equal(get_layout(1), list(nrows = 1, ncols = 3))
  expect_equal(get_layout(10, ncols = 10), list(nrows = 1, ncols = 10))
  expect_equal(get_layout(4, ncols = 10), list(nrows = 1, ncols = 10))
  expect_equal(get_layout(100, nrows = 2), list(nrows = 2, ncols = 50))
  expect_equal(get_layout(7, nrows = 2), list(nrows = 2, ncols = 4))
  expect_equal(get_layout(7, nrows = 2, ncols = 1), list(nrows = 2, ncols = 1))
  expect_equal(get_layout(2, nrows = 2, ncols = 2), list(nrows = 2, ncols = 2))
  expect_equal(get_layout(200, nrows = 6, ncols = 2), list(nrows = 6, ncols = 2))
})
