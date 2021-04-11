library(data.table)
mg.dat <- data.table(
  a = 1:10,
  b = c("x", "y", "z", "x", "x", "x", "z", "z", "y", "y"),
  c = rep(c(1, 2), times = 5),
  d = rep(c(1, 2), each = 5)
)

set.seed(42)


test_that("MarginalGenerator$all()", {
  mg <- MarginalGenerator$new(grid.dat = mg.dat, n.sample.dist = 2, dist.dat = mg.dat, features = c("a", "b"))
  res <- mg$all()
  expect_data_table(res, nrows = nrow(mg.dat) * 2)
  expect_equal(table(res$a), table(mg.dat$a) * 2)
  expect_equal(table(res$b), table(mg.dat$b) * 2)
  expect_equal(sort(colnames(res)), sort(c(colnames(mg.dat), c(".id"))))
  expect_equal(table(table(res$.id))[[1]], nrow(mg.dat))
  expect_equal(res$a, rep(mg.dat$a, each = 2))

  expect_null(mg$all())
})

test_that("MarginalGenerator$all()with cartesian n.sample.dist", {
  mg <- MarginalGenerator$new(grid.dat = mg.dat, dist.dat = mg.dat, features = c("a", "b"), cartesian = TRUE, id.dist = TRUE)
  res <- mg$all()
  expect_data_table(res, nrows = nrow(mg.dat) * nrow(mg.dat))
  expect_equal(table(res$a), table(mg.dat$a) * nrow(mg.dat))
  expect_equal(table(res$b), table(mg.dat$b) * nrow(mg.dat))
  expect_equal(sort(colnames(res)), sort(c(colnames(mg.dat), c(".id", ".id.dist"))))
  expect_equal(table(table(res$.id))[[1]], nrow(mg.dat))
  expect_equal(res$a, rep(mg.dat$a, each = nrow(mg.dat)))


  mg <- MarginalGenerator$new(grid.dat = mg.dat, n.sample.dist = 200, dist.dat = mg.dat, features = c("a", "b"), id.dist = TRUE)
  res <- mg$all()
  expect_data_table(res, nrows = nrow(mg.dat) * nrow(mg.dat))
  expect_equal(table(res$a), table(mg.dat$a) * nrow(mg.dat))
  expect_equal(table(res$b), table(mg.dat$b) * nrow(mg.dat))
  expect_equal(sort(colnames(res)), sort(c(colnames(mg.dat), c(".id", ".id.dist"))))
  expect_equal(table(table(res$.id))[[1]], nrow(mg.dat))
  expect_equal(res$a, rep(mg.dat$a, each = nrow(mg.dat)))
})


test_that("MarginalGenerator$all() different grid", {
  grid <- data.table(expand.grid(a = 1:10, b = c("x", "y", "z")))
  expect_error({
    MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a", "b", "c"))
  })
  expect_error({
    MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a", "c"))
  })
  mg <- MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a", "b"), cartesian = TRUE)
  res <- mg$all()
  expect_data_table(res, nrows = nrow(grid) * nrow(mg.dat))
  expect_equal(table(res$a), table(grid$a) * nrow(mg.dat))
  expect_equal(table(res$b), table(grid$b) * nrow(mg.dat))
  expect_equal(sort(colnames(res)), sort(c(colnames(mg.dat), c(".id"))))
  expect_equal(table(table(res$.id))[[1]], nrow(grid))
})


test_that("MarginalGenerator$all() repetitions", {
  grid <- data.table(expand.grid(a = 1:10, b = c("x", "y", "z")))
  mg <- MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a"), n.sample.dist = 1)
  res <- mg$all()
  expect_data_table(res, nrows = nrow(grid))
  expect_equal(table(res$a), table(grid$a))
  expect_equal(res$a, grid$a)

  mg <- MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a"), n.sample.dist = 2)
  res <- mg$all()
  expect_data_table(res, nrows = nrow(grid) * 2)
  expect_equal(table(res$a), table(grid$a) * 2)
  expect_equal(res$a, rep(grid$a, each = 2))


  mg <- MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a", "b"), n.sample.dist = 5)
  res <- mg$all()
  expect_data_table(res, nrows = nrow(grid) * 5)
  expect_equal(table(res$a), table(grid$a) * 5)
  expect_equal(res$a, rep(grid$a, each = 5))
})


test_that("MarginalGenerator$batch()", {
  grid <- data.table(expand.grid(a = 1:10))
  mg <- MarginalGenerator$new(grid.dat = grid, dist.dat = mg.dat, features = c("a"), cartesian = TRUE)
  expect_error(mg$next.batch())
  res <- mg$next.batch(10)
  expect_data_table(res, nrows = 10)
  expect_equal(unique(res$a), 1)
  expect_equal(sort(colnames(res)), sort(c(colnames(mg.dat), c(".id"))))


  res <- mg$next.batch(1)
  expect_data_table(res, nrows = 1)
  expected_res <- data.table(cbind(mg.dat[1, ]), .id = 1, .id.dist = 1)
  expect_data_table(res, nrows = 1)


  mg$next.batch(88)
  res <- mg$next.batch(2)
  expect_data_table(res, nrows = 1)
  expect_null(mg$next.batch(1))
})
