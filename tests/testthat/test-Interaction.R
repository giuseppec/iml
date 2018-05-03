context("Interaction()")

test.interaction.range = function(dat) {
  expect_numeric(dat$.interaction, lower = 0, upper = 1, 
    finite = TRUE, any.missing = FALSE)
}

test_that("Interaction works for single output and single feature", {
  grid.size = 10
  inter.obj = Interaction$new(predictor1, features = 1, grid.size = grid.size)
  dat = inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), 1)  
  test.interaction.range(dat)
  checkPlot(inter.obj)
})


test_that("Interaction works for all features", {
  inter.obj = Interaction$new(predictor1)
  dat = inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), ncol(X))  
  test.interaction.range(dat)
  checkPlot(inter.obj)
})

test_that("Interaction works for 2 features", {
  inter.obj = Interaction$new(predictor1, features = c("a", "b"))
  dat = inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), 1)  
  test.interaction.range(dat)
  checkPlot(inter.obj)
})

test_that("Interaction does not work for 3 features", {
  expect_error({inter.obj = Interaction$new(predictor1, features = c("a", "b", "c"))})
  expect_error({inter.obj = Interaction$new(predictor1, features = c(1, 2, 3))})
})



test_that("intervene.interaction", {
  dataSample = data.table(a = 1:10, b = 1:10, c = 1:10)
  feature.name = "b"
  grid.size = 3
  
  dat = intervene.interaction(dataSample, feature.name, grid.size) 
  expect_data_table(dat, any.missing = FALSE, nrows = 3 + 3*10*2)
  expect_equal(colnames(dat), c("b", "a", "c", ".id", ".type"))
  expect_equal(unique(dat$.type), c("j", "no.j", "f"))
  expect_equal(unique(dat$.id), 1:3)
  expect_equal(sort(unique(dat$a)), 1:10)
  expect_equal(sort(unique(dat$b)), 1:10)
  expect_equal(sort(unique(dat$c)), 1:10)
  dat.agg = aggregate.interaction(dat, data.frame(pred = rnorm(n = nrow(dat))), features = c("a")) 
  expect_equal(colnames(dat.agg), c(".feature", ".interaction"))
})


test_that("h.test", {
  expect_equal(h.test(f.all = 1:10, f.j = rep(0, times = 10), f.no.j = 1:10), 0)
  expect_equal(h.test(f.all = 1:10, f.no.j = rep(0, times = 10), f.j = 1:10), 0)
  expect_equal(h.test(f.all = 1:10, f.j = 1:10, f.no.j = 1:10), 1)
  expect_number(h.test(f.all = 1:10, f.j = 1:10, f.no.j = c(1:6, 0, 0, 0, 0)), lower = 0, upper = 1)
})

test_that("generate.marginals", {
  n = 20
  dat = data.table(x1 = rnorm(n),
  x2 = rnorm(n), x3 = rnorm(n),
  x4 = rnorm(n), x5 = rnorm(n), x6 = rnorm(n))
  f = function(dat) {
  dat$x1 * dat$x3 + dat$x1 * dat$x3
  }
  dat1 = generate.marginals(dat, dat, "x1")
  expect_data_table(dat1, any.missing = FALSE, nrows = n * n)
  expect_equal(colnames(dat1), c(colnames(dat), ".id"))
  sds = dat1[,lapply(.SD, var), by = ".id"]
  expect_equal(colMeans(sds)["x1"], c(x1 = 0))
  expect_true(all(colMeans(sds)[-which(colnames(sds) == "x1")] > 0))
  
  expect_error({generate.marginals(dat, dat, c("x2", "x2"))})
  
  dat2 = generate.marginals(dat, dat, c("x2", "x3"))
  
  expect_data_table(dat2, any.missing = FALSE, nrows = n * n)
  expect_equal(sort(colnames(dat2)), sort(c(colnames(dat), ".id")))
  sds = dat2[,lapply(.SD, var), by = ".id"]
  expect_equal(colMeans(sds)[c("x2", "x3")], c(x2 = 0, x3 = 0))
  expect_true(all(colMeans(sds)[-which(colnames(sds) %in% c("x2", "x3"))] > 0))
})








