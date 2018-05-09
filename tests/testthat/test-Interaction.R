context("Interaction()")

test.interaction.range = function(dat) {
  expect_numeric(dat$.interaction, lower = 0, upper = 1, 
    finite = TRUE, any.missing = FALSE)
}


test_that("Interaction works for all features", {
  inter.obj = Interaction$new(predictor1)
  dat = inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), ncol(X))  
  test.interaction.range(dat)
  checkPlot(inter.obj)
})

test_that("Interaction works for 2-way interactions", {
  inter.obj = Interaction$new(predictor1, feature = c("a"))
  dat = inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), ncol(X) - 1)  
  test.interaction.range(dat)
  checkPlot(inter.obj)
})

test_that("Interaction does not work for 2 or 3 features", {
  expect_error({inter.obj = Interaction$new(predictor1, feature = c("a", "b"))})
  expect_error({inter.obj = Interaction$new(predictor1, feature = c(1, 2))})
  expect_error({inter.obj = Interaction$new(predictor1, feature = c("a", "b", "c"))})
  expect_error({inter.obj = Interaction$new(predictor1, feature = c(1, 2, 3))})
})


test_that("Interaction for multiclass", {
  ia = Interaction$new(predictor2)
  dat = ia$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".class", ".interaction"))
  expect_equal(nrow(dat), 2 * 4)  
  checkPlot(ia)
})



test_that("intervene.interaction", {
  dataSample = data.table(a = 1:10, b = 1:10, c = 1:10)
  feature.name = "b"
  grid.size = 3
  
  dat = intervene.interaction.multi(dataSample, feature.name, grid.size = grid.size) 
  expect_data_table(dat, any.missing = FALSE, nrows = grid.size * nrow(dataSample) * 3 * 2)
  expect_equal(colnames(dat), c("a", "b", "c", ".id", ".type", ".feature"))
  expect_equal(unique(dat$.type), c("jk", "j", "k"))
  expect_equal(unique(dat$.id), 1:3)
  expect_equal(sort(unique(dat$a)), 1:10)
  expect_equal(sort(unique(dat$b)), 1:10)
  expect_equal(sort(unique(dat$c)), 1:10)
  dat.agg = aggregate.interaction(dat, data.frame(pred = rnorm(n = nrow(dat))), feature = c("a")) 
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
  
  dat3 = generate.marginals(dat, dat, c("x2"), n.sample.dist = 2)
  expect_data_table(dat3, any.missing = FALSE, nrows = n * 2)
  expect_equal(sort(colnames(dat3)), sort(c(colnames(dat), ".id")))
  sds = dat3[,lapply(.SD, var), by = ".id"]
  expect_equal(colMeans(sds)[c("x2")], c(x2 = 0))
  expect_true(all(colMeans(sds)[-which(colnames(sds) %in% c("x2"))] > 0))
})








