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








