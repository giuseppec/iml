context("Interaction()")

test.interaction.range <- function(dat) {
  expect_numeric(dat$.interaction,
    lower = 0, upper = 1.000001,
    finite = TRUE, any.missing = FALSE
  )
}


test_that("Interaction works for all features", {
  inter.obj <- Interaction$new(predictor1)
  dat <- inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), ncol(X))
  test.interaction.range(dat)
  checkPlot(inter.obj)
})

test_that("Interaction works for 2-way interactions", {
  inter.obj <- Interaction$new(predictor1, feature = c("a"))
  dat <- inter.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".interaction"))
  expect_equal(nrow(dat), ncol(X) - 1)
  test.interaction.range(dat)
  checkPlot(inter.obj)
})

test_that("Interaction does not work for 2 or 3 features", {
  expect_error({
    inter.obj <- Interaction$new(predictor1, feature = c("a", "b"))
  })
  expect_error({
    inter.obj <- Interaction$new(predictor1, feature = c(1, 2))
  })
  expect_error({
    inter.obj <- Interaction$new(predictor1, feature = c("a", "b", "c"))
  })
  expect_error({
    inter.obj <- Interaction$new(predictor1, feature = c(1, 2, 3))
  })
})


test_that("Interaction for multiclass", {
  ia <- Interaction$new(predictor2)
  dat <- ia$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".feature", ".class", ".interaction"))
  expect_equal(nrow(dat), 2 * 4)
  checkPlot(ia)
})



test_that("h.test", {
  expect_equal(h.test(f.all = 1:10, f.j = rep(0, times = 10), f.no.j = 1:10), 0)
  expect_equal(h.test(f.all = 1:10, f.no.j = rep(0, times = 10), f.j = 1:10), 0)
  expect_equal(h.test(f.all = 1:10, f.j = 1:10, f.no.j = 1:10), 1)
  expect_number(h.test(f.all = 1:10, f.j = 1:10, f.no.j = c(1:6, 0, 0, 0, 0)), lower = 0, upper = 1)
})
