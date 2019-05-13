context("MarginalEffects()")


test_that("MarginalEffects general", {
  me = MarginalEffects$new(predictor1, feature = 1, step.size = 1)
  dat = me$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
})


