context("MarginalEffects()")

test_that("MarginalEffects general", {
  me = MarginalEffects$new(predictor1, feature = 1, step.size = 1)
  dat = me$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c("a", ".meffect"))
  checkPlot(me)
})

test_that("MarginalEffects for lm", {
  dat = cars
  dat$x = 1:nrow(cars)
  mod = lm(dist ~ ., data = dat)
  pred = Predictor$new(mod, dat)
  me = MarginalEffects$new(pred, feature = "speed", step.size = 1)
  cf = unname(coef(mod)["speed"])
  expect_equal(c("pred" = cf), me$ame)
  expect_equal(cf, min(me$results$.meffect))
  expect_equal(cf, max(me$results$.meffect))

  me = MarginalEffects$new(pred, feature = "speed", step.size = 0.5)
  cf = unname(coef(mod)["speed"])
  expect_equal(c("pred" = cf), 2 * me$ame)
  expect_equal(cf, 2 * min(me$results$.meffect))
  expect_equal(cf, 2 * max(me$results$.meffect))
})

test_that("MarginalEffects for multiple features", {
  dat = cars
  dat$x = 1:nrow(cars)
  mod = lm(dist ~ ., data = dat)
  pred = Predictor$new(mod, dat)
  me = MarginalEffects$new(pred, feature = c("speed", "x"), step.size = c(1, 2))
  dat = me$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c("speed", "x", ".meffect"))
  checkPlot(me)
  cf = 1 * unname(coef(mod)["speed"]) + 2 * unname(coef(mod)["x"])
  names(cf) = "pred"
})

test_that("MarginalEffects for multiclass", {
  me = MarginalEffects$new(predictor2, feature = 1, step.size = 1)
  dat = me$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c("a", ".class", ".meffect"))
  checkPlot(me)
  cf =  1/155
  expect_equal(me$ame, c("pred" = cf, "pred2" =  - cf))
})

test_that("MarginalEffects for multiclass and multiple features", {
  me = MarginalEffects$new(predictor2, feature = c("a", "b"),
			   step.size = c(155, 155))
  dat = me$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c("a", "b",  ".class", ".meffect"))
  checkPlot(me)
  cf = 2
  expect_equal(me$ame, c("pred" = cf, "pred2" =  - cf))
})
