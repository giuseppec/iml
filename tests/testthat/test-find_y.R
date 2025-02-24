test_that("find_y works with mlr", {
  skip_if_not_installed("mlr")
  require("mlr")
  data(cars, package = "datasets")

  # with mlr model
  tsk <- makeRegrTask(data = cars, target = "dist")
  lrn <- makeLearner("regr.rpart")
  mod <- mlr::train(lrn, tsk)
  expect_equal(find_y(mod), "dist")
})

test_that("find_y works with caret", {
  skip_if_not_installed("caret")
  require("caret")
  data(cars, package = "caret")

  # with mlr model
  mod <- caret::train(Price ~ ., data = cars, method = "rf", ntree = 1)
  expect_equal(find_y(mod), "Price")
})


test_that("find_y works with lm", {
  data(cars, package = "datasets")

  # with mlr model
  mod <- lm(speed ~ ., data = cars)
  expect_equal(find_y(mod), "speed")
})



test_that("find_y works with rpart", {
  skip_if_not_installed("rpart")
  require("rpart")
  data(cars, package = "datasets")

  # with mlr model
  mod <- rpart(speed ~ ., data = cars)
  expect_equal(find_y(mod), "speed")
})
