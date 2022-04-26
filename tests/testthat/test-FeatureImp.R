set.seed(42)

expected_colnames <- c(
  "feature", "importance.05", "importance",
  "importance.95", "permutation.error"
)

test_that("FeatureImp works for single output", {
  var.imp <- FeatureImp$new(predictor1, loss = "mse")
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), ncol(X))
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  p <- plot(var.imp, sort = FALSE)
  expect_s3_class(p, c("gg", "ggplot"))
  p

  p <- var.imp$plot()
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("FeatureImp works for single output with single repetition", {
  var.imp <- FeatureImp$new(predictor1, loss = "mse", n.repetitions = 1)
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), ncol(X))
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("FeatureImp with difference", {
  var.imp <- FeatureImp$new(predictor1, loss = "mse", compare = "difference")
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), ncol(X))
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("FeatureImp with 0 model error", {
  data(iris)
  require("mlr")
  lrn <- mlr::makeLearner("classif.rpart", predict.type = "prob")
  tsk <- mlr::makeClassifTask(data = iris, target = "Species")
  mod <- mlr::train(lrn, tsk)
  pred <- Predictor$new(mod, data = iris, y = iris$Species == "setosa", class = "setosa")
  expect_warning(
    {
      var.imp <- FeatureImp$new(pred, loss = "mae")
    },
    "Model error is 0"
  )
  expect_equal(var.imp$compare, "difference")
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("FeatureImp works for single output and function as loss", {
  var.imp <- FeatureImp$new(predictor1, loss = Metrics::mse)
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  # Making sure the result is sorted by decreasing importance
  expect_equal(dat$importance, dat[order(dat$importance, decreasing = TRUE), ]$importance)
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), ncol(X))
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("FeatureImp works for multiple output", {
  var.imp <- FeatureImp$new(predictor2, loss = "ce")
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), ncol(X))
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})


test_that("FeatureImp fails without target vector", {
  predictor2 <- Predictor$new(f, data = X, predict.fun = predict.fun)
  expect_error(FeatureImp$new(predictor2, loss = "ce"))
})

test_that("Works for different repetitions.", {
  var.imp <- FeatureImp$new(predictor1, loss = "mse", n.repetitions = 2)
  dat <- var.imp$results
  expect_class(dat, "data.frame")
})


test_that("Model receives data.frame without additional columns", {
  # https://stackoverflow.com/questions/51980808/r-plotting-importance-feature-using-featureimpnew
  library(mlr)
  library(ranger)
  data("iris")
  tsk <- mlr::makeClassifTask(data = iris, target = "Species")
  lrn <- mlr::makeLearner("classif.ranger", predict.type = "prob")
  mod <- mlr:::train(lrn, tsk)
  X <- iris[which(names(iris) != "Species")]
  predictor <- Predictor$new(mod, data = X, y = iris$Species)
  imp <- FeatureImp$new(predictor, loss = "ce")
  expect_r6(imp)
})


set.seed(12)
X <- data.frame(x1 = 1:10, x2 = 1:10, x3 = 1:10)
y <- X[, 1] + X[, 2] + rnorm(10, 0, 0.1)


pred.fun <- function(newdata) {
  newdata[, 1] + newdata[, 2]
}

pred <- Predictor$new(data = X, predict.fun = pred.fun, y = y)

test_that("Feature Importance 0", {
  fimp <- FeatureImp$new(pred, loss = "mae", n.repetitions = 3)
  expect_equal(fimp$results$importance[3], 1)
})

test_that("FeatureImp works for a subset of features", {
  var.imp <- FeatureImp$new(predictor1, loss = "mse", features = c("a", "b"))
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), 2)
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("Invalid feature names are caught", {
  expect_error(
    FeatureImp$new(predictor1, loss = "mse", features = c("x", "y", "z")),
    "failed: Must be a subset of {'a','b','c','d'}, but has additional elements {'x','y','z'}",
    fixed = TRUE
  )
})

test_that("FeatureImp works for groups of features", {
  groups = list(ab = c("a", "b"), cd = c("c", "d"))
  var.imp <- FeatureImp$new(predictor1, loss = "mse", features = groups)
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), 2)
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})

test_that("FeatureImp works for overlapping groups of features", {
  groups = list(ab = c("a", "b"), bc = c("b", "c"))
  var.imp <- FeatureImp$new(predictor1, loss = "mse", features = groups)
  dat <- var.imp$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected_colnames)
  expect_equal(nrow(dat), 2)
  p <- plot(var.imp)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})
