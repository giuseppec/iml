library("mlr")
library("mlr3")
library("caret")

## mlr
task <- mlr::makeClassifTask(data = iris, target = "Species")
lrn <- mlr::makeLearner("classif.rpart", predict.type = "prob")
mod.mlr <- mlr::train(lrn, task)
predictor.mlr <- create_predict_fun(mod.mlr, "classification")

lrn.response <- mlr::makeLearner("classif.rpart", predict.type = "response")
mod.mlr.response <- mlr::train(lrn.response, task)
predictor.mlr.response <- create_predict_fun(mod.mlr.response, "classification")

# mlr3
task_iris <- TaskClassif$new(id = "iris", backend = iris, target = "Species")
learner <- lrn("classif.rpart", predict_type = "prob")
learner$train(task_iris)
predictor.mlr3 <- create_predict_fun(learner, "classification")

lrn.response <- lrn("classif.rpart", predict_type = "response")
mod.mlr.response <- lrn.response$train(task_iris)
predictor.mlr3.response <- create_predict_fun(mod.mlr.response, "classification")


# S3 predict
mod.S3 <- mod.mlr$learner.model
predict.fun <- function(object, newdata) predict(object, newdata, type = "prob")
predictor.S3 <- create_predict_fun(mod.S3, predict.fun = predict.fun)

# caret
mod.caret <- caret::train(Species ~ .,
  data = iris, method = "knn",
  trControl = caret::trainControl(method = "cv")
)
predictor.caret <- create_predict_fun(mod.caret, task = "classification", type = "prob")

# function
mod.f <- function(newdata) {
  predict(mod.caret, newdata = newdata, type = "prob")
}
predictor.f <- create_predict_fun(NULL, predict.fun = mod.f, task = "classification")
iris.test <- iris[c(2, 20, 100, 150), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
prediction.f <- predictor.f(iris.test)

test_that("output shape", {
  checkmate::expect_data_frame(prediction.f, ncols = 3)
  checkmate::expect_data_frame(predictor.caret(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.S3(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.mlr(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.mlr.response(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.mlr3(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.mlr.response(iris.test), ncols = 3)
})

test_that("equivalence", {
  expect_equal(ignore_attr = TRUE, prediction.f, predictor.caret(iris.test))
  expect_equal(ignore_attr = TRUE, predictor.mlr(iris.test), data.frame(predictor.S3(iris.test)))
  expect_equal(ignore_attr = TRUE, predictor.mlr3(iris.test), data.frame(predictor.S3(iris.test)))
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("setosa", "versicolor", "virginica"))
  expect_s3_class(prediction.f, "data.frame")
  predictor.f.1 <- create_predict_fun(NULL, predict.fun = mod.f, task = "classification")
  expect_equal(prediction.f[, 1], predictor.f.1(iris.test)$setosa)
})


predictor.S3.2 <- create_predict_fun(mod.S3, predict.fun = NULL)

test_that("output is label", {
  pred <- predictor.S3.2(iris)
  checkmate::expect_data_frame(pred, nrows = nrow(iris), ncols = 3)
})


# Test numeric predictions

data(Boston, package = "MASS")
## mlr
task <- mlr::makeRegrTask(data = Boston, target = "medv")
lrn <- mlr::makeLearner("regr.rpart")
mod.mlr <- mlr::train(lrn, task)
predictor.mlr <- create_predict_fun(mod.mlr, task = "regression")

task <- TaskRegr$new(id = "Boston", backend = Boston, target = "medv")
learn <- lrn("regr.rpart")
learn$train(task)
predictor.mlr3 <- create_predict_fun(learn, task = "regression")


# S3 predict
mod.S3 <- mod.mlr$learner.model
predictor.S3 <- create_predict_fun(mod.S3, task = "regression")

# caret
mod.caret <- caret::train(medv ~ .,
  data = Boston, method = "knn",
  trControl = caret::trainControl(method = "cv")
)
predictor.caret <- create_predict_fun(mod.caret, task = "regression")

# function
mod.f <- function(newdata) {
  predict(mod.caret, newdata = newdata)
}
predictor.f <- create_predict_fun(NULL, predict.fun = mod.f, task = "regression")
boston.test <- Boston[c(1, 2, 3, 4), ]
prediction.f <- predictor.f(boston.test)

test_that("output shape", {
  checkmate::expect_data_frame(prediction.f, ncols = 1)
  checkmate::expect_data_frame(predictor.caret(boston.test), ncols = 1)
  checkmate::expect_data_frame(predictor.S3(boston.test), ncols = 1)
  checkmate::expect_data_frame(predictor.mlr(boston.test), ncols = 1)
  checkmate::expect_data_frame(predictor.mlr3(boston.test), ncols = 1)
})


test_that("equivalence", {
  expect_equal(ignore_attr = TRUE, prediction.f, predictor.caret(boston.test))
  expect_equal(ignore_attr = TRUE, predictor.mlr(boston.test), predictor.S3(boston.test))
  expect_equal(ignore_attr = TRUE, predictor.mlr(boston.test), predictor.mlr3(boston.test))
})
