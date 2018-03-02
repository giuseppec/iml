context("createPredictionFunction")


library("mlr")
library("randomForest")
library("caret")


## mlr
task = mlr::makeClassifTask(data = iris, target = "Species")
lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = createPredictionFunction(mod.mlr, "classification")

lrn.response = mlr::makeLearner("classif.randomForest", predict.type = "response")
mod.mlr.response = mlr::train(lrn.response, task)
predictor.mlr.response = createPredictionFunction(mod.mlr.response, "classification")


# S3 predict
mod.S3 = mod.mlr$learner.model
predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
predictor.S3 = createPredictionFunction(mod.S3, predict.fun = predict.fun)

# caret
mod.caret = caret::train(Species ~ ., data = iris, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
predictor.caret = createPredictionFunction(mod.caret, task = "classification")

# function
mod.f = function(newdata) {
  predict(mod.caret, newdata = newdata,  type = "prob")
}
predictor.f = createPredictionFunction(NULL, predict.fun = mod.f, task = "classification")
iris.test = iris[c(2,20, 100, 150), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
prediction.f = predictor.f(iris.test)

test_that("output shape", {
  checkmate::expect_data_frame(prediction.f, ncols = 3)
  checkmate::expect_data_frame(predictor.caret(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.S3(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.mlr(iris.test), ncols = 3)
  checkmate::expect_data_frame(predictor.mlr.response(iris.test), ncols = 3)
})


test_that("equivalence", {
  expect_equivalent(prediction.f, predictor.caret(iris.test))
  expect_equivalent(predictor.mlr(iris.test), data.frame(predictor.S3(iris.test)))
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("setosa", "versicolor", "virginica"))
  expect_s3_class(prediction.f, "data.frame")
  predictor.f.1 = createPredictionFunction(NULL, predict.fun = mod.f, task = "classification")
  expect_equal(prediction.f[,1], predictor.f.1(iris.test)$setosa)
})


predictor.S3.2 = createPredictionFunction(mod.S3, predict.fun = NULL)

test_that("output is label", {
  expect_warning({pred = predictor.S3.2(iris)})
  checkmate::expect_data_frame(pred, nrows = nrow(iris), ncols = 3)
})


# Test numeric predictions

data(Boston, package="MASS")
## mlr
task = mlr::makeRegrTask(data = Boston, target = "medv")
lrn = mlr::makeLearner("regr.randomForest")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = createPredictionFunction(mod.mlr, task = "regression")

# S3 predict
mod.S3 = mod.mlr$learner.model
predictor.S3 = createPredictionFunction(mod.S3, task = "regression")

# caret
mod.caret = caret::train(medv ~ ., data = Boston, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
predictor.caret = createPredictionFunction(mod.caret, task = "regression")

# function
mod.f = function(newdata) {
  predict(mod.caret, newdata = newdata)
}
predictor.f = createPredictionFunction(NULL, predict.fun = mod.f, task = "regression")
boston.test = Boston[c(1,2,3,4), ]
prediction.f = predictor.f(boston.test)

test_that("output shape", {
  checkmate::expect_data_frame(prediction.f, ncols = 1)
  checkmate::expect_data_frame(predictor.caret(boston.test), ncols = 1)
  checkmate::expect_data_frame(predictor.S3(boston.test), ncols = 1)
  checkmate::expect_data_frame(predictor.mlr(boston.test), ncols = 1)
  
})


test_that("equivalence", {
  expect_equivalent(prediction.f, predictor.caret(boston.test))
  expect_equivalent(predictor.mlr(boston.test), predictor.S3(boston.test))
})



