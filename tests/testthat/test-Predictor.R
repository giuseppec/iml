context("Predictor")


library("mlr")
library("randomForest")
library("caret")
library("data.table")

## mlr
task = mlr::makeClassifTask(data = iris, target = "Species")
lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = Predictor$new(mod.mlr, data = iris)

# S3 predict
mod.S3 = mod.mlr$learner.model
predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
predictor.S3 = Predictor$new(mod.S3, data = iris, predict.fun = predict.fun)

# caret
mod.caret = caret::train(Species ~ ., data = iris, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
predictor.caret = Predictor$new(mod.caret, data = iris)

# function
mod.f = function(newdata) {
  predict(mod.caret, newdata = newdata,  type = "prob")
}
predictor.f = Predictor$new(predict.fun = mod.f, data = iris)
iris.test = iris[c(2,20, 100, 150), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
prediction.f = predictor.f$predict(iris.test)


test_that("equivalence", {
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(predictor.mlr$predict(iris.test), predictor.S3$predict(iris.test))
  
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("setosa", "versicolor", "virginica"))
  expect_s3_class(prediction.f, "data.frame")
  predictor.f.1 = Predictor$new(predict.fun = mod.f, class = 1, data = iris)
  expect_equal(prediction.f[,1], predictor.f.1$predict(iris.test)$setosa)
})


# Test single class  predictions



## mlr
predictor.mlr = Predictor$new(mod.mlr, class = 2, data = iris)
# S3 predict
predictor.S3 = Predictor$new(mod.S3, class = 2, predict.fun = predict.fun, data = iris)
# caret
predictor.caret = Predictor$new(mod.caret, class = 2, data = iris)
# function
predictor.f = Predictor$new(predict.fun = mod.f, class = 2, data = iris)
prediction.f = predictor.f$predict(iris.test)
test_that("equivalence",{
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(predictor.mlr$predict(iris.test), predictor.S3$predict(iris.test))
})

test_that("Missing predict.type gives warning", {
  task = mlr::makeClassifTask(data = iris, target = "Species")
  lrn = mlr::makeLearner("classif.randomForest")
  mod.mlr = mlr::train(lrn, task)
  expect_warning(Predictor$new(mod.mlr, data = iris))
})




# Test numeric predictions

data(Boston, package="MASS")
## mlr
task = mlr::makeRegrTask(data = Boston, target = "medv")
lrn = mlr::makeLearner("regr.randomForest")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = Predictor$new(mod.mlr, data = Boston)

# S3 predict
mod.S3 = mod.mlr$learner.model
predictor.S3 = Predictor$new(mod.S3, data = Boston)

# caret
mod.caret = caret::train(medv ~ ., data = Boston, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
  predictor.caret = Predictor$new(mod.caret, data = Boston)

# function
mod.f = function(newdata) {
  predict(mod.caret, newdata = newdata)
}
predictor.f = Predictor$new(predict.fun = mod.f, data = Boston)
boston.test = Boston[c(1,2,3,4), ]
prediction.f = predictor.f$predict(boston.test)


test_that("equivalence", {
  expect_equivalent(prediction.f, predictor.caret$predict(boston.test))
  expect_equivalent(predictor.mlr$predict(boston.test), predictor.S3$predict(boston.test))
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("pred"))
  expect_class(prediction.f, "data.frame")
})


predictor.mlr = Predictor$new(mod.mlr, class = 2, data = iris, y = iris$Species)
predictor.mlr2 = Predictor$new(mod.mlr, class = 2, data = iris, y = "Species")


test_that("Giving y", {
  expect_equal(predictor.mlr$data$y, data.frame(.y = iris$Species))
  expect_equal(predictor.mlr2$data$y, data.frame(Species = iris$Species))
  expect_false("Species" %in% colnames(predictor.mlr2$data$X))
})




