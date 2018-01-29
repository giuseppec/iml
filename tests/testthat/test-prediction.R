context('Prediction')


library('mlr')
library('randomForest')
library('caret')


## mlr
task = mlr::makeClassifTask(data = iris, target = "Species")
lrn = mlr::makeLearner("classif.randomForest", predict.type = 'prob')
mod.mlr = mlr::train(lrn, task)
predictor.mlr = prediction.model(mod.mlr)

# S3 predict
mod.S3 = mod.mlr$learner.model
predictor.S3 = prediction.model(mod.S3, predict.args = list(type='prob'))

# caret
mod.caret = caret::train(Species ~ ., data = iris, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
predictor.caret = prediction.model(mod.caret)

# function
mod.f = function(X){
  predict(mod.caret, newdata = X,  type = 'prob')
}
predictor.f = prediction.model(mod.f)
iris.test = iris[c(2,20, 100, 150), c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')]
prediction.f = predictor.f$predict(iris.test)


test_that('equivalence',{
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(predictor.mlr$predict(iris.test), predictor.S3$predict(iris.test))
  
})

test_that('f works', {
  expect_equal(colnames(prediction.f), c('setosa', 'versicolor', 'virginica'))
  expect_class(prediction.f, 'data.frame')
  labels.out = data.frame(..class = factor(c('setosa', 'setosa', 'versicolor', 'virginica')))
  expect_equal(predictor.f$predict(iris.test, labels=TRUE), labels.out)
  predictor.f.1 = prediction.model(mod.f, class = 1)
  expect_equal(prediction.f[,1], predictor.f.1$predict(iris.test)$prediction)
})


# Test single class  predictions



## mlr
predictor.mlr = prediction.model(mod.mlr, class = 2)
# S3 predict
predictor.S3 = prediction.model(mod.S3, class = 2, predict.args = list(type='prob'))
# caret
predictor.caret = prediction.model(mod.caret, class = 2)
# function
predictor.f = prediction.model(mod.f, class = 2)
prediction.f = predictor.f$predict(iris.test)
test_that('equivalence',{
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(predictor.mlr$predict(iris.test), predictor.S3$predict(iris.test))
})

test_that('f works', {
  expect_equal(colnames(prediction.f), c('prediction'))
  expect_class(prediction.f, 'data.frame')
})




# Test numeric predictions

data(Boston, package='MASS')
## mlr
task = mlr::makeRegrTask(data = Boston, target = "medv")
lrn = mlr::makeLearner("regr.randomForest")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = prediction.model(mod.mlr)

# S3 predict
mod.S3 = mod.mlr$learner.model
predictor.S3 = prediction.model(mod.S3)

# caret
mod.caret = caret::train(medv ~ ., data = Boston, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
  predictor.caret = prediction.model(mod.caret)

# function
mod.f = function(X){
  predict(mod.caret, newdata = X)
}
predictor.f = prediction.model(mod.f)
boston.test = Boston[c(1,2,3,4), ]
prediction.f = predictor.f$predict(boston.test)




test_that('equivalence',{
  expect_equivalent(prediction.f, predictor.caret$predict(boston.test))
  expect_equivalent(predictor.mlr$predict(boston.test), predictor.S3$predict(boston.test))
})

test_that('f works', {
  expect_equal(colnames(prediction.f), c('prediction'))
  expect_class(prediction.f, 'data.frame')
})

