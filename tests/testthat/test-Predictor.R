context("Predictor")


library("mlr")
library("mlr3")
library("caret")
library("data.table")
library("keras")
library("h2o")

## mlr
task = mlr::makeClassifTask(data = iris, target = "Species")
lrn = mlr::makeLearner("classif.rpart", predict.type = "prob")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = Predictor$new(mod.mlr, data = iris)

## mlr3
task_iris = TaskClassif$new(id = "iris", backend = iris, target = "Species")
learner_iris = lrn("classif.rpart", predict_type = "prob")
learner_iris$train(task_iris)
predictor.mlr3 = Predictor$new(learner_iris, data = iris) 


# S3 predict
mod.S3 = mod.mlr$learner.model
predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
predictor.S3 = Predictor$new(mod.S3, data = iris, predict.fun = predict.fun)

# caret
mod.caret = caret::train(Species ~ ., data = iris, method = "knn", 
  trControl = caret::trainControl(method = "cv"))
predictor.caret = Predictor$new(mod.caret, data = iris, type = "prob")


# h2o multinomial classification
h2o.init()
h2o.no_progress()
# fit h2o model
dat =  as.h2o(iris)
y = "Species"
x = setdiff(names(iris), y)
mod.h2o.class = h2o.glm(training_frame = dat, x = x, y = y, family = 'multinomial', solver = 'L_BFGS')
# create predictor
predictor.h2o.class = Predictor$new(mod.h2o.class, data = iris)

# Artificially create binary classification task from iris
iris2 = iris
iris2$Species = as.factor(iris2$Species == "setosa")
dat2 = as.h2o(iris2)
# h2o binomial classification
mod.h2o.class2 = h2o.glm(training_frame = dat2, x = x, y = y, family = 'binomial', solver = 'L_BFGS')
# create predictor
predictor.h2o.class2 = Predictor$new(mod.h2o.class2, data = iris2)


# h2o regression
y = "Sepal.Width"
x = setdiff(names(iris), y)
dat = as.h2o(iris)
mod.h2o.regr = h2o.randomForest(training_frame = dat, x = x, y = y)
predictor.h2o.regr = Predictor$new(mod.h2o.regr, data = iris)

# keras
k = backend()
k$clear_session()
x_mat = data.matrix(iris[,1:4])
y_mat = model.matrix(~ Species - 1, iris)
mod.keras = keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = 4) %>% 
  layer_dense(units = 3, activation = 'softmax') %>%
  compile(loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop(), metrics = c('accuracy'))
mod.keras %>% fit(x = x_mat, y = y_mat, epochs = 25 ,batch_size = 20,validation_split = 0, verbose = 0)
predictor.keras = Predictor$new(mod.keras, data = iris)
predictor.keras.prob = Predictor$new(mod.keras, data = iris, type = "prob")
predictor.keras.nice = Predictor$new(mod.keras, data = iris, predict.fun = function(object, newdata)  {
  res = predict(object, newdata)
  colnames(res) = levels(iris$Species)
  res
})

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
  expect_equivalent(predictor.mlr3$predict(iris.test), predictor.S3$predict(iris.test))
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("setosa", "versicolor", "virginica"))
  expect_s3_class(prediction.f, "data.frame")
  predictor.f.1 = Predictor$new(predict.fun = mod.f, class = 1, data = iris)
  expect_equal(prediction.f[,1], predictor.f.1$predict(iris.test)$setosa)
})

test_that("extracts y automatically for mlr::WrappedModel", {
  expect_equal(predictor.mlr$data$y.names, "Species")
})

test_that("extracts y automatically for caret::train", {
  expect_equal(predictor.caret$data$y.names, "Species")
})

test_that("extracts y automatically for randomForest", {
  expect_equal(predictor.S3$data$y.names, "Species")
})

test_that("extracts y automatically for H2OMultinomialModel", {
  expect_equal(predictor.h2o.class$data$y.names, "Species")
})

test_that("extracts y automatically for H2OBinomialModel", {
  expect_equal(predictor.h2o.class2$data$y.names, "Species")
})

test_that("extracts y automatically for H2ORegressionModel", {
  expect_equal(predictor.h2o.regr$data$y.names, "Sepal.Width")
})



test_that("extracts data automatically for caret::train", {
  predictor.caret2 = Predictor$new(mod.caret, type = "prob")
  expect_equal(data.frame(predictor.caret2$data$X), iris[,-which(names(iris) == "Species")])
})

test_that("errors when trying to extract data from for mlr::WrappedModel", {
  expect_error(Predictor$new(mod.mlr, type = "prob"))
})

test_that("errors when trying to extract data from for mlr3::Learner", {
  expect_error(Predictor$new(learner_iris, type = "prob"))
})



test_that("h20 prediction works", {
  expect_equal(predictor.h2o.class$predict(iris),
	       as.data.frame(predict(mod.h2o.class, newdata = dat))[-1])
  expect_equal(predictor.h2o.class2$predict(iris2),
	       as.data.frame(predict(mod.h2o.class2, newdata = dat2))[-1])
  expect_equal(predictor.h2o.regr$predict(iris),
	       as.data.frame(predict(mod.h2o.regr, newdata = dat)))
})


test_that("Keras classification predictions work without prob", {
  expect_equal(predictor.keras$predict(newdata = iris.test),
               as.data.frame(predict(mod.keras, data.matrix(iris.test))) %>% `colnames<-`(c("X1", "X2", "X3")))
})

test_that("Keras classification predictions work with prob", {
  expect_equal(predictor.keras.prob$predict(newdata = iris.test),
               as.data.frame(predict(mod.keras, data.matrix(iris.test))) %>% `colnames<-`(c("X1", "X2", "X3")))
})

test_that("Keras classification can get nice column names through custom predict funs", {
  expect_equal(predictor.keras.nice$predict(newdata = iris.test),
               as.data.frame(predict(mod.keras, data.matrix(iris.test))) %>% `colnames<-`(c("setosa", "versicolor", "virginica")))
})

# Test single class  predictions



# mlr
predictor.mlr = Predictor$new(mod.mlr, class = 2, data = iris)
# mlr3
predictor.mlr3 = Predictor$new(learner_iris, class = 2, data = iris)
# S3 predict
predictor.S3 = Predictor$new(mod.S3, class = 2, predict.fun = predict.fun, data = iris)
# caret
predictor.caret = Predictor$new(mod.caret, class = 2, data = iris, type = "prob")
# function
predictor.f = Predictor$new(predict.fun = mod.f, class = 2, data = iris)
prediction.f = predictor.f$predict(iris.test)
test_that("equivalence",{
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(predictor.mlr$predict(iris.test), predictor.S3$predict(iris.test))
  expect_equivalent(predictor.mlr3$predict(iris.test), predictor.S3$predict(iris.test))
})

test_that("Missing predict.type for mlr gives warning", {
  task = mlr::makeClassifTask(data = iris, target = "Species")
  lrn = mlr::makeLearner("classif.randomForest")
  mod.mlr = mlr::train(lrn, task)
  expect_warning(Predictor$new(mod.mlr, data = iris))
})

# Test numeric predictions

data(Boston, package="MASS")
## mlr
task = mlr::makeRegrTask(data = Boston, target = "medv")
lrn = mlr::makeLearner("regr.rpart")
mod.mlr = mlr::train(lrn, task)
predictor.mlr = Predictor$new(mod.mlr, data = Boston)

# mlr3 
task = TaskRegr$new(id = "bn", backend = Boston, target = "medv")
learner = lrn("regr.rpart")
learner$train(task)
predictor.mlr3 = Predictor$new(learner, data = Boston) 

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
  expect_equivalent(predictor.mlr3$predict(boston.test), predictor.S3$predict(boston.test))
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("pred"))
  expect_class(prediction.f, "data.frame")
})


predictor.mlr = Predictor$new(mod.mlr, class = 2, data = iris, y = iris$Species)
predictor.mlrb = Predictor$new(mod.mlr, class = 2, data = iris, y = "Species")
predictor.mlr3 = Predictor$new(learner_iris, class = 2, data = iris, y = iris$Species)
predictor.mlr3b = Predictor$new(learner_iris, class = 2, data = iris, y = "Species")

test_that("Returning y", {
  expect_equal(predictor.mlr$data$y, data.frame(.y = iris$Species))
  expect_equal(predictor.mlrb$data$y, data.frame(Species = iris$Species))
  expect_equal(predictor.mlrb$data$y, data.frame(Species = iris$Species))
  expect_false("Species" %in% colnames(predictor.mlrb$data$X))

  expect_equal(predictor.mlr3$data$y, data.frame(.y = iris$Species))
  expect_equal(predictor.mlr3b$data$y, data.frame(Species = iris$Species))
  expect_equal(predictor.mlr3b$data$y, data.frame(Species = iris$Species))
  expect_false("Species" %in% colnames(predictor.mlr3b$data$X))

})


test_that("Predictor errors with only one feature", {
  dat = data.frame(y = 1:10, x = factor(c(1,2,1,2,1,2,1,2,1,2), levels = c(1,2,3)))
  mod = lm(y ~ x, data = dat)
  expect_error(Predictor$new(mod, data = dat))
})

test_that("Predictor errors with data, which includes NAs.", {
  dat = data.frame(y = 1:10, x = factor(c(NA,NA,1,2,1,2,1,2,1,2), levels = c(1,2,3)))
  mod = lm(y ~ x, data = dat)
  expect_error(Predictor$new(mod, data = dat))
})

# keras
k = backend()
k$clear_session()
x_mat = data.matrix(Boston[,1:13])
y_mat = data.matrix(Boston[,14])
mod.keras = keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = 13) %>% 
  layer_dense(units = 1, activation = 'linear') %>%
  compile(loss = 'mean_squared_error', optimizer = optimizer_rmsprop(), metrics = c('mean_squared_error'))
mod.keras %>% fit(x = x_mat, y = y_mat, epochs = 25 ,batch_size = 20,validation_split = 0, verbose = 0)
predictor.keras = Predictor$new(mod.keras, data = Boston)

test_that("Keras regression predictions work", {
  expect_equal(predictor.keras$predict(newdata = boston.test[,1:13]),
               as.data.frame(predict(mod.keras, data.matrix(boston.test[,1:13]))) %>% `colnames<-`(c("pred")))
})


