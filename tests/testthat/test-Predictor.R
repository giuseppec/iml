context("Predictor")

test_that("equivalence", {
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(
    predictor.mlr$predict(iris.test),
    predictor.S3$predict(iris.test)
  )
  expect_equivalent(
    predictor.mlr3$predict(iris.test),
    predictor.S3$predict(iris.test)
  )
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("setosa", "versicolor", "virginica"))
  expect_s3_class(prediction.f, "data.frame")
  predictor.f.1 <- Predictor$new(predict.fun = mod.f, class = 1, data = iris)
  expect_equal(prediction.f[, 1], predictor.f.1$predict(iris.test)$setosa)
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
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(predictor.h2o.class$data$y.names, "Species")
})

test_that("extracts y automatically for H2OBinomialModel", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(predictor.h2o.class2$data$y.names, "Species")
})

test_that("extracts y automatically for H2ORegressionModel", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(predictor.h2o.regr$data$y.names, "Sepal.Width")
})

test_that("extracts data automatically for caret::train", {
  predictor.caret2 <- Predictor$new(mod.caret, type = "prob")
  expect_equal(
    data.frame(predictor.caret2$data$X),
    iris[, -which(names(iris) == "Species")]
  )
})

test_that("errors when trying to extract data from for mlr::WrappedModel", {
  expect_error(Predictor$new(mod.mlr, type = "prob"))
})

test_that("errors when trying to extract data from for mlr3::Learner", {
  expect_error(Predictor$new(learner_iris, type = "prob"))
})

test_that("h20 prediction works", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    predictor.h2o.class$predict(iris),
    as.data.frame(predict(mod.h2o.class, newdata = dat))[-1]
  )
  expect_equal(
    predictor.h2o.class2$predict(iris2),
    as.data.frame(predict(mod.h2o.class2, newdata = dat2))[-1]
  )
  expect_equal(
    predictor.h2o.regr$predict(iris),
    as.data.frame(predict(mod.h2o.regr, newdata = dat))
  )
})


test_that("Keras classification predictions work without prob", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    predictor.keras1$predict(newdata = iris.test),
    as.data.frame(predict(mod.keras1, data.matrix(iris.test))) %>%
      `colnames<-`(c("1", "2", "3"))
  )
})

test_that("Keras classification predictions work with prob", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    predictor.keras1.prob$predict(newdata = iris.test),
    as.data.frame(predict(mod.keras1, data.matrix(iris.test))) %>%
      `colnames<-`(c("1", "2", "3"))
  )
})

test_that("Keras classification can get nice column names through custom predict funs", {
  skip_on_os("windows")
  skip_on_cran()

  expect_equal(
    predictor.keras1.nice$predict(newdata = iris.test),
    as.data.frame(predict(mod.keras1, data.matrix(iris.test))) %>%
      `colnames<-`(c("setosa", "versicolor", "virginica"))
  )
})

# Test single class predictions

# mlr
predictor.mlr <- Predictor$new(mod.mlr, class = 2, data = iris)
# mlr3
predictor.mlr3 <- Predictor$new(learner_iris, class = 2, data = iris)
# mlr3_ check that mlr3 tasks work when supplied as "data" (#115)
train <- sample(task_iris$nrow, task_iris$nrow * 2 / 3)
predictor.mlr3_2 <- Predictor$new(learner_iris,
  data = task_iris$data(train, cols = task_iris$feature_names),
  y = task_iris$truth(train)
)
# S3 predict
predictor.S3 <- Predictor$new(mod.S3,
  class = 2, predict.fun = predict.fun,
  data = iris
)
# caret
predictor.caret <- Predictor$new(mod.caret,
  class = 2, data = iris,
  type = "prob"
)
# function
predictor.f <- Predictor$new(predict.fun = mod.f, class = 2, data = iris)
prediction.f <- predictor.f$predict(iris.test)

test_that("equivalence", {
  expect_equivalent(prediction.f, predictor.caret$predict(iris.test))
  expect_equivalent(
    predictor.mlr$predict(iris.test),
    predictor.S3$predict(iris.test)
  )
  expect_equivalent(
    predictor.mlr3$predict(iris.test),
    predictor.S3$predict(iris.test)
  )
})

test_that("Missing predict.type for mlr gives warning", {
  task <- mlr::makeClassifTask(data = iris, target = "Species")
  lrn <- mlr::makeLearner("classif.randomForest")
  mod.mlr <- mlr::train(lrn, task)
  expect_warning(Predictor$new(mod.mlr, data = iris))
})

# Test numeric predictions

## mlr
task <- mlr::makeRegrTask(data = Boston, target = "medv")
lrn <- mlr::makeLearner("regr.rpart")
mod.mlr <- mlr::train(lrn, task)
predictor.mlr <- Predictor$new(mod.mlr, data = Boston)

# mlr3
task <- TaskRegr$new(id = "bn", backend = Boston, target = "medv")
learner <- lrn("regr.rpart")
learner$train(task)
predictor.mlr3 <- Predictor$new(learner, data = Boston)

# S3 predict
mod.S3 <- mod.mlr$learner.model
predictor.S3 <- Predictor$new(mod.S3, data = Boston)

# caret
mod.caret <- caret::train(medv ~ .,
  data = Boston, method = "knn",
  trControl = caret::trainControl(method = "cv")
)
predictor.caret <- Predictor$new(mod.caret, data = Boston)

# function
mod.f <- function(newdata) {
  predict(mod.caret, newdata = newdata)
}
predictor.f <- Predictor$new(predict.fun = mod.f, data = Boston)
boston.test <- Boston[c(1, 2, 3, 4), ]
prediction.f <- predictor.f$predict(boston.test)


test_that("equivalence", {
  expect_equivalent(
    prediction.f,
    predictor.caret$predict(boston.test)
  )
  expect_equivalent(
    predictor.mlr$predict(boston.test),
    predictor.S3$predict(boston.test)
  )
  expect_equivalent(
    predictor.mlr3$predict(boston.test),
    predictor.S3$predict(boston.test)
  )
})

test_that("f works", {
  expect_equal(colnames(prediction.f), c("pred"))
  expect_class(prediction.f, "data.frame")
})


predictor.mlr <- Predictor$new(mod.mlr,
  class = 2, data = iris,
  y = iris$Species
)
predictor.mlrb <- Predictor$new(mod.mlr,
  class = 2, data = iris,
  y = "Species"
)
predictor.mlr3 <- Predictor$new(learner_iris,
  class = 2, data = iris,
  y = iris$Species
)
predictor.mlr3b <- Predictor$new(learner_iris,
  class = 2, data = iris,
  y = "Species"
)

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
  dat <- data.frame(y = 1:10, x = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
    levels = c(1, 2, 3)
  ))
  mod <- lm(y ~ x, data = dat)
  expect_error(Predictor$new(mod, data = dat))
})

test_that("Predictor errors with data, which includes NAs.", {
  dat <- data.frame(y = 1:10, x = factor(c(NA, NA, 1, 2, 1, 2, 1, 2, 1, 2),
    levels = c(1, 2, 3)
  ))
  mod <- lm(y ~ x, data = dat)
  expect_error(Predictor$new(mod, data = dat))
})

test_that("Predictor keeps factor names without X", {
  require(rpart)
  dat <- data.frame(
    y = factor(rep(c(1, 2), times = 5)),
    x = factor(c(1, 1, 1, 2, 1, 2, 1, 2, 1, 2),
      levels = c(1, 2, 3)
    ), x2 = 1:10
  )
  mod <- rpart(y ~ x, data = dat)
  pred <- Predictor$new(mod, data = dat)
  expect_equal(colnames(pred$predict(dat)), c("1", "2"))
})

test_that("Keras regression predictions work", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    predictor.keras2$predict(newdata = boston.test[, 1:13]),
    as.data.frame(predict(mod.keras2, data.matrix(boston.test[, 1:13]))) %>%
      `colnames<-`(c("pred"))
  )
})
