library("mlr")
library("mlr3")
library("caret")
library("data.table")
library("keras")
library("h2o")

f <- function(newdata, multi = FALSE) {
  pred <- unlist(newdata[, "a"] + newdata[, "b"] + 100 * (newdata[, "c"] == "a")) / (155)
  dat <- data.frame(pred = pred)
  colnames(dat) <- "pred"
  if (multi) dat$pred2 <- 1 - dat$pred
  dat
}

# With some interaction
f.inter <- function(newdata, multi = FALSE) {
  pred <- unlist(newdata[, "a"] + newdata[, "b"] + newdata[, "a"] * newdata[, "b"] + 100 * (newdata[, "c"] == "a") + newdata[, "a"] * (newdata[, "c"] == "a")) / (155)
  dat <- data.frame(pred = pred)
  colnames(dat) <- "pred"
  if (multi) dat$pred2 <- 1 - dat$pred
  dat
}

X <- data.frame(
  a = c(1, 2, 3, 4, 5),
  b = c(10, 20, 30, 40, 50),
  c = factor(c("a", "b", "c", "a", "b")),
  d = factor(c("A", "A", "B", "B", "B"))
)

set.seed(12)
y <- f(X) + rnorm(nrow(X))
y2 <- factor(ifelse(X$b + X$a < 20, "pred", "pred2"))

predictor1 <- Predictor$new(data = X, y = y, predict.fun = f)
predictor1.inter <- Predictor$new(data = X, predict.fun = f.inter)
predict.fun <- function(obj, newdata) obj(newdata, multi = TRUE)
predictor2 <- Predictor$new(f, data = X, y = y2, predict.fun = predict.fun)
predictor3 <- Predictor$new(f, data = X, predict.fun = predict.fun, class = 2)



checkPlot <- function(obj) {
  p <- plot(obj)
  expect_s3_class(p, c("gg", "ggplot"))
  plot(p)
}


if (Sys.info()[["sysname"]] != "Windows") {

  # keras
  k <- backend()
  k$clear_session()
  x_mat <- data.matrix(iris[, 1:4])
  y_mat <- model.matrix(~ Species - 1, iris)
  mod.keras1 <- keras_model_sequential() %>%
    layer_dense(units = 4, activation = "relu", input_shape = 4) %>%
    layer_dense(units = 3, activation = "softmax") %>%
    compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_rmsprop(), metrics = c("accuracy")
    )
  mod.keras1 %>% fit(
    x = x_mat, y = y_mat, epochs = 25, batch_size = 20,
    validation_split = 0, verbose = 0
  )
  predictor.keras1 <- Predictor$new(mod.keras1, data = iris)
  predictor.keras1.prob <- Predictor$new(mod.keras1, data = iris, type = "prob")
  predictor.keras1.nice <- Predictor$new(mod.keras1,
    data = iris,
    predict.fun = function(object, newdata) {
      res <- predict(object, newdata)
      colnames(res) <- levels(iris$Species)
      res
    }
  )

  # keras
  k <- backend()
  k$clear_session()
  x_mat <- data.matrix(Boston[, 1:13])
  y_mat <- data.matrix(Boston[, 14])
  mod.keras2 <- keras_model_sequential() %>%
    layer_dense(units = 4, activation = "relu", input_shape = 13) %>%
    layer_dense(units = 1, activation = "linear") %>%
    compile(
      loss = "mean_squared_error", optimizer = optimizer_rmsprop(),
      metrics = c("mean_squared_error")
    )
  mod.keras2 %>%
    fit(
      x = x_mat, y = y_mat, epochs = 25, batch_size = 20,
      validation_split = 0, verbose = 0
    )
  predictor.keras2 <- Predictor$new(mod.keras2, data = Boston)

}

## mlr
task <- mlr::makeClassifTask(data = iris, target = "Species")
lrn <- mlr::makeLearner("classif.rpart", predict.type = "prob")
mod.mlr <- mlr::train(lrn, task)
predictor.mlr <- Predictor$new(mod.mlr, data = iris)

## mlr3
task_iris <- TaskClassif$new(id = "iris", backend = iris, target = "Species")
learner_iris <- lrn("classif.rpart", predict_type = "prob")
learner_iris$train(task_iris)
predictor.mlr3 <- Predictor$new(learner_iris, data = iris)


# S3 predict
mod.S3 <- mod.mlr$learner.model
predict.fun <- function(object, newdata) predict(object, newdata, type = "prob")
predictor.S3 <- Predictor$new(mod.S3, data = iris, predict.fun = predict.fun)

# caret
mod.caret <- caret::train(Species ~ .,
  data = iris, method = "knn",
  trControl = caret::trainControl(method = "cv")
)
predictor.caret <- Predictor$new(mod.caret, data = iris, type = "prob")


# h2o multinomial classification
h2o.init()
h2o.no_progress()
# fit h2o model
dat <- as.h2o(iris)
y <- "Species"
x <- setdiff(names(iris), y)
mod.h2o.class <- h2o.glm(training_frame = dat, x = x, y = y, family = "multinomial", solver = "L_BFGS")
# create predictor
predictor.h2o.class <- Predictor$new(mod.h2o.class, data = iris)

# Artificially create binary classification task from iris
iris2 <- iris
iris2$Species <- as.factor(iris2$Species == "setosa")
dat2 <- as.h2o(iris2)
# h2o binomial classification
mod.h2o.class2 <- h2o.glm(training_frame = dat2, x = x, y = y, family = "binomial", solver = "L_BFGS")
# create predictor
predictor.h2o.class2 <- Predictor$new(mod.h2o.class2, data = iris2)

# h2o regression
y <- "Sepal.Width"
x <- setdiff(names(iris), y)
dat <- as.h2o(iris)
mod.h2o.regr <- h2o.randomForest(training_frame = dat, x = x, y = y)
predictor.h2o.regr <- Predictor$new(mod.h2o.regr, data = iris)

# function
mod.f <- function(newdata) {
  predict(mod.caret, newdata = newdata, type = "prob")
}
predictor.f <- Predictor$new(predict.fun = mod.f, data = iris)
iris.test <- iris[c(2, 20, 100, 150), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
prediction.f <- predictor.f$predict(iris.test)
