suppressPackageStartupMessages(library("mlr", quietly = TRUE))
suppressPackageStartupMessages(library("mlr3"))
suppressPackageStartupMessages(library("caret"))
suppressPackageStartupMessages(library("data.table", quietly = TRUE))
suppressPackageStartupMessages(library("keras"))
suppressPackageStartupMessages(library("h2o", quietly = TRUE))

data(Boston, package = "MASS")

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
y_fe <- y
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

# function
mod.f <- function(newdata) {
  predict(mod.caret, newdata = newdata, type = "prob")
}
predictor.f <- Predictor$new(predict.fun = mod.f, data = iris)
iris.test <- iris[c(2, 20, 100, 150), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
prediction.f <- predictor.f$predict(iris.test)
