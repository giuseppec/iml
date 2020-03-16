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
  mod.keras %>% fit(
    x = x_mat, y = y_mat, epochs = 25, batch_size = 20,
    validation_split = 0, verbose = 0
  )
  predictor.keras1 <- Predictor$new(mod.keras, data = iris)
  predictor.keras1.prob <- Predictor$new(mod.keras, data = iris, type = "prob")
  predictor.keras1.nice <- Predictor$new(mod.keras,
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
  predictor.keras2 <- Predictor$new(mod.keras, data = Boston)

}
