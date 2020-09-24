if ((Sys.info()[["sysname"]] != "Windows") &
  identical(Sys.getenv("NOT_CRAN"), "true")) {
  skip_on_cran()
  suppressPackageStartupMessages(library("h2o", quietly = TRUE))
  suppressPackageStartupMessages(library("keras"))

  # keras ----------------------------------------------------------------------
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

  # h2o multinomial classification ---------------------------------------------
  h2o.init()
  h2o.no_progress()
  # fit h2o model
  dat <- as.h2o(iris)
  y <- "Species"
  x <- setdiff(names(iris), y)
  mod.h2o.class <- h2o.glm(
    training_frame = dat, x = x, y = y,
    family = "multinomial", solver = "L_BFGS"
  )
  # create predictor
  predictor.h2o.class <- Predictor$new(mod.h2o.class, data = iris)

  # Artificially create binary classification task from iris
  iris2 <- iris
  iris2$Species <- as.factor(iris2$Species == "setosa")
  dat2 <- as.h2o(iris2)
  # h2o binomial classification
  mod.h2o.class2 <- h2o.glm(
    training_frame = dat2, x = x, y = y,
    family = "binomial", solver = "L_BFGS"
  )
  # create predictor
  predictor.h2o.class2 <- Predictor$new(mod.h2o.class2, data = iris2)

  # h2o regression
  y <- "Sepal.Width"
  x <- setdiff(names(iris), y)
  dat <- as.h2o(iris)
  mod.h2o.regr <- h2o.randomForest(training_frame = dat, x = x, y = y)
  predictor.h2o.regr <- Predictor$new(mod.h2o.regr, data = iris)
}
