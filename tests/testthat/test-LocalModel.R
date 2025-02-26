set.seed(12)
expected.colnames <- c("beta", "x.recoded", "effect", "x.original", "feature", "feature.value")

test_that("LocalModel works for single output and single feature", {
  x.interest <- X[2, ]
  k <- 1
  set.seed(42)
  LocalModel1 <- LocalModel$new(predictor1, x.interest = x.interest, k = k)
  dat <- LocalModel1$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)
  p <- plot(LocalModel1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
  
  x.interest2 <- X[4, ]
  LocalModel1$explain(x.interest2)
  dat <- LocalModel1$results
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)
  
  pred <- predict(LocalModel1, newdata = X[3:4, ])
  expect_data_frame(pred, nrows = 2)
  expect_equal(colnames(pred), "prediction")
  
  LocalModel1 <- suppressWarnings(LocalModel$new(predictor1,
                                x.interest = x.interest, k = k,
                                dist.fun = "euclidean", kernel.width = 1
  ))
  LocalModel1$explain(x.interest2)
  dat <- LocalModel1$results
  expect_equal(colnames(dat), expected.colnames)
  expect_lte(nrow(dat), k)
  
  pred <- predict(LocalModel1, newdata = X[3:4, ])
  expect_data_frame(pred, nrows = 2)
  expect_equal(colnames(pred), "prediction")
})

test_that("LocalModel works for multiple output", {
  skip_if_not_installed("rpart")
  require("rpart")
  clf <- rpart(Species ~ ., data = iris)
  mod <- Predictor$new(clf, data = iris)
  x.interest <- iris[1, -5]
  k <- 1
  set.seed(42)
  # rbind X a few times to eliminate glm warning
  LocalModel1 <- suppressWarnings(LocalModel$new(mod, x.interest, k = k))
  dat <- LocalModel1$results
  expect_equal(colnames(dat), c(expected.colnames, ".class"))
  expect_lte(nrow(dat), k * 3)
  pred2 <- predict(LocalModel1, iris[c(2, 3), -5])
  expect_class(dat, "data.frame")
  expect_data_frame(pred2, nrows = 2)
  expect_equal(colnames(pred2), c("setosa", "versicolor", "virginica"))
  
  p <- plot(LocalModel1)
  expect_s3_class(p, c("gg", "ggplot"))
  p
})




test_that("LocalModel prediction works and expects same cols as training dat", {
  x.interest <- X[2, ]
  k <- 2
  set.seed(42)
  LocalModel1 <- LocalModel$new(predictor1, x.interest = x.interest, k = k)
  expect_equal(LocalModel1$predict(x.interest)$prediction, LocalModel1$predict(X)[2,])
  expect_warning(LocalModel1$predict(cbind(x.interest, data.frame(blabla = 1))))
  expect_error(LocalModel1$predict(x.interest[-2]), "Missing")
})




test_that("LocalModel distance functions work as expected", {
  kernel.width <- 1
  gower.power <- 1
  distance.functions <- c(
    "gower", "euclidean", "maximum",
    "manhattan", "canberra", "binary", "minkowski")
  x.interest <- X[2, ]
  k <- 1
  set.seed(42)
  LocalModel1 <- LocalModel$new(predictor1, x.interest = x.interest, k = k, 
                                dist = "euclidean", kernel.width = kernel.width)
  # recode to avoid warning for categorical variables (NAs introduced by coercion)
  X.recode <- recode(LocalModel1$.__enclos_env__$private$dataDesign, x.interest)
  x.recoded <- recode(x.interest, x.interest)
  # first test the function that was used for fitting
  weights <- LocalModel1$.__enclos_env__$private$weight.fun(X.recode, x.recoded)
  expect_equal(object = weights[2], expected = 1)
  # test all distance functions by explicitly constructing them via get.weight.fun()
  for(fun in distance.functions){
    weight_fun <- LocalModel1$.__enclos_env__$private$get.weight.fun(
      dist.fun = fun, kernel.width = kernel.width, gower.power = gower.power)
    expect_equal(object = weight_fun(X.recode, x.recoded)[2], expected = 1)
  }
})
