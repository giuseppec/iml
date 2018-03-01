context("inferTask")

rf = randomForest(Species ~ ., data = iris, ntree = 1)
f = function(x) predict(rf, newdata = x)


test_that("classificaton",{
  ## use linear discriminant analysis to classify iris data
  task = makeClassifTask(data = iris, target = "Species")
  learner = makeLearner("classif.lda", method = "mle")
  mod = mlr::train(learner, task)
  expect_warning({tsk = inferTaskFromModel(mod)})
  expect_equal(tsk, "classification")
  
  TrainData <- iris[,1:4]
  TrainClasses <- iris[,5]
  
  knnFit1 <- train(TrainData, TrainClasses,
    method = "knn",
    preProcess = c("center", "scale"),
    tuneLength = 2,
    trControl = trainControl(method = "cv"))
  
  expect_equal(inferTaskFromModel(knnFit1), "classification")
  
  t1 = inferTaskFromPrediction(predict(rf, newdata = iris))
  t2 = inferTaskFromPrediction(predict(rf, newdata = iris, type = "prob"))
  
  expect_equal(t1, "classification")
  expect_equal(t2, "classification")
})

test_that("regression",{
  
  ## use linear discriminant analysis to classify iris data
  task = makeRegrTask(data = cars, target = "dist")
  learner = makeLearner("regr.lm")
  mod = mlr::train(learner, task)
  expect_equal(inferTaskFromModel(mod), "regression")
  
  
  lmFit <- train(dist ~ .,
    data = cars,
    method = "lm")
  
  expect_equal(inferTaskFromModel(lmFit), "regression")
  pred = predict(lmFit)
  expect_equal(inferTaskFromPrediction(pred), "regression")
})


test_that("unknown",{
  expect_equal(inferTaskFromModel(rf), "unknown")
  expect_equal(inferTaskFromModel(f), "unknown")
})

