## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ------------------------------------------------------------------------
set.seed(42)
library("iml")
library("randomForest")
data("Boston", package  = "MASS")
rf = randomForest(medv ~ ., data = Boston, ntree = 500)
X = Boston[which(names(Boston) != "medv")]
predictor = Predictor$new(rf, data = X, y = Boston$medv)

## ------------------------------------------------------------------------
library("doParallel")
# Creates a cluster with 2 cores
cl = makePSOCKcluster(2)
# Registers cluster
registerDoParallel(cl)

## ------------------------------------------------------------------------
imp = FeatureImp$new(predictor, loss = "mae", parallel = TRUE)
plot(imp)

## ------------------------------------------------------------------------
system.time(FeatureImp$new(predictor, loss = "mae", parallel = FALSE))
system.time(FeatureImp$new(predictor, loss = "mae", parallel = TRUE))

## ------------------------------------------------------------------------
system.time(FeatureImp$new(predictor, loss = "mae", parallel = FALSE, n.repetitions = 20))
system.time(FeatureImp$new(predictor, loss = "mae", parallel = TRUE, n.repetitions = 20))

## ------------------------------------------------------------------------
system.time(Interaction$new(predictor, parallel = FALSE))
system.time(Interaction$new(predictor, parallel = TRUE))

## ------------------------------------------------------------------------
stopCluster(cl)

