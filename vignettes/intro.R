## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ------------------------------------------------------------------------
data("Boston", package  = "MASS")
head(Boston)

## ---- message = FALSE----------------------------------------------------
set.seed(42)
library("iml")
library("randomForest")
data("Boston", package  = "MASS")
rf = randomForest(medv ~ ., data = Boston, ntree = 50)

## ------------------------------------------------------------------------
X = Boston[which(names(Boston) != "medv")]
predictor = Predictor$new(rf, data = X, y = Boston$medv)

## ------------------------------------------------------------------------
imp = FeatureImp$new(predictor, loss = "mae")
plot(imp)
imp$results

