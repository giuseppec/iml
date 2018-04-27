## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
set.seed(1014)

## ------------------------------------------------------------------------
data("Boston", package  = "MASS")
head(Boston)

## ---- message = FALSE----------------------------------------------------
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

## ------------------------------------------------------------------------
pdp.obj = Partial$new(predictor, feature = "lstat")
pdp.obj$plot()

## ------------------------------------------------------------------------
pdp.obj$set.feature("rm")
pdp.obj$plot()

## ------------------------------------------------------------------------
pdp.obj$center(min(Boston$rm))
pdp.obj$plot()

## ------------------------------------------------------------------------
tree = TreeSurrogate$new(predictor, maxdepth = 2)
plot(tree)

## ------------------------------------------------------------------------
head(tree$predict(Boston))

## ------------------------------------------------------------------------
lime.explain = LocalModel$new(predictor, x.interest = X[1,])
lime.explain$results
plot(lime.explain)

## ------------------------------------------------------------------------
lime.explain$explain(X[2,])
plot(lime.explain)

## ------------------------------------------------------------------------
shapley = Shapley$new(predictor, x.interest = X[1,])
shapley$plot()

## ------------------------------------------------------------------------
shapley$explain(x.interest = X[2,])
shapley$plot()

## ------------------------------------------------------------------------
results = shapley$results
head(results)

