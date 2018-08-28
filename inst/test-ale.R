load("~/repos/interpretable-ml-book/data/bike.RData")

library(mlr)
tsk = mlr::makeRegrTask(data = bike, target = "cnt")
lrn = mlr::makeLearner("regr.rpart")
mod = mlr::train(lrn, tsk)
pred = iml::Predictor$new(mod, bike, y = "cnt")


y.fun = function(X.model, newdata) {
  X.model$predict(newdata)[[1]]
}

xx  = ALEPlot(bike, pred, y.fun, J = c("temp", "hum"), K = 10, NA.plot = FALSE)

pdp  = iml::Partial$new(pred, aggregation = "ale", feature = c("temp", "hum"), grid.size = 10)
pdp$plot()

pdp$results
data.table::dcast(pdp$results, .interval1 ~ .interval2, measure.var = .y.hat)

x = ALEPlot::ALEPlot(bike, pred, y.fun, J = c("temp", "hum"), K = 10)
x$f.values

dcast(pdp$results, .interval1 ~ .interval2, value.var = ".y.hat")


pdp  = iml::Partial$new(pred, aggregation = "ale", feature = c("temp", "hum"), grid.size = 10)





# test which model is correct

set.seed(13)
dat = data.frame(
x1 = 3 * runif(1000),
x2 = 3 * runif(1000), 
  x3 = 1)

#dat = rbind(dat, data.frame(x1 = -10, x2 = -10, x3 = 1))

pred.fun = function(model, newdata) {
  dat = newdata
  #res = dat$x1 + dat$x2
  res = rep(0, times = nrow(newdata))
  res[dat$x1 >1 & dat$x1 < 2] = 42 * dat$x2[dat$x1 >1 & dat$x1 < 2]
  res
}
pred = Predictor$new(data = dat, predict.fun = pred.fun)

pdp = Partial$new(pred, "x1")
pdp$plot()
pdp = Partial$new(pred, feature = c("x1", "x2"))
pdp$plot()



K = 100

pdp = Partial$new(pred, c("x1", "x2"), aggregation = "ale", grid.size  = K)
pdp$plot() +   geom_vline(xintercept = c(1,2), color = "red") + 
  geom_vline(data = pdp$results, aes(xintercept = .left)) + 
  geom_vline(data = pdp$results, aes(xintercept = .right), color = "blue", alpha = 0.2) + 
  geom_point(data = dat) + 
  scale_fill_gradient2(low = "red", mid = "yellow", high = "white")

pdp$plot() +   
  scale_fill_gradient2(low = "red", mid = "yellow", high = "white")

xx  = dcast(pdp$results, .interval1 ~ .interval2, value.var = ".y.hat")[, -1]
xx = as.matrix(data.frame(xx))

y.fun = function(X.model, newdata) {
  X.model$predict(newdata)[[1]]
}

x = ALEPlot::ALEPlot(dat, pred, y.fun, J = c("x1", "x2"), K =  K)
abline(v = 1)
abline(v = 2)

x$f.values
all.equal(xx, x$f.values)
