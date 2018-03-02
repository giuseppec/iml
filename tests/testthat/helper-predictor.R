
f = function(newdata, multi = FALSE) {
  pred = unlist(newdata[1] + newdata[2] + 100 * (newdata[3] == "a")) / (155)
  dat = data.frame(pred = pred)
  if (multi) dat$pred2 = 1 - dat$pred
  dat
}

X = data.frame(a = c(1, 2, 3, 4, 5), 
  b = c(10, 20, 30, 40, 50), 
  c = factor(c("a", "b", "c", "a", "b")), 
  d = factor(c("A", "A", "B", "B", "B")))

set.seed(12)
y = f(X) + rnorm(nrow(X))
y2 = factor(ifelse(X$b + X$a < 20, "pred", "pred2"))

predictor1 = Predictor$new(data = X, y = y, predict.fun = f)
predict.fun = function(obj, newdata) obj(newdata, multi = TRUE)
predictor2 = Predictor$new(f, data = X, y = y2, predict.fun = predict.fun)
predictor3 = Predictor$new(f, data = X, predict.fun = predict.fun, class = 2)
