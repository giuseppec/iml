
f = function(x, multi = FALSE) {
  pred = unlist(x[1] + x[2] + 100 * (x[3] == "a")) / (155)
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

predictor1 = Predictor$new(f, data = X, y = y)
predictor2 = Predictor$new(f, data = X, y = y2, predict.args = list(multi = TRUE))
predictor3 = Predictor$new(f, data = X, predict.args = list(multi = TRUE), class = 2)
