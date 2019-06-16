context("Counterfactuals")

library("randomForest")
data("Boston", package  = "MASS")
Boston$chas = as.factor(Boston$chas)

set.seed(1000)
rf =  randomForest(medv ~ ., data = Boston)
X = Boston[,-which(names(Boston) == "medv")]
mod = Predictor$new(rf, data = X)

# Explain first instance 
x.interest = X[1,]
pred  = mod$predict(x.interest)[[1]]
target = 30
generations = 10

set.seed(100)
cf = Counterfactuals$new(mod, x.interest = x.interest, target = target, 
  mu = 50, generations = generations, use.ice.curve.var = FALSE)


test_that("Counterfactuals has correct output", {
  
  results.sub = cf$subset_results(10)
  p = plot(cf, labels = TRUE, decimal.points = 5, nr.solutions = 10)
  p_stats = cf$plot_statistics()
  p_search = cf$plot_search()
  hv = cf$calculate_hv()
  div = cf$calculate_diversity()
  
  expect_list(cf$results, any.missing = FALSE, len = 2, unique = TRUE)
  expect_data_frame(cf$log, any.missing = FALSE, nrows = generations + 1, 
    ncols = 11)
  expect_list(results.sub, len = 2, unique = TRUE)
  expect_data_frame(results.sub$counterfactuals, nrow = 10, 
    ncol = ncol(X) + 4)
  expect_data_frame(results.sub$counterfactuals.diff, nrow = 10, 
    ncol = ncol(X) + 4)
  expect_s3_class(p, c("gg", "ggplot"))
  expect_list(p_stats, len = 3)
  expect_s3_class(p_search, c("gg", "ggplot"))
  expect_number(hv, lower = 0, finite = TRUE)
  expect_number(div, lower = 0, finite = TRUE)
  diff = cf$results$counterfactuals.diff
  diff_1 = diff[diff$nr.changed == 1, names(X)]
  expect_true(all(rowSums(diff_1 != 0) == 1))
  
  # continue search 
  cf$continue_search(5L)
  expect_list(cf$results, any.missing = FALSE, len = 2, unique = TRUE)
  expect_data_frame(cf$log, any.missing = FALSE, nrows = generations + 1L + 5L, 
    ncols = 11)
  
})

test_that("Counterfactuals only one solutions if target equal prediction", {
  
  cf$explain(x.interest, target = pred)
  
  diff = cf$results$counterfactuals.diff
  
  expect_data_frame(cf$results$counterfactuals, nrow = 1, ncol = ncol(X) + 4)
  expect_data_frame(cf$results$counterfactuals.diff, nrow = 1, ncol = ncol(X) + 4)
  expect_equal(cf$results$counterfactuals$dist.target, 0)
  expect_equal(cf$results$counterfactuals$dist.x.interest, 0)
  expect_true(all(diff[, names(diff) != "pred"] == 0))
  expect_identical(cf$calculate_hv(), NaN)
  expect_warning(cf$subset_results(10), 
    "nr.solutions out of range")
  expect_list(cf$plot_statistics(), len = 3)
  expect_s3_class(cf$plot_search(), c("gg", "ggplot"))
  expect_s3_class(plot(cf, labels = TRUE, decimal.points = 3), 
    c("gg", "ggplot"))
})

test_that("colnames of x.interest not identical to training data names", {
  names(x.interest) = c("crim2", names(x.interest)[2:length(names(x.interest))])
  expect_error(Counterfactuals$new(mod, x.interest = x.interest, target = target, 
    mu = 50, generations = generations, use.ice.curve.var = FALSE), 
    "colnames of x.interest must be identical to training data")
  
})



test_that("Counterfactual solutions if target in infeasible space", {

  set.seed(100)
  target = -100
  cf$explain(x.interest, target = target)
  diff = cf$results$counterfactuals.diff
  
  expect_data_frame(cf$results$counterfactuals, min.rows = 1, ncol = ncol(X) + 4)
  expect_data_frame(cf$results$counterfactuals.diff, min.rows = 1, ncol = ncol(X) + 4)
  expect_numeric(cf$results$counterfactuals$dist.target, 
    upper = abs(target) + pred)
  expect_list(cf$plot_statistics(), len = 3)
  expect_s3_class(cf$plot_search(), c("gg", "ggplot"))
  expect_s3_class(plot(cf, labels = TRUE, decimal.points = 3), 
    c("gg", "ggplot"))
  expect_number(cf$calculate_hv(), lower = 0)
  expect_true(all(diff(cf$log$fitness.domHV) >= 0))
  expect_true(all(diff(cf$log$dist.target.min) <= 0))
})



test_that("no good solution can be found by one feature", {
  cols.sub = c("crim", "chas", "tax")
  set.seed(10)
  rf =  randomForest(medv ~ ., data = Boston[, c(cols.sub, "medv")])
  mod = Predictor$new(rf, data = X[, cols.sub])

  x.interest = X[1, cols.sub]
  pred  = mod$predict(x.interest)[[1]]
  target = 30
  generations = 30
  
  set.seed(100)
  cf = Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 50, generations = generations, use.ice.curve.var = FALSE,
    fixed.features = c("chas", "tax"))
  
  expect_numeric(cf$results$counterfactuals$pred, upper = 27, 
    any.missing = FALSE)
  expect_true(all(rowSums(cf$results$counterfactuals.diff[, cols.sub] != 0) %in% c(1, 0)))
})


test_that("if solution already known, solution is found", {
  test.df = Boston[, c("medv", "rm")]
  n = nrow(test.df)
  test.df$random1 = runif(n, 0, 1) 
  test.df$random2 = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  test.df$random3 = rnorm(n, 10, 1)
  lm =  lm(medv ~ ., data = test.df)
  test.df = test.df[,-1]
  x.interest = test.df[1,]
  mod = Predictor$new(lm, data = test.df)
  
  # get.rm = function(x, y) {
  #   return((y - lm$coefficient*x
  
  best = x.interest
  best$rm = 5.97644
  mod$predict(best)
  
  set.seed(100)
  cf.lm = Counterfactuals$new(predictor = mod, x.interest = x.interest, target = 20, 
    generations = 10, use.ice.curve.var = TRUE)
  cf.rm = cf.lm$results$counterfactuals$rm[cf.lm$results$counterfactuals$nr.changed == 1]
  #expect_true(any(abs(cf.rm - best$rm) < 0.005))
})


test_that("fixed features works with numbers and lower and upper specified", {
  
  
  test.df = Boston[, c("lstat", "rm", "medv")]
  lm =  lm(medv ~ ., data = test.df)
  mod = Predictor$new(lm, data = test.df)
  test.df = test.df[, names(test.df) %in% c("lstat", "rm")]
  x.interest = test.df[5,]
  
  cf.lm = Counterfactuals$new(predictor = mod, x.interest = x.interest, 
    target = 20, fixed.features = 1, generations = 10)
  expect_true(all(cf.lm$results$counterfactuals.diff$lstat == 0))
  
  # works although ordering of x.interest columns changed
  x.interest = x.interest[, c("rm", "lstat")]
  cf.lm = Counterfactuals$new(predictor = mod, x.interest = x.interest, 
    target = 20, fixed.features = 1, generations = 10)
  expect_true(all(cf.lm$results$counterfactuals.diff$lstat == 0))
})

test_that("works with lower and upper specified", {
  lower = 4.9
  upper = 5.1
  names(lower) = "lstat"
  names(upper) = "lstat"
  cf.lm.range = Counterfactuals$new(predictor = mod, x.interest = x.interest, 
    target = 20, generations = 10, 
    lower = lower, upper = upper)
  expect_true(all(cf.lm.range$results$counterfactuals$lstat >= 4.9 & 
      cf.lm.range$results$counterfactuals$lstat <= 5.1))
  
  # x.interest not in range lower to upper
  lower["lstat"] = 5.4
  expect_error(Counterfactuals$new(predictor = mod, x.interest = x.interest, 
    target = 20, generations = 10, 
    lower = lower, upper = upper), "'upper' is smaller than the corresponding one in 'lower'")
  
  # lower > upper 
  lower["lstat"] = 6
  expect_error(Counterfactuals$new(predictor = mod, x.interest = x.interest, 
    target = 20, generations = 10, 
    lower = lower, upper = upper), 
    "some component of 'upper' is smaller than the corresponding one in 'lower'")
  
  plot(cf.lm.range, nr.changed = 2)
  freq_1 = cf.lm.range$calculate_freq(plot = TRUE)
  expect_numeric(freq_1, len = ncol(x.interest), lower = 0, upper = 1)
  
  set.seed(1234)
  freq_2 = calculate_freq_wrapper(cf.lm.range, obs = x.interest)
  expect_numeric(freq_2, len = ncol(x.interest), lower = 0, upper = 1)
  
  cf.lm.range$target = NULL
  expect_error(calculate_freq_wrapper(cf.lm.range, obs = x.interest), 
    "target not specified")
  
  set.seed(1234)
  freq_3 = calculate_freq_wrapper(cf.lm.range, row.ids = 1, 
    target = 20, plot = TRUE)
  
  expect_equal(freq_2, freq_3)
  
  # value of x.interest not in lower/upper
  lower = 5.3
  upper = 5.6
  names(lower) = "lstat"
  names(upper) = "lstat"
  expect_error(Counterfactuals$new(predictor = mod, x.interest = x.interest, 
    target = 20, generations = 10, 
    lower = lower, upper = upper), "Feature values of x.interest outside range of training data")

  
})


test_that("if target infinite, error", {
  target = c(Inf, Inf)
  expect_error(Counterfactuals$new(mod, x.interest = x.interest, target = target, 
    mu = 50, generations = generations, use.ice.curve.var = FALSE), 
    "One element of target must be finite")
  cf = Counterfactuals$new(mod, x.interest = x.interest,  
    mu = 50, generations = generations, use.ice.curve.var = FALSE)
  expect_error(cf$explain(x.interest, target = Inf), 
    "One element of target must be finite")
  
})

test_that("refpoint correct if Inf in target", {
  cf1 = Counterfactuals$new(mod, x.interest = x.interest, target = c(-Inf, 20),
    mu = 20, generations = 10)
  expect_equal(cf1$.__enclos_env__$private$ref.point[1], mod$predict(x.interest)[[1]] - 20)
  cf2 = Counterfactuals$new(mod, x.interest = x.interest, target = c(27, Inf),
    mu = 20, generations = 10)
  expect_equal(cf2$.__enclos_env__$private$ref.point[1], 27 - mod$predict(x.interest)[[1]])
})

