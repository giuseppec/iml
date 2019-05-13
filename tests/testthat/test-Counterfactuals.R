context("Counterfactuals")

library("randomForest")
data("Boston", package  = "MASS")
Boston$chas = as.factor(Boston$chas)

set.seed(1000)
rf =  randomForest(medv ~ ., data = Boston)
X = Boston[-which(names(Boston) == "medv")]
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
  p_stats = cf$plot_statistics(range = TRUE)
  p_search = cf$plot_search()
  hv = cf$calculate_hv()
  div = cf$calculate_diversity()
  
  expect_list(cf$results, any.missing = FALSE, len = 2, unique = TRUE)
  expect_data_frame(cf$log, any.missing = FALSE, nrows = generations + 1, 
    ncols = 12)
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
})

test_that("Counterfactuals only one solutions if target equal prediction", {
  
  cf$explain(x.interest, target = pred)
  
  diff = cf$results$counterfactuals.diff
  
  expect_data_frame(cf$results$counterfactuals, nrow = 1, ncol = ncol(X) + 4)
  expect_data_frame(cf$results$counterfactuals.diff, nrow = 1, ncol = ncol(X) + 4)
  expect_equal(cf$results$counterfactuals$dist.target, 0)
  expect_equal(cf$results$counterfactuals$dist.x.interest, 0)
  expect_true(all(diff[, names(diff) != "pred"] == 0))
  expect_equal(cf$calculate_hv(), 0)
  expect_warning(cf$subset_results(10), 
    "nr.solutions > number of non-dominated solutions")
  expect_list(cf$plot_statistics(), len = 3)
  expect_s3_class(cf$plot_search(), c("gg", "ggplot"))
  expect_s3_class(plot(cf, labels = TRUE, decimal.points = 3), 
    c("gg", "ggplot"))
})



test_that("Counterfactuals only solutions if target in infeasible space", {

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
  expect_number(cf$calculate_hv(), lower = 1)
  expect_true(all(diff(cf$log$fitness.domHV) >= 0))
  expect_true(all(diff(cf$log$dist.target.min) <= 0))
})

cols.sub = c("crim", "chas", "tax")
set.seed(10)
rf =  randomForest(medv ~ ., data = Boston[, c(cols.sub, "medv")])
mod = Predictor$new(rf, data = X[, cols.sub])

# Explain first instance
x.interest = X[1, cols.sub]
pred  = mod$predict(x.interest)[[1]]
target = 30
generations = 30

set.seed(100)
cf = Counterfactuals$new(mod, x.interest = x.interest, target = target,
  mu = 50, generations = generations, use.ice.curve.var = FALSE,
  fixed.features = c("chas", "tax"))


test_that("no good solution can be found by one feature", {
  expect_numeric(cf$results$counterfactuals$pred, upper = 27, 
    any.missing = FALSE)
  expect_true(all(rowSums(cf$results$counterfactuals.diff[, cols.sub] != 0) %in% c(1, 0)))
})


# test_that("if solution already known, solution is found", {
#   target = 23
#   best = x.interes
#   best$crim = 7.269167
#   mod$predict(best)
# })