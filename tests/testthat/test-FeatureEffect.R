context("FeatureEffect()")


test_that("FeatureEffect is Partial", {
  grid.size = 10
  pdp.obj1 = expect_warning(Partial$new(predictor1, aggregation = "pdp", ice = FALSE, feature = 1, grid.size = grid.size))
  pdp.obj2 = FeatureEffect$new(predictor1, method = "pdp", feature = 1, grid.size = grid.size)
  expect_equal(pdp.obj1$results, pdp.obj2$results)
  
  pdp.obj1 = expect_warning(Partial$new(predictor1, aggregation = "pdp", ice = TRUE, feature = 1, grid.size = grid.size))
  pdp.obj2 = FeatureEffect$new(predictor1, method = "pdp+ice", feature = 1, grid.size = grid.size)
  expect_equal(pdp.obj1$results, pdp.obj2$results)
  
  pdp.obj1 = expect_warning(Partial$new(predictor1, aggregation = "ale", ice = FALSE, feature = 1, grid.size = grid.size))
  pdp.obj2 = FeatureEffect$new(predictor1, method = "ale", feature = 1, grid.size = grid.size)
  expect_equal(pdp.obj1$results, pdp.obj2$results)
  
  pdp.obj1 = expect_warning(Partial$new(predictor1, aggregation = "none", ice = TRUE, feature = 1, grid.size = grid.size))
  pdp.obj2 = FeatureEffect$new(predictor1, method = "ice", feature = 1, grid.size = grid.size)
  expect_equal(pdp.obj1$results, pdp.obj2$results)
  
  pdp.obj1 = expect_warning(Partial$new(predictor1, aggregation = "ale", ice = TRUE, feature = c(1,3), grid.size = grid.size))
  pdp.obj2 = FeatureEffect$new(predictor1, method = "ale", feature = c(1,3), grid.size = grid.size)
  expect_equal(pdp.obj1$results, pdp.obj2$results)
})

test_that("FeatureEffect (pdp only) works for single output and single feature", {
  grid.size = 10
  pdp.obj = FeatureEffect$new(predictor1, method = "pdp", feature = 1, grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c("a", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
  
  expect_equal(pdp.obj$feature.name, "a")
  pdp.obj$set.feature(3)
  expect_equal(pdp.obj$feature.name, "c")
  pdp.obj$set.feature("b")
  expect_equal(pdp.obj$feature.name, "b")
  
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size)  
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  
  # Centering 
  pdp.obj$center(0)
  checkPlot(pdp.obj)
  dat = pdp.obj$results
  expect_equal(min(dat$.y), 0)
})

test_that("FeatureEffect (pdp only) works for single output and 2 features, 2D grid.size", {
  ## two numerical features with 2 grid.sizes
  grid.size = c(10,2)
  pdp.obj = FeatureEffect$new(predictor1, method="pdp", feature = c("a", "b"), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size[1] * grid.size[2])  
  expect_equal(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(pdp.obj)
  pdp.obj2 = FeatureEffect$new(predictor1, method="pdp", feature = c("a", "b"), grid.size = grid.size)
  expect_equal(pdp.obj$results, pdp.obj2$results)
  pdp.obj3 = FeatureEffect$new(predictor1, method = "pdp", center.at = 0, feature = c("a", "b"), grid.size = grid.size)
  expect_equal(pdp.obj$results, pdp.obj3$results)
})

test_that("FeatureEffect (pdp only) works for single output and 2 numerical features, 1D grid.size", {
  ## Two numerical with same grid.size
  grid.size = 7
  pdp.obj = FeatureEffect$new(predictor1, method = "pdp", feature = c(1,2), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size * grid.size)  
  expect_equal(nrow(unique(dat)), grid.size * grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})

test_that("FeatureEffect (pdp only) works for single output and numerical + categorical feature", {
  
  ## One feature categorical
  grid.size = 11
  pdp.obj = FeatureEffect$new(predictor1, feature = c(1,3), grid.size = grid.size, method = "pdp")
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "c", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size[1] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size * length(unique(X[,3])))  
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(unique(dat$c), unique(X$c))  
  checkPlot(pdp.obj)
  
  ## One feature categorical
  grid.size = c(7,9)
  pdp.obj = FeatureEffect$new(predictor1, method = "pdp", feature = c(3,2), grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", "b", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size[2] * length(unique(X[,3])))  
  expect_equal(nrow(unique(dat)), grid.size[2] * length(unique(X[,3])))  
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  expect_equal(unique(dat$c), unique(X$c))  
  checkPlot(pdp.obj)
})

test_that("FeatureEffect (pdp) works for categorical output", {
  grid.size = 10
  pdp.obj = FeatureEffect$new(predictor1, method = "pdp+ice", feature = "c", grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c",  ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), length(unique(X$c)) * (nrow(X) + 1))  
  checkPlot(pdp.obj)
})

test_that("FeatureEffect (pdp) works for multiple output", {
  grid.size = 10
  pdp.obj = FeatureEffect$new(predictor2, method = "pdp", feature = "a", grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type"))
  expect_equal(nrow(dat), grid.size * 2)  
  expect_equal(nrow(unique(dat)), grid.size * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})

test_that("FeatureEffect (pdp+ice) works for multiple output", {
  grid.size = 10
  pdp.obj = FeatureEffect$new(predictor2, method = "pdp+ice", feature = "a", grid.size = grid.size)
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * 2 + grid.size * nrow(X) * 2)  
  expect_equal(nrow(unique(dat)), grid.size * 2 + grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})



test_that("FeatureEffect (ice) works for single output and single feature", {
  grid.size = 10
  ice.obj = FeatureEffect$new(predictor1, method = "ice", feature = 1, grid.size = grid.size)
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * nrow(X))  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
  expect_error({FeatureEffect$new(predictor1, method = "ice", feature = c("a", "c"), grid.size = grid.size)})
})

test_that("FeatureEffect (ice) works for multiple output", {
  
  grid.size = 10
  ice.obj = FeatureEffect$new(predictor2, feature = "a", grid.size = grid.size, method = "ice")
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type",  ".id"))
  expect_equal(nrow(dat), grid.size * nrow(X) * 2)  
  expect_equal(nrow(unique(dat)), grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
})


test_that("centered FeatureEffect (ice) works for multiple output", {
  
  grid.size = 10
  ice.obj = FeatureEffect$new(predictor2, feature = "a", grid.size = grid.size, center = 10, method = "pdp+ice")
  dat = ice.obj$results
  expect_equal(ice.obj$center.at, 10)
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".y.hat", ".type", ".id"))
  expect_equal(nrow(dat), (grid.size + 1) * nrow(X) * 2 + (grid.size + 1) * 2)  
  expect_equal(nrow(unique(dat)), (grid.size + 1) * nrow(X) * 2 + (grid.size + 1) * 2)  
  expect_equal(max(dat$a), 10)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
  
  ice.obj$center(-1)
  expect_equal(ice.obj$center.at, -1)
  
  expect_warning({ice.obj$center.at = 10})
  expect_equal(ice.obj$center.at, -1)
  
  dat = ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), -1)
  
})



test_that("method='ale' works for 1D numerical", {
  grid.size = 3
  ale = FeatureEffect$new(predictor1, feature = 1, grid.size = grid.size, method = "ale")
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c(".ale", ".type", "a"))
  expect_equal(nrow(dat), grid.size + 1)  
  expect_equal(nrow(unique(dat)), grid.size + 1)
  expect_equal(max(dat$a, na.rm = TRUE), 5)
  expect_equal(min(dat$a, na.rm = TRUE), 1)
  checkPlot(ale)
  
  expect_equal(ale$feature.name, "a")
  ale$set.feature("b")
  expect_equal(ale$feature.name, "b")
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(ale$results), c(".ale", ".type", "b"))
  expect_equal(nrow(dat), grid.size + 1)  
  expect_equal(nrow(unique(dat)), grid.size + 1 )
  expect_equal(max(dat$b, na.rm = TRUE), 50)
  expect_equal(min(dat$b, na.rm = TRUE), 10)
  
  # Centering 
  expect_warning(ale$center(0))
  
  # multi class output
  grid.size = 3
  ale = FeatureEffect$new(predictor2, feature = "a", method = "ale", grid.size = grid.size)
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".class", ".ale", ".type", "a"))
  expect_equal(nrow(dat), (grid.size + 1) * 2)  
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)
})

test_that("method='ale' works for 2D numerical", {
  ## two numerical features with 2 grid.sizes
  grid.size = c(10, 5)
  ale = FeatureEffect$new(predictor1, feature = c("a", "b"), grid.size = grid.size)
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(".ale", ".right", ".left", ".bottom", ".top", "a", "b", ".type")))
  expect_lte(nrow(dat), grid.size[1] * grid.size[2])
  expect_lte(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(ale)
  ale2 = FeatureEffect$new(predictor1, feature = c("a", "b"), grid.size = grid.size, method = "ale")
  expect_equal(ale$results, ale2$results)
  
  ## two numerical features with 2D prediction
  grid.size = c(10, 5)
  ale = FeatureEffect$new(predictor2, method = "ale", feature = c("a", "b"), grid.size = grid.size)
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(".class", ".ale", ".right", ".left", ".bottom", ".top", "a", "b", ".type")))
  expect_lte(nrow(dat), grid.size[1] * grid.size[2])
  expect_lte(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(ale)
  ale2 = FeatureEffect$new(predictor2, feature = c("a", "b"), grid.size = grid.size, method = "ale")
  expect_equal(ale$results, ale2$results)
})

test_that("iml::FeatureEffect with method='ale' equal to ALEPLot::ALEPlot", {
  require("ALEPlot")
  
  # one numerical feature
  grid.size = 3
  ale = FeatureEffect$new(predictor1, feature = 1, grid.size = grid.size)
  ale.dat = ale$results
  ale.original = ALEPlot(X, predictor1, pred.fun = function(X.model, newdata){X.model$predict(newdata)[,1]}, J = 1,K = 3)
  # equality of x values and ALE estimates
  expect_equal(ale.dat$a, ale.original$x.values)
  expect_equal(ale.dat$.ale, ale.original$f.values)
  
  # two numerical features
  grid.size = c(5)
  ale = FeatureEffect$new(predictor1, method = "ale", feature = c("a", "b"), grid.size = grid.size)
  ale.dat = ale$results
  ale.original = ALEPlot(X, predictor1, pred.fun = function(X.model, newdata){X.model$predict(newdata)[,1]}, 
    J = c("a", "b"), K = grid.size)
  
  expect_equal(unique(ale.dat$a), ale.original$x.values[[1]])
  expect_equal(unique(ale.dat$b), ale.original$x.values[[2]])
  dd = unname(as.matrix(data.table::dcast(ale.dat, a ~ b, value.var = ".ale"))[,-1])
  dd.orig = unname(ale.original$f.values)
  expect_equal(dd, dd.orig)
  
  # two numerical features, but this time with more interaction
  grid.size = c(5)
  ale = FeatureEffect$new(predictor1.inter,feature = c("a", "b"), grid.size = grid.size)
  ale.dat = ale$results
  ale.original = ALEPlot(X, predictor1.inter, pred.fun = function(X.model, newdata){X.model$predict(newdata)[,1]}, 
    J = c("a", "b"), K = grid.size)
  
  expect_equal(unique(ale.dat$a), ale.original$x.values[[1]])
  expect_equal(unique(ale.dat$b), ale.original$x.values[[2]])
  dd = as.matrix(data.table::dcast(ale.dat, a ~ b, value.var = ".ale"))[,-1]
  expect_equal(unname(ale.original$f.values), unname(dd))
  
  # one categorical feature
  ale = FeatureEffect$new(predictor1, feature = "c")
  ale.dat = ale$results
  ale.original = ALEPlot(X, predictor1, pred.fun = function(X.model, newdata){X.model$predict(newdata)[,1]}, J = 3)
  # equality of x values and ALE estimates
  expect_equal(as.character(ale.dat$c), ale.original$x.values)
  expect_equal(ale.dat$.ale, ale.original$f.values)
  
  # another categorical feature
  ale = FeatureEffect$new(predictor1.inter, feature = "c", method = "ale")
  ale.dat = ale$results
  ale.original = ALEPlot(X, predictor1.inter, pred.fun = function(X.model, newdata){X.model$predict(newdata)[,1]}, J = 3)
  # equality of x values and ALE estimates
  expect_equal(as.character(ale.dat$c), ale.original$x.values)
  expect_equal(ale.dat$.ale, ale.original$f.values)
  
  # one numerical, one categorical feature
  ale = FeatureEffect$new(predictor1.inter, feature = c("a", "c"))
  ale.dat = ale$results
  ale.original = ALEPlot(X, predictor1.inter, pred.fun = function(X.model, newdata){X.model$predict(newdata)[,1]}, J = c(3,1))
  # equality of x values and ALE estimates
  expect_equal(as.character(unique(ale.dat$c)), ale.original$x.values[[1]])
  expect_equal(unique(ale.dat$a), ale.original$x.values[[2]])
  res.iml = unname(as.matrix(data.table::dcast(ale.dat, c ~ a, value.var = ".ale")[, -1]))
  expect_equal(res.iml, unname(ale.original$f.values))
})


test_that("method='ale' works for 1D categorical", {
  ale = FeatureEffect$new(predictor1, feature = 'c')
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c( ".ale", "c"))
  expect_equal(nrow(dat), length(unique(X$c)))  
  checkPlot(ale)
  
  expect_equal(ale$feature.name, "c")
  ale$set.feature(4)
  expect_equal(ale$feature.name, "d")
  expect_equal(colnames(ale$results), c(".ale", "d"))
  # Centering 
  expect_warning(ale$center(0))
  
  # multi class output
  ale = FeatureEffect$new(predictor2, feature = "c", method = "ale", grid.size = 100)
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".class", ".ale",  "c"))
  expect_equal(nrow(dat), length(unique(X$c)) * 2)  
  checkPlot(ale)
  
  # ordered by the user
  X2 = X
  X2$c = ordered(X2$c)
  pred1.order = Predictor$new(data = X2, y = y, predict.fun = f)
  ale = FeatureEffect$new(pred1.order, feature = 'c')
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c( ".ale", "c"))
  expect_equal(nrow(dat), length(unique(X$c)))  
  expect_equal(as.character(dat$c), c("a", "b", "c"))
  checkPlot(ale)
})


test_that("method='ale' works for 2D numerical x categorical", {
  ## two numerical features with 2 grid.sizes
  grid.size = c(2)
  ale = FeatureEffect$new(predictor1, feature = c("a", "c"), grid.size = grid.size)
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(".ale", ".right", ".left", ".bottom", ".top", "a", "c", ".type")))
  expect_equal(nrow(dat), (1 + grid.size[1]) * length(unique(X$c)))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)
  ale2 = FeatureEffect$new(predictor1, feature = c("c", "a"), grid.size = grid.size, method = "ale")
  expect_equal(ale$results, ale2$results)
  
  ## two numerical features with 2D prediction
  grid.size = c(3, 5)
  ale = FeatureEffect$new(predictor2,  feature = c("a", "c"), grid.size = grid.size)
  dat = ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(".class", ".ale", ".right", ".left", ".bottom", ".top", "a", "c", ".type")))
  expect_equal(nrow(dat), (1 + grid.size[1]) * length(unique(X$c)) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)
  ale2 = FeatureEffect$new(predictor2, feature = c("a", "c"), grid.size = grid.size, method = "ale")
  expect_equal(ale$results, ale2$results)
})


