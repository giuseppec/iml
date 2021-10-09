test_that("FeatureEffect is Partial", {
  grid.size <- 10
  expect_warning(Partial$new(predictor1,
    aggregation = "pdp",
    ice = FALSE, feature = 1, grid.size = grid.size
  ))
  pdp.obj1 <- suppressWarnings(Partial$new(predictor1,
    aggregation = "pdp",
    ice = FALSE, feature = 1, grid.size = grid.size
  ))
  pdp.obj2 <- FeatureEffect$new(predictor1,
    method = "pdp", feature = 1,
    grid.size = grid.size
  )
  expect_equal(pdp.obj1$results, pdp.obj2$results)

  expect_warning(Partial$new(predictor1,
    aggregation = "pdp",
    ice = TRUE, feature = 1, grid.size = grid.size
  ))
  pdp.obj1 <- suppressWarnings(Partial$new(predictor1,
    aggregation = "pdp",
    ice = TRUE, feature = 1, grid.size = grid.size
  ))
  pdp.obj2 <- FeatureEffect$new(predictor1,
    method = "pdp+ice", feature = 1,
    grid.size = grid.size
  )
  expect_equal(pdp.obj1$results, pdp.obj2$results)

  expect_warning(Partial$new(predictor1,
    aggregation = "ale",
    ice = FALSE, feature = 1, grid.size = grid.size
  ))
  pdp.obj1 <- suppressWarnings(Partial$new(predictor1,
    aggregation = "ale",
    ice = FALSE, feature = 1, grid.size = grid.size
  ))
  pdp.obj2 <- FeatureEffect$new(predictor1,
    method = "ale", feature = 1,
    grid.size = grid.size
  )
  expect_equal(pdp.obj1$results, pdp.obj2$results)

  expect_warning(Partial$new(predictor1,
    aggregation = "none",
    ice = TRUE, feature = 1, grid.size = grid.size
  ))
  pdp.obj1 <- suppressWarnings(Partial$new(predictor1,
    aggregation = "none",
    ice = TRUE, feature = 1, grid.size = grid.size
  ))

  pdp.obj2 <- FeatureEffect$new(predictor1,
    method = "ice", feature = 1,
    grid.size = grid.size
  )
  expect_equal(pdp.obj1$results, pdp.obj2$results)

  expect_warning(Partial$new(predictor1,
    aggregation = "ale",
    ice = TRUE, feature = c(1, 3), grid.size = grid.size
  ))
  pdp.obj1 <- suppressWarnings(Partial$new(predictor1,
    aggregation = "ale",
    ice = TRUE, feature = c(1, 3), grid.size = grid.size
  ))

  pdp.obj2 <- FeatureEffect$new(predictor1,
    method = "ale", feature = c(1, 3),
    grid.size = grid.size
  )
  expect_equal(pdp.obj1$results, pdp.obj2$results)
})

test_that("FeatureEffect (method=pdp) works for single output and single feature", {
  grid.size <- 10
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp", feature = 1,
    grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c("a", ".value", ".type"))
  expect_equal(nrow(dat), grid.size)
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
  expect_numeric(pdp.obj$predict(1), len = 1)
  expect_true(all(is.na(pdp.obj$predict(c(-100, 1000), extrapolate = FALSE))))
  expect_numeric(pdp.obj$predict(1000, extrapolate = TRUE))
  expect_numeric(pdp.obj$predict(c(1, 2)), len = 2, any.missing = FALSE)
  expect_numeric(pdp.obj$predict(X), len = nrow(X), any.missing = FALSE)
  expect_numeric(pdp.obj$predict(X[1:2, ]), len = 2, any.missing = FALSE)
  expect_equal(pdp.obj$predict(10), as.numeric(NA))
  expect_equal(pdp.obj$feature.name, "a")

  pdp.obj$set.feature("c")
  # make sure the merging in $predict does not shuffle results
  expect_equal(pdp.obj$predict(X), pdp.obj$results$.value[pdp.obj$results$c[X$c]])

  pdp.obj$set.feature("d")
  expect_numeric(pdp.obj$predict("A", extrapolate = FALSE), len = 1)
  expect_numeric(pdp.obj$predict("A", extrapolate = TRUE), len = 1)
  expect_numeric(pdp.obj$predict(c("A", "B")), len = 2, any.missing = FALSE)
  expect_numeric(pdp.obj$predict(X), len = nrow(X), any.missing = FALSE)
  expect_numeric(pdp.obj$predict(X[1:2, ]), len = 2, any.missing = FALSE)
  expect_equal(pdp.obj$predict(10), as.numeric(NA))

  pdp.obj$set.feature(3)
  expect_equal(pdp.obj$feature.name, "c")
  pdp.obj$set.feature("b")
  expect_equal(pdp.obj$feature.name, "b")

  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("b", ".value", ".type"))
  expect_equal(nrow(dat), grid.size)
  expect_equal(nrow(unique(dat)), grid.size)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)

  # Centering
  pdp.obj$center(0)
  checkPlot(pdp.obj)
  dat <- pdp.obj$results
  expect_equal(min(dat$.value), 0)


  # Centering
  p <- plot(pdp.obj, ylim = c(1, 2))
  expect_s3_class(p, c("gg", "ggplot"))
  suppressWarnings(plot(p))

  # User provided grid
  grpoints  <- 1:3
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp", feature = 1,
    grid.points = grpoints
  )
  expect_equal(pdp.obj$results$a, grpoints)
  p <- plot(pdp.obj, ylim = c(1, 2))
  expect_s3_class(p, c("gg", "ggplot"))
  suppressWarnings(plot(p))

  grpoints  <- c(1, 1, 1:3)
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp", feature = 1,
    grid.points = grpoints
  )
  expect_equal(pdp.obj$results$a, unique(grpoints))
})

test_that("FeatureEffect (method=pdp) works for single output and 2 features, 2D grid.size", {
  ## two numerical features with 2 grid.sizes
  grid.size <- c(10, 2)
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp",
    feature = c("a", "b"), grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".value", ".type"))
  expect_equal(nrow(dat), grid.size[1] * grid.size[2])
  expect_equal(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(pdp.obj)
  expect_error(pdp.obj$predict(1))
  pdp.obj2 <- FeatureEffect$new(predictor1,
    method = "pdp",
    feature = c("a", "b"), grid.size = grid.size
  )
  expect_equal(pdp.obj$results, pdp.obj2$results)
  pdp.obj3 <- FeatureEffect$new(predictor1,
    method = "pdp", center.at = 0,
    feature = c("a", "b"), grid.size = grid.size
  )
  expect_equal(pdp.obj$results, pdp.obj3$results)

  grid.points <- list(1:3, c(10, 10, 20, 40))
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp",
    feature = c("a", "b"), grid.points = grid.points
  )
  dat = pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".value", ".type"))
  expect_equal(nrow(dat), length(unique(grid.points[[1]])) * length(unique(grid.points[[2]])))
  expect_equal(max(dat$a), 3)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 40)
  expect_equal(min(dat$b), 10)

})

test_that("FeatureEffect (method=pdp) works for single output and 2 numerical features, 1D grid.size", {
  ## Two numerical with same grid.size
  grid.size <- 7
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp", feature = c(1, 2),
    grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "b", ".value", ".type"))
  expect_equal(nrow(dat), grid.size * grid.size)
  expect_equal(nrow(unique(dat)), grid.size * grid.size)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
  expect_error(pdp.obj$predict(1))
})

test_that("FeatureEffect (method=pdp) works for single output and numerical + categorical feature", {

  ## One feature categorical
  grid.size <- 11
  pdp.obj <- FeatureEffect$new(predictor1,
    feature = c(1, 3),
    grid.size = grid.size, method = "pdp"
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", "c", ".value", ".type"))
  expect_equal(nrow(dat), grid.size[1] * length(unique(X[, 3])))
  expect_equal(nrow(unique(dat)), grid.size * length(unique(X[, 3])))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(unique(dat$c), unique(X$c))
  checkPlot(pdp.obj)
  expect_error(pdp.obj$predict(1))

  ## One feature categorical
  grid.size <- c(7, 9)
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp", feature = c(3, 2),
    grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", "b", ".value", ".type"))
  expect_equal(nrow(dat), grid.size[2] * length(unique(X[, 3])))
  expect_equal(nrow(unique(dat)), grid.size[2] * length(unique(X[, 3])))
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  expect_equal(unique(dat$c), unique(X$c))
  checkPlot(pdp.obj)
  expect_error(pdp.obj$predict(1))

  # Using grid points
  grpoints <- list(c("a", "b"), c(10, 11))
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp", feature = c(3, 2),
    grid.points = grpoints
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", "b", ".value", ".type"))
  expect_equal(nrow(dat), 4)
  expect_equal(nrow(unique(dat)), 4)
  expect_equal(max(dat$b), 11)
  expect_equal(min(dat$b), 10)
  expect_equal(as.character(unique(dat$c)), grpoints[[1]])
  checkPlot(pdp.obj)
  expect_error(pdp.obj$predict(1))

})

test_that("FeatureEffect (pdp) works for categorical output", {
  grid.size <- 10
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp+ice", feature = "c",
    grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", ".value", ".type", ".id"))
  expect_equal(nrow(dat), length(unique(X$c)) * (nrow(X) + 1))
  checkPlot(pdp.obj)

  # User-defined grid points
  pdp.obj <- FeatureEffect$new(predictor1,
    method = "pdp+ice", feature = "c",
    grid.points = c("a", "b")
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("c", ".value", ".type", ".id"))
  expect_equal(nrow(dat), 2 * (nrow(X) + 1))
  checkPlot(pdp.obj)
})

test_that("FeatureEffect (pdp) works for multiple output", {
  grid.size <- 10
  pdp.obj <- FeatureEffect$new(predictor2,
    method = "pdp", feature = "a",
    grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_error(pdp.oj$predict(2))
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".value", ".type"))
  expect_equal(nrow(dat), grid.size * 2)
  expect_equal(nrow(unique(dat)), grid.size * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})

test_that("FeatureEffect (pdp+ice) works for multiple output", {
  grid.size <- 10
  pdp.obj <- FeatureEffect$new(predictor2,
    method = "pdp+ice", feature = "a",
    grid.size = grid.size
  )
  dat <- pdp.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".value", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * 2 + grid.size * nrow(X) * 2)
  expect_equal(nrow(unique(dat)), grid.size * 2 + grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(pdp.obj)
})



test_that("FeatureEffect (ice) works for single output and single feature", {
  grid.size <- 10
  ice.obj <- FeatureEffect$new(predictor1,
    method = "ice", feature = 1,
    grid.size = grid.size
  )
  dat <- ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".value", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * nrow(X))
  expect_equal(nrow(unique(dat)), grid.size * nrow(X))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
  expect_error({
    FeatureEffect$new(predictor1,
      method = "ice", feature = c("a", "c"),
      grid.size = grid.size
    )
  })
})

test_that("FeatureEffect (ice) works for multiple output", {
  grid.size <- 10
  ice.obj <- FeatureEffect$new(predictor2,
    feature = "a", grid.size = grid.size,
    method = "ice"
  )
  dat <- ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".value", ".type", ".id"))
  expect_equal(nrow(dat), grid.size * nrow(X) * 2)
  expect_equal(nrow(unique(dat)), grid.size * nrow(X) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)
})


test_that("centered FeatureEffect (ice) works for multiple output", {
  skip("temp")

  grid.size <- 10
  ice.obj <- FeatureEffect$new(predictor2,
    feature = "a", grid.size = grid.size,
    center = 10, method = "pdp+ice"
  )
  dat <- ice.obj$results
  expect_equal(ice.obj$center.at, 10)
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c("a", ".class", ".value", ".type", ".id"))
  expect_equal(nrow(dat), (grid.size + 1) * nrow(X) * 2 + (grid.size + 1) * 2)
  expect_equal(nrow(unique(dat)), (grid.size + 1) * nrow(X) * 2 + (grid.size + 1) * 2)
  expect_equal(max(dat$a), 10)
  expect_equal(min(dat$a), 1)
  checkPlot(ice.obj)

  ice.obj$center(-1)
  expect_equal(ice.obj$center.at, -1)

  expect_warning({
    ice.obj$center.at <- 10
  })
  expect_equal(ice.obj$center.at, -1)

  dat <- ice.obj$results
  expect_class(dat, "data.frame")
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), -1)
})



test_that("method='ale' works for 1D numerical", {
  grid.size <- 3
  ale <- FeatureEffect$new(predictor1,
    feature = 1, grid.size = grid.size,
    method = "ale"
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c(".type", ".value", "a"))
  expect_equal(nrow(dat), grid.size + 1)
  expect_equal(nrow(unique(dat)), grid.size + 1)
  expect_equal(max(dat$a, na.rm = TRUE), 5)
  expect_equal(min(dat$a, na.rm = TRUE), 1)
  checkPlot(ale)

  expect_equal(ale$feature.name, "a")
  ale$set.feature("b")
  expect_equal(ale$feature.name, "b")
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(ale$results), c(".type", ".value", "b"))
  expect_equal(nrow(dat), grid.size + 1)
  expect_equal(nrow(unique(dat)), grid.size + 1)
  expect_equal(max(dat$b, na.rm = TRUE), 50)
  expect_equal(min(dat$b, na.rm = TRUE), 10)

  # Centering
  expect_warning(ale$center(0))

  # multi class output
  grid.size <- 3
  ale <- FeatureEffect$new(predictor2,
    feature = "a", method = "ale",
    grid.size = grid.size
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".type", ".class", ".value", "a"))
  expect_equal(nrow(dat), (grid.size + 1) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)

  ale <- FeatureEffect$new(predictor1,
    feature = 1, grid.points = c(1,2,3),
    method = "ale"
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c(".type", ".value", "a"))
  expect_equal(nrow(dat), 3)
  expect_equal(nrow(unique(dat)), 3)
  expect_equal(max(dat$a, na.rm = TRUE), 3)
  expect_equal(min(dat$a, na.rm = TRUE), 1)
  checkPlot(ale)
})

test_that("method='ale' works for 2D numerical", {
  ## two numerical features with 2 grid.sizes
  grid.size <- c(10, 5)
  ale <- FeatureEffect$new(predictor1,
    feature = c("a", "b"),
    grid.size = grid.size
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(
    ".type", ".ale", ".right", ".left",
    ".bottom", ".top", "a", "b"
  )))
  expect_lte(nrow(dat), grid.size[1] * grid.size[2])
  expect_lte(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(ale)
  ale2 <- FeatureEffect$new(predictor1,
    feature = c("a", "b"),
    grid.size = grid.size, method = "ale"
  )
  expect_equal(ale$results, ale2$results)

  ## two numerical features with 2D prediction
  grid.size <- c(10, 5)
  ale <- FeatureEffect$new(predictor2,
    method = "ale", feature = c("a", "b"),
    grid.size = grid.size
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(
    ".type", ".class", ".ale", ".right",
    ".left", ".bottom", ".top", "a", "b"
  )))
  expect_lte(nrow(dat), grid.size[1] * grid.size[2])
  expect_lte(nrow(unique(dat)), grid.size[1] * grid.size[2])
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  expect_equal(max(dat$b), 50)
  expect_equal(min(dat$b), 10)
  checkPlot(ale)
  ale2 <- FeatureEffect$new(predictor2,
    feature = c("a", "b"),
    grid.size = grid.size, method = "ale"
  )
  expect_equal(ale$results, ale2$results)

  grid.points <- list(3:5, c(30, 40, 52))
  ale <- FeatureEffect$new(predictor2,
    method = "ale", feature = c("a", "b"),
    grid.points = grid.points
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(
    ".type", ".class", ".ale", ".right",
    ".left", ".bottom", ".top", "a", "b"
  )))
  expect_lte(nrow(dat), grid.size[1] * grid.size[2] * 2)
  expect_lte(nrow(unique(dat)), 2*9)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 3)
  expect_equal(max(dat$b), 52)
  expect_equal(min(dat$b), 30)
  expect_equal(3:5, unique(dat$a))
  expect_equal(c(30, 40, 52), unique(dat$b))
  checkPlot(ale)

})

test_that("iml::FeatureEffect with method='ale' equal to ALEPLot::ALEPlot", {
  require("ALEPlot")

  # one numerical feature
  grid.size <- 3
  ale <- FeatureEffect$new(predictor1, feature = 1, grid.size = grid.size)
  ale.dat <- ale$results
  ale.original <- ALEPlot(X, predictor1, pred.fun = function(X.model, newdata) {
    X.model$predict(newdata)[, 1]
  }, J = 1, K = 3)
  # equality of x values and ALE estimates
  expect_equal(ale.dat$a, ale.original$x.values)
  expect_equal(ale.dat$.value, ale.original$f.values)

  # two numerical features
  grid.size <- c(5)
  ale <- FeatureEffect$new(predictor1,
    method = "ale", feature = c("a", "b"),
    grid.size = grid.size
  )
  ale.dat <- ale$results
  ale.original <- ALEPlot(X, predictor1,
    pred.fun = function(X.model, newdata) {
      X.model$predict(newdata)[, 1]
    },
    J = c("a", "b"), K = grid.size
  )

  expect_equal(unique(ale.dat$a), ale.original$x.values[[1]])
  expect_equal(unique(ale.dat$b), ale.original$x.values[[2]])
  dd <- unname(as.matrix(data.table::dcast(as.data.table(ale.dat), a ~ b,
    value.var = ".ale"
  ))[, -1])
  dd.orig <- unname(ale.original$f.values)
  expect_equal(dd, dd.orig)

  # two numerical features, but this time with more interaction
  grid.size <- c(5)
  ale <- FeatureEffect$new(predictor1.inter,
    feature = c("a", "b"),
    grid.size = grid.size
  )
  ale.dat <- ale$results
  ale.original <- ALEPlot(X, predictor1.inter,
    pred.fun = function(X.model, newdata) {
      X.model$predict(newdata)[, 1]
    },
    J = c("a", "b"), K = grid.size
  )

  expect_equal(unique(ale.dat$a), ale.original$x.values[[1]])
  expect_equal(unique(ale.dat$b), ale.original$x.values[[2]])
  dd <- as.matrix(data.table::dcast(as.data.table(ale.dat), a ~ b,
    value.var = ".ale"
  ))[, -1]
  expect_equal(unname(ale.original$f.values), unname(dd))

  # one categorical feature
  ale <- FeatureEffect$new(predictor1, feature = "c")
  ale.dat <- ale$results
  ale.original <- ALEPlot(X, predictor1, pred.fun = function(X.model, newdata) {
    X.model$predict(newdata)[, 1]
  }, J = 3)
  # equality of x values and ALE estimates
  expect_equal(as.character(ale.dat$c), ale.original$x.values)
  expect_equal(ale.dat$.value, ale.original$f.values)

  # another categorical feature
  ale <- FeatureEffect$new(predictor1.inter, feature = "c", method = "ale")
  ale.dat <- ale$results
  ale.original <- ALEPlot(X, predictor1.inter,
    pred.fun = function(X.model, newdata) {
      X.model$predict(newdata)[, 1]
    }, J = 3
  )
  # equality of x values and ALE estimates
  expect_equal(as.character(ale.dat$c), ale.original$x.values)
  expect_equal(ale.dat$.value, ale.original$f.values)

  # one numerical, one categorical feature
  ale <- FeatureEffect$new(predictor1.inter, feature = c("a", "c"))
  ale.dat <- ale$results
  ale.original <- ALEPlot(X, predictor1.inter,
    pred.fun = function(X.model, newdata) {
      X.model$predict(newdata)[, 1]
    }, J = c(3, 1)
  )
  # equality of x values and ALE estimates
  expect_equal(as.character(unique(ale.dat$c)), ale.original$x.values[[1]])
  expect_equal(unique(ale.dat$a), ale.original$x.values[[2]])
  res.iml <- unname(as.matrix(data.table::dcast(as.data.table(ale.dat),
    c ~ a,
    value.var = ".ale"
  )[, -1]))
  expect_equal(res.iml, unname(ale.original$f.values))
})


test_that("method='ale' works for 1D categorical", {
  ale <- FeatureEffect$new(predictor1, feature = "c")
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c(".type", ".value", "c"))
  expect_equal(nrow(dat), length(unique(X$c)))
  checkPlot(ale)

  expect_equal(ale$feature.name, "c")
  ale$set.feature(4)
  expect_equal(ale$feature.name, "d")
  expect_equal(colnames(ale$results), c(".type", ".value", "d"))
  # Centering
  expect_warning(ale$center(0))

  # multi class output
  ale <- FeatureEffect$new(predictor2,
    feature = "c", method = "ale",
    grid.size = 100
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(colnames(dat), c(".type", ".class", ".value", "c"))
  expect_equal(nrow(dat), length(unique(X$c)) * 2)
  checkPlot(ale)

  # ordered by the user
  X2 <- X
  X2$c <- ordered(X2$c)
  pred1.order <- Predictor$new(data = X2, y = y_fe, predict.fun = f)
  ale <- FeatureEffect$new(pred1.order, feature = "c")
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_false("data.table" %in% class(dat))
  expect_equal(colnames(dat), c(".type", ".value", "c"))
  expect_equal(nrow(dat), length(unique(X$c)))
  expect_equal(as.character(dat$c), c("a", "b", "c"))
  checkPlot(ale)
})


test_that("method='ale' works for 2D numerical x categorical", {
  ## two numerical features with 2 grid.sizes
  grid.size <- c(2)
  ale <- FeatureEffect$new(predictor1,
    feature = c("a", "c"),
    grid.size = grid.size
  )
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(
    ".type", ".ale", ".right", ".left",
    ".bottom", ".top", "a", "c"
  )))
  expect_equal(nrow(dat), (1 + grid.size[1]) * length(unique(X$c)))
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)
  ale2 <- FeatureEffect$new(predictor1,
    feature = c("c", "a"),
    grid.size = grid.size, method = "ale"
  )
  expect_equal(ale$results, ale2$results)

  ## two numerical features with 2D prediction
  grid.size <- c(3, 5)
  ale <- FeatureEffect$new(predictor2, feature = c("a", "c"), grid.size = grid.size)
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(
    ".type", ".class", ".ale", ".right",
    ".left", ".bottom", ".top", "a", "c"
  )))
  expect_equal(nrow(dat), (1 + grid.size[1]) * length(unique(X$c)) * 2)
  expect_equal(max(dat$a), 5)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)
  ale2 <- FeatureEffect$new(predictor2,
    feature = c("a", "c"),
    grid.size = grid.size, method = "ale"
  )
  expect_equal(ale$results, ale2$results)

  grpoints = c(2, 4, 1)
  ale <- FeatureEffect$new(predictor2, feature = c("a", "c"), grid.points = grpoints)
  dat <- ale$results
  expect_class(dat, "data.frame")
  expect_equal(sort(colnames(dat)), sort(c(
    ".type", ".class", ".ale", ".right",
    ".left", ".bottom", ".top", "a", "c"
  )))
  # nlevels * l(grpoints) * target_dim
  expect_equal(nrow(dat), 3 * 3 * 2)
  expect_equal(max(dat$a), 4)
  expect_equal(min(dat$a), 1)
  checkPlot(ale)
})


test_that("method='ale' in case of single category as well", {
  dat.reduced <- X[3:5, ]
  pred.reduced <- Predictor$new(data = dat.reduced, predict.fun = f)
  expect_error(
    FeatureEffect$new(pred.reduced, feature = 4, method = "ale"),
    "feature has only one unique value"
  )
})

test_that("FeatureEffect handles empty level", {
  set.seed(123)
  dat <- data.frame(y = 1:10, x = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
    levels = c(3, 2, 1)
  ), xx = rnorm(10))
  mod <- lm(y ~ x, data = dat)
  pred <- Predictor$new(mod, data = dat)
  fe <- FeatureEffect$new(pred, "x")
  dat2 <- data.frame(
    y = 1:10, x = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
    xx = rnorm(10)
  )
  mod2 <- lm(y ~ x, data = dat2)
  pred2 <- Predictor$new(mod2, data = dat2)
  fe2 <- FeatureEffect$new(pred, "x")
  expect_equal(fe, fe2)


  fe <- FeatureEffect$new(pred, "x", method = "pdp+ice")
  fe2 <- FeatureEffect$new(pred, "x", method = "pdp+ice")
  expect_equal(fe, fe2)
})
