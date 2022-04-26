get.feature.type <- function(feature.class) {
  assertCharacter(feature.class)

  feature.types <- c(
    "numeric" = "numerical",
    "integer" = "numerical",
    "character" = "categorical",
    "factor" = "categorical",
    "ordered" = "categorical"
  )

  stopifnot(all(feature.class %in% names(feature.types)))
  feature.types[feature.class]
}

#' returns TRUE if object has predict function
#' @importFrom utils methods
#' @param object The object to check.
has.predict <- function(object) {
  classes <- class(object)
  any(unlist(lapply(classes, function(x) {
    "predict" %in% attr(methods(class = x), "info")$generic
  })))
}

#' Turn class probabilities into class labels
#' @param prediction Prediction object.
probs.to.labels <- function(prediction) {
  checkmate::assert_data_frame(prediction)
  if (ncol(prediction) > 1) {
    levels <- colnames(prediction)
    prediction <- factor(colnames(prediction)[apply(prediction, 1, which.max)], levels = levels)
    data.frame(..class = prediction)
  } else {
    prediction
  }
}

#' Extract glmnet effects
#' @param betas glmnet$beta
#' @param best.index index k
#' @param x.recoded the recoded version of x
#' @param x.original the original x
#' Assuming that the first row is the x.interest
extract.glmnet.effects <- function(betas, best.index, x.recoded, x.original) {
  checkmate::assert_data_frame(x.recoded, nrows = 1)
  checkmate::assert_data_frame(x.original, nrows = 1)
  res <- data.frame(beta = betas[, best.index])
  res$x.recoded <- unlist(x.recoded[1, ])
  res$effect <- res$beta * res$x.recoded
  res$x.original <- unlist(lapply(x.original[1, ], as.character))
  res$feature <- colnames(x.recoded)
  res$feature.value <- sprintf("%s=%s", colnames(x.original), res$x.original)
  res
}




# binarizes categorical variables: TRUE if same level as x.interest, else FALSE
# used in lime
recode <- function(dat, x.interest) {
  checkmate::assert_data_frame(dat)
  checkmate::assert_data_frame(x.interest, nrows = 1, ncols = ncol(dat))
  checkmate::assert_true(all(colnames(dat) == colnames(x.interest)))

  types <- unlist(lapply(dat, function(feature) get.feature.type(class(feature))))
  new.colnames <- colnames(dat)
  cat.index <- types == "categorical"
  x.cat <- unlist(lapply(x.interest[cat.index], as.character))
  new.colnames[cat.index] <- sprintf("%s=%s", colnames(dat)[cat.index], x.cat)

  dat2 <- data.frame(lapply(1:ncol(dat), function(x) {
    if (types[x] == "categorical") {
      1 * (dat[[x]] == x.interest[[x]])
    } else {
      dat[[x]]
    }
  }))
  colnames(dat2) <- new.colnames
  dat2
}

# Return the paths of a ctree for each training data point
pathpred <- function(object, ...) {
  ## coerce to "party" object if necessary
  if (!inherits(object, "party")) object <- partykit::as.party(object)

  ## get rules for each node
  rls <- list.rules.party(object)

  ## get predicted node and select corresponding rule
  rules <- rls[as.character(predict(object, type = "node", ...))]
  rules <- gsub("&", "&\n", rules)

  return(rules)
}

is_label <- function(pred) {
  if (inherits(pred, c("character", "factor"))) {
    return(TRUE)
  }
  if (inherits(pred, c("data.frame", "matrix")) &&
    inherits(pred[, 1], "character") && ncol(pred) == 1) {
    return(TRUE)
  }
  FALSE
}

checkPrediction <- function(prediction, data) {
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(prediction,
    nrows = nrow(data), any.missing = FALSE,
    types = c("numeric", "integerish", "factor")
  )
}

# Currently not used
sanitizePrediction <- function(prediction) {
  if (inherits(prediction, c("numeric", "integer"))) {
    prediction <- data.frame(..prediction = prediction, fix.empty.names = FALSE)
  } else if (inherits(prediction, "matrix")) {
    cnames <- colnames(prediction)
    res <- data.frame(prediction)
    if (is.null(cnames)) {
      if (ncol(prediction) == 1) {
        colnames(prediction) <- "..prediction"
      } else {
        colnames(prediction) <- paste("..prediction", 1:ncol(prediction), sep = ".")
      }
    } else {
      colnames(prediction) <- cnames
    }
  } else {
    prediction <- data.frame(prediction)
  }
  prediction
}

cumsum_na <- function(values) {
  values[is.na(values)] <- 0
  cumsum(values)
}

get.grid <- function(dat, grid.size, anchor.value = NULL, type = "equidist") {
  assert_data_frame(dat, min.cols = 1)
  features <- colnames(dat)
  feature.type <- unlist(lapply(dat, function(x) {
    # old: get.feature.type(class(x))
    get.feature.type(class(x)[1])
  }))
  assert_character(features, min.len = 1, max.len = 2)
  assert_true(length(features) == length(feature.type))
  assert_numeric(grid.size, min.len = 1, max.len = length(features))
  if (length(features) == 1) {
    grid <- get.grid.1D(dat[[features[1]]],
      feature.type = feature.type[1], grid.size = grid.size[1], type = type
    )
    if (!is.null(anchor.value) && !(anchor.value %in% grid)) {
      grid <- sort(c(grid, anchor.value))
    }
  } else if (length(features) == 2) {
    if (length(grid.size == 1)) grid.size <- c(grid.size, grid.size)
    grid1 <- get.grid.1D(dat[[features[1]]], feature.type = feature.type[1], grid.size[1], type = type)
    grid2 <- get.grid.1D(dat[[features[2]]], feature.type = feature.type[2], grid.size[2], type = type)
    grid <- expand.grid(grid1, grid2)
  }
  grid.dt <- data.table(grid)
  colnames(grid.dt) <- features
  grid.dt
}


get.grid.1D <- function(feature, grid.size, feature.type = NULL, type = "equidist") {
  checkmate::assert_vector(feature, all.missing = FALSE, min.len = 2)
  checkmate::assert_choice(feature.type, c("numerical", "categorical"), null.ok = TRUE)
  checkmate::assert_numeric(grid.size)
  checkmate::assert_choice(type, c("equidist", "quantile"))

  if (is.null(feature.type)) feature.type <- get.feature.type(class(feature))

  if (feature.type == "numerical") {
    # remove NaN NA and inf
    feature <- feature[is.finite(feature)]
    if (length(feature) == 0) stop("Feature does not contain any finite values.")

    if (type == "equidist") {
      grid <- seq(
        from = min(feature),
        to = max(feature),
        length.out = grid.size
      )
    } else if (type == "quantile") {
      probs <- seq(from = 0, to = 1, length.out = grid.size)
      grid <- quantile(feature, probs = probs, names = FALSE, type = 1)
    }
  } else if (feature.type == "categorical") {
    grid <- unique(feature)
  }
  grid
}

#' @title Order levels of a categorical features
#'
#' @description
#' Orders the levels by their similarity in other features. Computes per feature
#' the distance, sums up all distances and does multi-dimensional scaling
#'
#' @details
#' Goal: Compute the distances between two categories.
#' Input: Instances from category 1 and 2
#'
#' 1. For all features, do (excluding the categorical feature for which we are computing the order):
#'  - If the feature is numerical: Take instances from category 1, calculate the
#'  empirical cumulative probability distribution function (ecdf) of the
#'  feature. The ecdf is a function that tells us for a given feature value, how
#'  many values are smaller. Do the same for category 2. The distance is the
#'  absolute maximum point-wise distance of the two ecdf. Practically, this
#'  value is high when the distribution from one category is strongly shifted
#'  far away from the other. This measure is also known as the
#'  Kolmogorov-Smirnov distance
#'  (\url{https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test}).
#'  - If the feature is categorical: Take instances from category 1 and
#'  calculate a table with the relative frequency of each category of the other
#'  feature. Do the same for instances from category 2. The distance is the sum
#'  of the absolute difference of both relative frequency tables.
#' 2. Sum up the distances over all features
#'
#' This algorithm we run for all pairs of categories.
#' Then we have a k times k matrix, when k is the number of categories, where
#' each entry is the distance between two categories. Still not enough to have a
#' single order, because, a (dis)similarity tells you the pair-wise distances,
#' but does not give you a one-dimensional ordering of the classes. To kind of
#' force this thing into a single dimension, we have to use a dimension
#' reduction trick called multi-dimensional scaling. This can be solved using
#' multi-dimensional scaling, which takes in a distance matrix and returns a
#' distance matrix with reduced dimension. In our case, we only want 1 dimension
#' left, so that we have a single ordering of the categories and can compute the
#' accumulated local effects. After reducing it to a single ordering, we are
#' done and can use this ordering to compute ALE. This is not the Holy Grail how
#' to order the factors, but one possibility.
#'
#' @param dat data.frame with the training data
#' @param feature.name the name of the categorical feature
#' @return the order of the levels (not levels itself)
order_levels <- function(dat, feature.name) {
  assert_data_frame(dat)
  assert_character(feature.name)
  assert_true(feature.name %in% names(dat))
  assert_factor(dat[, feature.name, with = FALSE][[1]])

  dat[, feature.name] <- droplevels(dat[, feature.name, with = FALSE])
  feature <- dat[, feature.name, with = FALSE][[1]]
  x.count <- as.numeric(table(dat[, feature.name, with = FALSE]))
  x.prob <- x.count / sum(x.count)
  K <- nlevels(dat[, feature.name, with = FALSE])

  dists <- lapply(setdiff(colnames(dat), feature.name), function(x) {
    feature.x <- dat[, x, with = FALSE][[1]]
    dists <- expand.grid(levels(feature), levels(feature))
    colnames(dists) <- c("from.level", "to.level")
    if (inherits(feature.x, "factor")) {
      A <- table(feature, feature.x) / x.count
      dists$dist <- rowSums(abs(A[dists[, "from.level"], ] - A[dists[, "to.level"], ])) / 2
    } else {
      quants <- quantile(feature.x, probs = seq(0, 1, length.out = 100), na.rm = TRUE, names = FALSE)
      ecdfs <- data.frame(lapply(levels(feature), function(lev) {
        x.ecdf <- ecdf(feature.x[feature == lev])(quants)
      }))
      colnames(ecdfs) <- levels(feature)
      ecdf.dists.all <- abs(ecdfs[, dists$from.level] - ecdfs[, dists$to.level])
      dists$dist <- apply(ecdf.dists.all, 2, max)
    }
    dists
  })

  dists.cumulated.long <- as.data.table(Reduce(function(d1, d2) {
    d1$dist <- d1$dist + d2$dist
    d1
  }, dists))
  dists.cumulated <- data.table::dcast(dists.cumulated.long, from.level ~ to.level, value.var = "dist")[, -1]
  diag(dists.cumulated) <- 0
  scaled <- cmdscale(dists.cumulated, k = 1)
  order(scaled)
}

# Get the layout for plotting multiple feature effects
get_layout <- function(n_features, nrows = NULL, ncols = NULL) {
  assert_integerish(n_features, lower = 1, null.ok = FALSE, any.missing = FALSE)
  assert_integerish(ncols, lower = 1, null.ok = TRUE, len = 1, any.missing = FALSE)
  assert_integerish(nrows, lower = 1, null.ok = TRUE, len = 1, all.missing = FALSE)

  # Get the size of the gtable
  if (is.null(nrows) & is.null(ncols)) {
    ncols <- 3
    nrows <- ceiling(n_features / ncols)
  } else {
    if (is.null(nrows)) {
      nrows <- ceiling(n_features / ncols)
    }
    if (is.null(ncols)) {
      ncols <- ceiling(n_features / nrows)
    }
  }
  list("nrows" = nrows, "ncols" = ncols)
}

# move positions of columns in a data.frame
# https://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe?noredirect=1&lq=1

moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp) - 1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    }
  )
  x
}
