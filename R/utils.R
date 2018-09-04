get.feature.type = function(feature.class) {
  assertCharacter(feature.class)
  
  feature.types = c(
    'numeric'='numerical', 
    'integer'='numerical', 
    'character'='categorical',
    'factor'='categorical',
    'ordered'='categorical'
  )
  
  stopifnot(all(feature.class %in% names(feature.types)))
  feature.types[feature.class]
}



# returns TRUE if object has predict function
#' @importFrom utils methods
has.predict = function(object) {
  classes = class(object)
  any(unlist(lapply(classes, function(x) {
    'predict' %in% attr(methods(class = x), 'info')$generic
  })))
}


# Turn class probabilities into class labels
probs.to.labels = function(prediction) {
  checkmate::assert_data_frame(prediction)
  if (ncol(prediction) > 1) {
    levels = colnames(prediction)
    prediction = factor(colnames(prediction)[apply(prediction, 1, which.max)], levels = levels)
    data.frame(..class = prediction)
  } else {
    prediction
  }
}

# Extract glmnet effects
# @param betas glmnet$beta
# @param best.index index k 
# @param x.recoded the recoded version of x
# @param x.original the original x
# Assuming that the first row is the x.interest
extract.glmnet.effects = function(betas, best.index, x.recoded, x.original) {
  checkmate::assert_data_frame(x.recoded, nrows=1)
  checkmate::assert_data_frame(x.original, nrows=1)
  res = data.frame(beta = betas[, best.index])
  res$x.recoded = unlist(x.recoded[1,])
  res$effect = res$beta * res$x.recoded
  res$x.original = unlist(lapply(x.original[1,], as.character))
  res$feature = colnames(x.recoded)
  res$feature.value = sprintf('%s=%s', colnames(x.original), res$x.original)
  res
}




# binarizes categorical variables: TRUE if same level as x.interest, else FALSE
# used in lime
recode = function(dat, x.interest) {
  checkmate::assert_data_frame(dat)
  checkmate::assert_data_frame(x.interest, nrows = 1, ncols = ncol(dat))
  checkmate::assert_true(all(colnames(dat) == colnames(x.interest)))
  
  types = unlist(lapply(dat, function(feature) get.feature.type(class(feature))))
  new.colnames = colnames(dat)
  cat.index = types == 'categorical'
  x.cat = unlist(lapply(x.interest[cat.index], as.character))
  new.colnames[cat.index] = sprintf('%s=%s', colnames(dat)[cat.index], x.cat)
  
  dat2 = data.frame(lapply(1:ncol(dat), function(x) {
    if (types[x] == 'categorical') {
      1 * (dat[[x]] == x.interest[[x]])
    } else {
      dat[[x]]
    }
  }))
  colnames(dat2) = new.colnames
  dat2
}

# Return the paths of a ctree for each training data point
pathpred = function(object, ...) {
  ## coerce to "party" object if necessary
  if (!inherits(object, "party")) object = partykit::as.party(object)
  
  ## get rules for each node
  rls = list.rules.party(object)
  
  ## get predicted node and select corresponding rule
  rules = rls[as.character(predict(object, type = "node", ...))]
  rules = gsub("&", "&\n", rules)
  
  return(rules)
}


is.label.output = function(pred) {
  if (inherits(pred, c("character", "factor"))) return(TRUE)
  if (inherits(pred, c("data.frame", "matrix")) && 
      inherits(pred[,1], "character") && ncol(pred) == 1) {
    return(TRUE)
  }
  FALSE
}


checkPrediction = function(prediction, data) {
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(prediction, nrows = nrow(data), any.missing = FALSE, 
    types = c("numeric", "integerish", "factor"))
}

# Currently not used
sanitizePrediction = function(prediction) {
  if (inherits(prediction, c("numeric", "integer"))) {
    prediction = data.frame(..prediction = prediction, fix.empty.names = FALSE)
  } else if (inherits(prediction, "matrix")) {
    cnames = colnames(prediction)
    res = data.frame(prediction)
    if (is.null(cnames)) {
      if (ncol(prediction) == 1) {
        colnames(prediction) = "..prediction"
      } else {
        colnames(prediction) = paste("..prediction", 1:ncol(prediction), sep = ".")
      }
    } else {
      colnames(prediction) = cnames
    }
  } else {
    prediction = data.frame(prediction)
  }
  prediction
}


cumsum_na = function(values) {
  values[is.na(values)] = 0
  cumsum(values)
}


get.grid = function(dat, grid.size, anchor.value = NULL, type = "equidist") {
  assert_data_frame(dat, min.cols = 1)
  features = colnames(dat)
  feature.type = unlist(lapply(dat, function(x){get.feature.type(class(x))}))
  assert_character(features, min.len = 1, max.len = 2)
  assert_true(length(features) == length(feature.type))
  assert_numeric(grid.size, min.len = 1, max.len = length(features))
  if (length(features) == 1) {
    grid = get.grid.1D(dat[[features[1]]], 
      feature.type = feature.type[1], grid.size = grid.size[1], type  = type)
    if (!is.null(anchor.value) && !(anchor.value %in% grid)) {
      grid = sort(c(grid, anchor.value))
    }
  } else if (length(features) == 2) {
    if(length(grid.size == 1)) grid.size = c(grid.size, grid.size)
    grid1 = get.grid.1D(dat[[features[1]]], feature.type = feature.type[1], grid.size[1], type = type)
    grid2 = get.grid.1D(dat[[features[2]]], feature.type = feature.type[2], grid.size[2], type = type)
    grid = expand.grid(grid1, grid2)
  } 
  grid.dt = data.table(grid)
  colnames(grid.dt) = features
  grid.dt
}


get.grid.1D = function(feature, grid.size,  feature.type = NULL, type = "equidist") {
  checkmate::assert_vector(feature, all.missing = FALSE, min.len = 2)
  checkmate::assert_choice(feature.type, c("numerical", "categorical"), null.ok = TRUE)
  checkmate::assert_numeric(grid.size)
  checkmate::assert_choice(type, c("equidist", "quantile"))
  
  if(is.null(feature.type)) feature.type = get.feature.type(class(feature))
  
  if (feature.type == "numerical") {
    # remove NaN NA and inf
    feature = feature[is.finite(feature)]
    if (length(feature) == 0) stop("Feature does not contain any finite values.")
    
    if(type == "equidist") {
      grid = seq(from = min(feature), 
        to = max(feature), 
        length.out = grid.size)
    } else if(type == "quantile") {
      probs = seq(from = 0, to = 1, length.out = grid.size)
      grid = quantile(feature, probs = probs, names = FALSE, type = 1)
    }
  } else if (feature.type == "categorical") {
    grid = unique(feature)
  }
  grid
}



#' Order levels of a categorical features
#' 
#' Orders the levels by their similarity in other features.
#' Computes per feature the distance, sums up all distances and does multi-dimensional scaling
#' @param dat data.frame with the training data
#' @param feature.name the name of the categorical feature
#' @return the order of the levels (not levels itself)
order_levels = function(dat, feature.name) {
  assert_data_frame(dat)
  assert_character(feature.name)
  assert_true(feature.name %in%  names(dat))
  assert_factor(dat[, feature.name, with = FALSE][[1]])
  
  dat[, feature.name] =  droplevels(dat[, feature.name, with = FALSE])
  feature = dat[, feature.name, with = FALSE][[1]]
  x.count = as.numeric(table(dat[, feature.name, with = FALSE]))
  x.prob = x.count/sum(x.count)
  K = nlevels( dat[, feature.name, with = FALSE] )
  
  dists = lapply(setdiff(colnames(dat), feature.name), function(x){
    feature.x = dat[, x, with = FALSE][[1]]
    dists = expand.grid(levels(feature), levels(feature))
    colnames(dists) = c("from.level", "to.level")
    if(class(feature.x) == "factor") {
      A = table(feature, feature.x) / x.count
      dists$dist = rowSums(abs(A[dists[,"from.level"],] - A[dists[,"to.level"],]))/2
    } else {
      quants = quantile(feature.x, probs = seq(0, 1, length.out = 100), na.rm = TRUE, names = FALSE)
      ecdfs = data.frame(lapply(levels(feature), function(lev){
        x.ecdf = ecdf(feature.x[feature == lev])(quants)
      }))
      colnames(ecdfs) = levels(feature)
      ecdf.dists.all = abs(ecdfs[,dists$from.level] - ecdfs[, dists$to.level])
      dists$dist = apply(ecdf.dists.all, 2, max)
    }
    dists
  })
  
  dists.cumulated.long = Reduce(function(d1, d2) {d1$dist = d1$dist + d2$dist; d1}, dists)
  dists.cumulated = dcast(dists.cumulated.long, from.level ~ to.level, value.var = "dist")[,-1]
  diag(dists.cumulated) = 0
  scaled = cmdscale(dists.cumulated, k = 1)
  order(scaled)
}

