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
probs.to.labels = function(prediction, levels = NULL) {
  checkmate::assert_data_frame(prediction)
  if (ncol(prediction) > 1) {
    prediction = colnames(prediction)[apply(prediction, 1, which.max)]
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
# TODO: Keep level names for factors
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
  if (inherits(pred, c("data.frame", "matrix")) && inherits(pred[,1], "character")) {
    return(TRUE)
  }
  FALSE
}


## TODO:document
## TODO: Write test

get.1D.grid = function(feature, feature.type, grid.size) {
  checkmate::assert_vector(feature)
  if (feature.type == "numerical") {
    grid = seq(from = min(feature), 
      to = max(feature), 
      length.out = grid.size)
  } else if (feature.type == "categorical") {
    grid = unique(feature)
  }
}
