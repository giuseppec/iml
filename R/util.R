get.feature.type = function(feature.class){
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
has.predict = function(object){
  classes = class(object)
  any(unlist(lapply(classes, function(x){
    'predict' %in% attr(methods(class = x), 'info')$generic
  })))
}

#test = randomForest(Species ~ ., data = iris)


#' Turn class probabilities into class labels
probs.to.labels = function(prediction){
  checkmate::assert_data_frame(prediction)
  if(ncol(prediction) > 1){
    prediction = colnames(prediction)[apply(prediction, 1, which.max)]
    data.frame(..class = prediction)
  } else {
    prediction
  }
}

#' Extract glmnet effects
#' @param betas glment$beta
#' Assuming that the first row is the x.interest
extract.glment.effects = function(betas, best.index, model.mat){
  res = data.frame(beta = betas[, best.index])
  res$x = model.mat[1,]
  res$effect = res$beta * res$x
  res$feature = colnames(model.mat)
  res$feature.value = sprintf('%s=%s', res$feature, res$x)
  res
}




# binarizes categorical variables: TRUE if same level as x.interest, else FALSE
# TODO: Keep level names for factors
# used in lime
recode.data = function(dat, x.interest){
  checkmate::assert_data_frame(dat)
  checkmate::assert_data_frame(x.interest, nrows = 1, ncols = ncol(dat))
  checkmate::assert_true(all(colnames(dat) == colnames(x.interest)))
  
  types = unlist(lapply(dat, function(feature) get.feature.type(class(feature))))
  new.colnames = colnames(dat)
  cat.index = types == 'categorical'
  x.cat = unlist(lapply(x.interest[cat.index], as.character))
  new.colnames[cat.index] = sprintf('%s=%s', colnames(dat)[cat.index], x.cat)

  dat2 = data.frame(lapply(1:ncol(dat), function(x){
    if(types[x] == 'categorical'){
      1 * (dat[[x]] == x.interest[[x]])
    } else {
      dat[[x]]
    }
  }))
  colnames(dat2) = new.colnames
  dat2
}