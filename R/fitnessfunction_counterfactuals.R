fitness.fun = function(x, x.interest, target, predictor, range, param.set) {
  
  #assert_dataframe(x, min.row = 1, max.row = 1, any.missing = FALSE)
  assertDataFrame(x.interest, min.row = 1, max.row = 1, any.missing = FALSE)
  assertNumeric(target, min.len = 1, max.len = 2)
  assertNumeric(range, lower = 0, finite = TRUE, any.missing = TRUE, 
    len = ncol(x))
  if (!all.equal(names(x.interest), names(range))) {
    stop("range for gower distance needs same order and features as 
      candidates and original x")
  }
  
  # use.orig not needed
  # transform to data.frame
  x$use.orig = NULL
  #x = transformToOrig(x, x.interest, delete.use.orig = TRUE)
  x = as.data.frame(x, stringsAsFactors = FALSE)
  x.factor = x
  x[,sapply(x, is.factor)] = as.character(x[,sapply(x, is.factor)])
  
  if(!all.equal(names(x), names(x.interest))) {
    stop("original x and candidate have different features, check ordering")
  }
  equal.type = all(mapply(x, x.interest,
    FUN = function(x1, x2) {class(x1) == class(x2)}))
  if (!equal.type) {
    stop("original x and candidate need same feature types")
  }
  
  # Fitness values
  pred = predictor$predict(newdata = x.factor)[[1]]
  f1 = ifelse(length(target) == 2 & (pred > target[1]) & (pred < target[2]), 
    0, min(abs(pred - target)))
  f2 = StatMatch::gower.dist(data.x = x.interest, data.y = x, rngs = range)[1,]
  f3 = rowSums(x != x.interest)[[1]]
  return(c(f1, f2, f3))
}
