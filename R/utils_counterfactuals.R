###### Utils Counterfactual #####

#' Extract information about each feature from input dataset 
#' 
#' Results in a list as input for ParamHelpers::makeParamSet.
#' 
#' @section Arguments: 
#' \describe{
#' \item{input.data:}{(data.frame)\cr Training data}
#' \item{lower: }{numeric\cr Vector of minimal values for numeric features. 
#' If NULL lower is extracted from input data specified in field 'data' of 
#' 'predictor'.}
#' \item{upper: }{numeric\cr Vector of maximal values for numeric features.
#' If NULL upper is extracted from input data specified in field 'data' of 
#' 'predictor'.}
#' }
#' @return (list) 
make_paramlist = function(input.data, lower = NULL, upper = NULL) {
  checkmate::assert_data_frame(input.data)
  checkmate::assert_numeric(lower, null.ok = TRUE)
  checkmate::assert_numeric(upper, null.ok = TRUE)
  
  assert_true(all(names(lower) %in% names(input.data)))
  assert_true(all(names(upper) %in% names(input.data)))
  
  ncol = ncol(input.data)
  
  l = lapply(colnames(input.data), function(colnam) { 
    col = input.data[[colnam]]
    l = ifelse(colnam %in% names(lower), 
      lower[[colnam]], tryCatch(min(col, na.rm = TRUE), error = function(err) NA))
    u = ifelse(colnam %in% names(upper), 
      upper[[colnam]], tryCatch(max(col, na.rm = TRUE), error = function(err) NA))
    
    if (is.double(col)) {
      ParamHelpers::makeNumericParam(colnam, lower = l, upper = u) 
    } 
    
    else if (is.integer(col)) {
      ParamHelpers::makeIntegerParam(colnam, lower = l, upper = u)
    }
    
    else { 
      if (is.character(col)) {
        values = unique(col)
      } else {
        values = char_to_factor(levels(col))
      }
      ParamHelpers::makeDiscreteParam(colnam, values = values) 
    } 
  })
  l[[length(l)+1]] = ParamHelpers::makeLogicalVectorParam("use.orig", len = ncol)
  return(l)
}

#' Create a list with named vectors of standard deviation 
#' 
#' Will be grouped by the feature type extracted from param.set.
#' @section Arguments: 
#' \describe{
#' \item{sdev: }{(numeric)\cr Vector of standard deviations.}
#' \item{param.set: }{(ParamSet)\cr Paramset to create elements of list, 
#' one for digit features and one for integer features.}
#' }
#' @return (list)
sdev_to_list = function(sdev, param.set) {
  checkmate::assert_numeric(sdev, any.missing = FALSE)
  checkmate::assert_class(param.set, "ParamSet")
  checkmate::assert_true(all(names(sdev) %in% ParamHelpers::getParamIds(param.set)))
  param.ids = ParamHelpers::getParamIds(param.set)
  paramtypes = gsub("vector$", "", ParamHelpers::getParamTypes(param.set))
  needed_type = c("numeric", "integer")
  typegroups = sapply(needed_type, function(type) {
    sdev[param.ids[paramtypes == type]]
  }, simplify = FALSE)
  return(typegroups)
}

#' Get difference between two feature vectors 
#' 
#' Difference is 0 if two characters or factors are equal. 
#' Otherwise the class of x is displayed. 
#' @section Arguments: 
#' \describe{
#' \item{x: }{(data.frame)\cr Data frame with one row.}
#' \item{x.interest: }{(data.frame)\cr Data frame with one row.}
#' }
#' @return (data.frame)
get_diff = function(x, x.interest) {
  assertDataFrame(x, ncol = ncol(x.interest))
  assertDataFrame(x.interest, nrow = 1)
  
  diff = data.frame(matrix(data = NA, nrow = nrow(x), ncol = length(x.interest)))
  names(diff) = names(x.interest)
  
  x.interest = x.interest[rep(row.names(x.interest), nrow(x)), ]
  
  for (i in 1:ncol(x)) {
    if (class(x[,i]) == "character"|
        class(x[,i]) == "factor") {
      p.val = as.character(x[,i])
      x.val = as.character(x.interest[,i])
      diff[,i] = ifelse(p.val != x.val, p.val, "0")
    } else {
      diff[,i] = x[,i] - x.interest[,i]
    }
  }
  return(diff)
}

#' Transform features to x.interest 
#' 
#' Replace features of x by value of x.interest 
#' where use.orig is set to TRUE. 
#' 
#' @section Arguments: 
#' \describe{
#' \item{x: }{(list)\cr List of features, must have list element 
#' use.origin.}
#' \item{x.interest: }{(data.frame)\cr Data frame with one row.}
#' \item{delete.use.orig: }{(logical(1))\cr Whether to 
#' delete list element use.orig of x.}
#' \item{fixed.features: }{(character)\cr 
#' Indicate fixed features by feature name.}
#' \item{max.changed: }{numeric(1)\cr Number indicating 
#' how many feature are allowed to maximially differ from the original data point.}
#' }
#' 
#' @return (list)
transform_to_orig = function(x, x.interest, delete.use.orig = FALSE, 
  fixed.features = NULL, max.changed = NULL) {
  checkmate::assert_list(x, len = ncol(x.interest) + 1)
  checkmate::assert_data_frame(x.interest, any.missing = FALSE, nrows = 1)
  checkmate::assert(
    check_character(fixed.features, null.ok = TRUE),
    check_true(fixed.features %in% names(x))
  )
  checkmate::assert_integerish(max.changed, null.ok = TRUE)
  types = lapply(x[names(x)!="use.orig"], class)
  
  if (!is.null(fixed.features)) {
    pos = which(names(x) %in% fixed.features)
    x$use.orig[pos] = TRUE
  }
  
  use.orig = x$use.orig 
  if (!is.null(max.changed)) {
    n.changed = sum(!use.orig)
    if (n.changed > max.changed) {
      n = n.changed - max.changed 
      mut.idx = sample(which(!use.orig), n)
      use.orig[mut.idx] = TRUE
    }
  }
  x$use.orig = NULL 
  x[use.orig] = x.interest[use.orig]
  types.after.trans = lapply(x, class)
  if(length(setdiff(types, types.after.trans)) > 0) {
    stop("setting values to x.interest values introduced a type shift")
  }
  if (!delete.use.orig) {
    x$use.orig = use.orig
  }
  return(x)
}

#' Transmit levels of factor variable to parameter set
#'
#' @section Arguments:  
#' \describe{
#' \item{levels: }{(character)\cr Character vector of feature class labels.}
#' }
char_to_factor= function(levels){
  sapply(as.character(levels), function(x)
    factor(x, levels=levels),
    simplify = FALSE)
}

#' Round numeric elements in data frame 
#' @section Arguments: 
#' \describe{
#' \item{df: }{(data.frame)\cr Data frame in which numeric elments will be rounded.}
#' \item{digits: }{integer(1)\cr Number of decimal places used. Default is 3.}
#' }
#' @return (data.frame) with rounded elements
round_df = function(df, digits = 3) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] = round(df[,nums], digits = digits)
  return(df)
}

#' Calculate ice curve variance over all features
#' 
#' @section Arguments: 
#' \describe{
#' \item{x.interest: }{(data.frame)\cr Data point, for which ice curves 
#' should be calculated.}
#' \item{predictor: }{(Predictor)\cr Object, that holds the prediction model and dataset.}
#' \item{param.set: }{(ParamSet)\cr Parameter set of features in dataset.}
#' }
#' @return (numeric) Vector with standard deviations for each feature.
get_ICE_var = function(x.interest, predictor, param.set) {
  min.max = as.data.frame(rbind(getLower(param.set), 
    getUpper(param.set)))
  val = getValues(param.set)
  val$use.orig = NULL
  val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
  
  values = c(as.list(min.max), val.l)
  
  sd.eff = sapply(names(x.interest), function(x){
    get_ice_curve(x.interest, x, predictor, values = values[[x]])
  })
  
  return(sd.eff)
  
}

# Calculate ice curve variance per feature
get_ice_curve = function(instance, feature, predictor, values, 
  grid.size = 20) {
  
  # make grid of one feature
  grid = iml:::get.grid.1D(feature = values, grid.size = grid.size, 
    type = "equidist")
  grid.size = length(grid)
  grid = as.data.frame(grid)
  colnames(grid) = feature
  
  instance = instance[, !names(instance) %in% feature]
  instance.df = instance[rep(row.names(instance), grid.size), ]
  
  grid.df = cbind.data.frame(instance.df, grid)
  
  # predict outcomes
  #pred = predict(predictor, newdata = grid.df)$data$response
  pred = predictor$predict(newdata = grid.df)[[1]]
  
  # calculate sd 
  return(sd(pred))
  
}

#' Algorithm to remove solutions of final set 
#' 
#' @section Arguments: 
#' \describe{
#' \item{fitness: }{(data.frame)\cr Data frame of fitness values. Each column 
#' represents one objective.}
#' \item{pareto.set: }{(data.frame)\cr Corresponding Pareto set to 
#' fitness.}
#' \item{nr.solutions: }{(numeric(1))\cr Number of solutions that should
#' be returned.} 
#' }
get_diverse_solutions = function(fitness, pareto.set, nr.solutions) {
  
  assert_data_frame(fitness, any.missing = FALSE, ncols = 3, nrows = nrow(pareto.set))
  assert_data_frame(pareto.set, any.missing = FALSE)
  assert_number(nr.solutions)
  
  n = nrow(pareto.set)
  max = apply(fitness, 2, max)
  min = apply(fitness, 2, min)
  g.dist = StatMatch::gower.dist(pareto.set, KR.corr = FALSE)
  
  dds = numeric(n)
  ods = numeric(n)
  
  for (i in seq_len(ncol(fitness)-1)) {
    
    # get the order of the points when sorted according to the i-th objective
    ord = order(fitness[,3], fitness[,i])
    min.changed = c(TRUE, diff(fitness[ord, 3]) > 0)
    max.changed = rev(c(TRUE, diff(rev(fitness[ord, 3])) < 0))
    ind.inf = min.changed|max.changed
    
    # set the extreme values to Inf for each nr.features.changed (objective 3)
    dds[ord[ind.inf]] = Inf
    ods[ord[ind.inf]] = Inf
    
    # update the remaining crowding numbers
    if (n > 2L) {
      for (j in 2:(n - 1L)) {
        
        if (max[i] - min[i] != 0) {
          ods[ord[j]] = ods[ord[j]] + 
            (abs(fitness[ord[j + 1L], i] - fitness[ord[j - 1L], i])/(max[i]-min[i]))
        }
        dds[ord[j]] = dds[ord[j]] +
          g.dist[ord[j], ord[j-1]] +
          g.dist[ord[j], ord[j+1]]
        
      }
    }
  }
  cds = rank(ods) + rank(dds)
  cds = jitter(cds, factor = 1)
  idx = order(cds, decreasing = TRUE)[1:nr.solutions]
  return(idx)
}


#' Calculate diversity
#' 
#' The diversity is equal to pair-wise mean of Gower distances.
#' 
#' @section Arguments: 
#' \describe{
#' \item{df: }{(data.frame)\cr Pareto set.}
#' \item{range: }{(numeric)\cr Vector of ranges for numeric features. 
#' Must have same ordering as columns in df.}
#' }
#' @return (numeric(1))
compute_diversity = function(df, range) {
  dis = StatMatch::gower.dist(data.x = df, 
    rngs = range, KR.corr = FALSE)
  single.dist = dis[lower.tri(dis)]
  mean.dist = mean(single.dist)
  return(mean.dist)
}




