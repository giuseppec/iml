###### Utils Counterfactual #####

#' Extract information about each features from input dataset 
#' to create a parameter list as input for ParamHelpers::makeParamSet
#' 
#' @section Arguments: 
#' \describe{
#' \item{input.data: }{(data.frame)\cr Training data}
#' ....}
#' 
makeParamlist <- function(input.data, lower = NULL, upper = NULL, integers = NULL) {
  
  checkmate::assert_data_frame(input.data)
  checkmate::assert_numeric(lower, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(upper, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(integers, null.ok = TRUE, any.missing = FALSE)
  
  ncol = ncol(input.data)
  
  l = lapply(colnames(input.data), function(colnam) { 
    if (colnam %in% integers) {
      checkmate::assert_numeric(input.data[[colnam]])
      col <- as.integer(input.data[[colnam]])
    }
    else {
      col <- input.data[[colnam]]
    }
    l = ifelse(colnam %in% names(lower), 
      lower[[colnam]], tryCatch(min(col), error = function(err) NA))
    u = ifelse(colnam %in% names(upper), 
      upper[[colnam]], tryCatch(max(col), error = function(err) NA))
    
    if (is.double(col)) {
      ParamHelpers::makeNumericParam(colnam, lower = l, upper = u) 
    } 
    
    else if (is.integer(col)) {
      ParamHelpers::makeIntegerParam(colnam, lower = l, upper = u)
    }
    
    else { 
      ParamHelpers::makeDiscreteParam(colnam, values = fctvals(unique(col))) 
    } 
  })
  l[[length(l)+1]] = ParamHelpers::makeLogicalVectorParam("use.orig", len = ncol)
  return(l)
}

#' Create a list with named vectors of standard deviation 
#' grouped by the feature type extracted from param.set
#' @section Arguments: 
#' \describe{
#' \item{sdev: }{(numeric)\cr Vector of standard deviations}
#' ....}

sdev.to.namedlist = function(sdev, param.set) {
  paramtypes <- gsub("vector$", "", ParamHelpers::getParamTypes(param.set))
  param.ids <- ParamHelpers::getParamIds(param.set)
  needed_type = c("numeric", "integer")
  typegroups <- sapply(needed_type, function(type) {
    sdev[param.ids[paramtypes == type]]
  }, simplify = FALSE)
  return(typegroups)
}



list.to.df = function(x) {
  x = lapply(x, function(e) {
    e$use.orig = NULL 
    return(e)
  })
  
  df = lapply(x, function(e) {as.data.frame(e, stringsAsFactors = FALSE)})
  df = do.call(rbind, df)
  return(df)
}


getDiff = function(pareto.set, x.interest, digits = NULL) {
  assertDataFrame(pareto.set, ncol = ncol(x.interest))
  assertDataFrame(x.interest, nrow = 1)
  
  diff = data.frame(matrix(data = NA, nrow = nrow(pareto.set), ncol = length(x.interest)))
  names(diff) = names(x.interest)
  
  x.interest = x.interest[rep(row.names(x.interest), nrow(pareto.set)), ]
  
  for (i in 1:ncol(pareto.set)) {
    if (class(pareto.set[,i]) == "character"|
        class(pareto.set[,i]) == "factor") {
      p.val = as.character(pareto.set[,i])
      x.val = as.character(x.interest[,i])
      diff[,i] = ifelse(p.val != x.val, p.val, "0")
    } else {
      diff[,i] = pareto.set[,i] - x.interest[,i]
    }
  }
  if (!is.null(digits)) {
    diff = round(diff, digits)
  }
  return(diff)
}

# Transform features of solution candidates to value of x.interest 
# where use.orig is set to TRUE
transform.to.orig = function(x, x.interest, delete.use.orig = FALSE, 
  fixed.features = NULL) {
  types = lapply(x[names(x)!="use.orig"], class)
  
  if (!is.null(fixed.features)) {
    pos = which(names(x) %in% fixed.features)
    x$use.orig[pos] = TRUE
  }
  
  use.orig <- x$use.orig 
  x$use.orig <- NULL 
  x[use.orig] <- x.interest[use.orig]
  types.after.trans = lapply(x, class)
  if(length(setdiff(types, types.after.trans)) > 0) {
    stop("setting values to x.interest values introduced a type shift")
  }
  if (!delete.use.orig) {
    x$use.orig = use.orig
  }
  return(x)
}

# Transmit levels of factor variable to parameter set
fctvals <- function(lvls){
  sapply(as.character(lvls), function(x)
    factor(x, levels=lvls),
    simplify = FALSE)
}


# Remove columns with 0 entries 
remove.zero.cols <- function(df, columns) {
  rem.vec <- NULL
  for(i in 1:ncol(df)){
    if (names(df[i]) %in% columns) {
      sum.col = sum(abs(df[,i]))
      if(sum.col == 0) {
        rem.vec[i] <- names(df)[i]
      }
    }
  }
  if (!is.null(rem.vec)) {
    features.to.remove <- rem.vec[!is.na(rem.vec)]
    rem.ind <- which(names(df) %in% features.to.remove)
    df.new = c()
    df.new = df[,-rem.ind]
    names(df.new) = names(df)[!names(df) %in% features.to.remove]
    return(df.new)
  }
  return(df)
}



getDiverseSolutions = function(fitness, pareto.set, range) {
  checkmate::assert_data_frame(fitness, any.missing = FALSE, nrows = nrow(pareto.set))
  checkmate::assertDataFrame(pareto.set, any.missing = FALSE, nrows = nrow(fitness))
  
  if (!all(rownames(pareto.set) == rownames(fitness))) {
    stop("rownames of pareto set and rownames of fitness matrix needs to be identical")
  }
  
  n = nrow(fitness)
  
  # get the order of the points when sorted according to the i-th objective
  # 1 is the candidate with the smallest objective function value
  ord = order(fitness[,1], fitness[,3])
  
  gow.dist = StatMatch::gower.dist(pareto.set, rngs = range)
  gow.dist[gow.dist == 0] = NA
  
  obs.div = 1:nrow(pareto.set)
  similar.obs = c()
  
  for (j in 2:n) {
    id = order(gow.dist[ord[j],])[1:(j-1-length(similar.obs))]
    
    if (any(id %in% ord[1:(j-1)])) {
      gow.dist[ord[j],] = NA
      gow.dist[,ord[j]] = NA
      similar.obs = c(similar.obs, ord[j])
    }
  }
  return(obs.div[-similar.obs])
}



#' Calculate Spacing 
spacing = function(solutions, metric = "euclidean", ranges = NULL) {
  checkmate::assert_choice(metric, choices = c("euclidean", "manhattan", "gower"))
  if (metric == "euclidean"| metric == "manhattan") {
    dist = as.matrix(stats::dist(solutions, method = metric, 
      diag = FALSE, upper = TRUE))
  }
  else {
    assertVector(ranges, all.missing = FALSE)
    dist = StatMatch::gower.dist(solutions, rngs = ranges)
  }
  dist[dist == 0] = NA
  min = apply(dist, MARGIN = 1, function(x) min(x, na.rm = TRUE))
  return(sd(min))
}



