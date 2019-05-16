###### Utils Counterfactual #####

#' Extract information about each features from input dataset 
#' to create a parameter list as input for ParamHelpers::makeParamSet
#' 
#' @section Arguments: 
#' \describe{
#' \item{input.data: }{(data.frame)\cr Training data}
#' ....}
#' 
make_paramlist <- function(input.data, lower = NULL, upper = NULL, integers = NULL) {
  
  checkmate::assert_data_frame(input.data)
  checkmate::assert_numeric(lower, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(upper, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(integers, null.ok = TRUE, any.missing = FALSE)
  
  assert_true(all(names(lower) %in% names(input.data)))
  assert_true(all(names(upper) %in% names(input.data)))
  
  ncol = ncol(input.data)
  
  l = lapply(colnames(input.data), function(colnam) { 
    col <- input.data[[colnam]]
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
#' grouped by the feature type extracted from param.set
#' @section Arguments: 
#' \describe{
#' \item{sdev: }{(numeric)\cr Vector of standard deviations}
#' ....}

sdev_to_list = function(sdev, param.set) {
  checkmate::assert_numeric(sdev, any.missing = FALSE)
  checkmate::assert_class(param.set, "ParamSet")
  checkmate::assert_true(all(names(sdev) %in% ParamHelpers::getParamIds(param.set)))
  param.ids <- ParamHelpers::getParamIds(param.set)
  paramtypes <- gsub("vector$", "", ParamHelpers::getParamTypes(param.set))
  needed_type = c("numeric", "integer")
  typegroups <- sapply(needed_type, function(type) {
    sdev[param.ids[paramtypes == type]]
  }, simplify = FALSE)
  return(typegroups)
}


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

# Transform features of solution candidates to value of x.interest 
# where use.orig is set to TRUE
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
  
  use.orig <- x$use.orig 
  if (!is.null(max.changed)) {
    n.changed = sum(!use.orig)
    if (n.changed > max.changed) {
      n = n.changed - max.changed 
      mut.idx = sample(which(!use.orig), n)
      use.orig[mut.idx] = TRUE
    }
  }
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
char_to_factor<- function(levels){
  sapply(as.character(levels), function(x)
    factor(x, levels=levels),
    simplify = FALSE)
}


round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  return(df)
}

# Calculate ice curve variance over all features
get_ICE_var = function(x.interest, mod, param.set) {
  min.max = as.data.frame(rbind(getLower(param.set), 
    getUpper(param.set)))
  val = getValues(param.set)
  val$use.orig = NULL
  val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
  
  values = c(as.list(min.max), val.l)
  
  sd.eff = sapply(names(x.interest), function(x){
    get_ice_curve(x.interest, x, mod, values = values[[x]])
  })
  
  return(sd.eff)
  
}
# Ice curve variance per feature
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

# new version separated by each front
get_diverse_solutions = function(fitness, pareto.set, range, nr.solutions) {
  
  n = nrow(pareto.set)
  max = apply(fitness, 2, max)
  min = apply(fitness, 2, min)
  g.dist = StatMatch::gower.dist(pareto.set, rngs = range)
  
  dds = numeric(n)
  ods = numeric(n)
  
  for (i in seq_len(ncol(fitness)-1)) {
    
    # get the order of the points when sorted according to the i-th objective
    ord = order(fitness[,3], fitness[,i])
    min.changed = c(TRUE, diff(fitness[ord, 3]) > 0)
    max.changed = rev(c(TRUE, diff(rev(fitness[ord, 3])) < 0))
    ind.inf = min.changed|max.changed
    # set the extreme values to Inf for each nr.features.changed (objective 3)
    dds[ind.inf] = Inf
    ods[ind.inf] = Inf
    
    #t = candidates[ord]
    # update the remaining crowding numbers
    if (n > 2L) {
      for (j in 2:(n - 1L)) {
        ods[ord[j]] = ods[ord[j]] + 
          (abs(fitness[ord[j + 1L], i] - fitness[ord[j - 1L], i])/(max[i]-min[i]))
        #ods[ord[j]] = ods[ord[j]] + (fitness[i, ord[j + 1L]] - fitness[i, ord[j]])
        
        dds[ord[j]] = dds[ord[j]] +
          g.dist[ord[j], ord[j-1]] +
          g.dist[ord[j], ord[j+1]]
        
      }
    }
  }
  cds = rank(ods) + rank(dds)
  idx = order(cds, decreasing = TRUE)[1:nr.solutions]
  return(idx)
}


### Oldest Version: Sort for y1
# get_diverse_solutions = function(fitness, pareto.set, range) {
#   checkmate::assert_data_frame(fitness, any.missing = FALSE, nrows = nrow(pareto.set))
#   checkmate::assertDataFrame(pareto.set, any.missing = FALSE, nrows = nrow(fitness))
#   
#   if (!all(rownames(pareto.set) == rownames(fitness))) {
#     stop("rownames of pareto set and rownames of fitness matrix needs to be identical")
#   }
#   
#   n = nrow(fitness)
#   
#   # get the order of the points when sorted according to the i-th objective
#   # 1 is the candidate with the smallest objective function value
#   ord = order(fitness[,1], fitness[,3])
#   
#   gow.dist = StatMatch::gower.dist(pareto.set, rngs = range)
#   gow.dist[gow.dist == 0] = NA
#   
#   obs.div = 1:nrow(pareto.set)
#   similar.obs = c()
#   
#   for (j in 2:n) {
#     id = order(gow.dist[ord[j],])[1:(j-1-length(similar.obs))]
#     
#     if (any(id %in% ord[1:(j-1)])) {
#       gow.dist[ord[j],] = NA
#       gow.dist[,ord[j]] = NA
#       similar.obs = c(similar.obs, ord[j])
#     }
#   }
#   return(obs.div[-similar.obs])
# }



# Old Version 
# get_diverse_solutions = function(fitness, pareto.set, range, nr.solutions) {
# 
#   n = nrow(pareto.set)
#   g.dist = StatMatch::gower.dist(pareto.set, rngs = range)
# 
#   dds = numeric(n)
#   for (i in seq_len(ncol(fitness)-1)) {
# 
#     # get the order of the points when sorted according to the i-th objective
#     ord = order(fitness[,3], fitness[,i])
# 
#     # set the extreme values to Inf
#     dds[ord[1]] = Inf
#     dds[ord[n]] = Inf
# 
#     #t = candidates[ord]
#     # update the remaining crowding numbers
#     if (n > 2L) {
#       for (j in 2:(n - 1L)) {
#         #ods[ord[j]] = ods[ord[j]] + (fitness[i, ord[j + 1L]] - fitness[i, ord[j - 1L]])
#         #ods[ord[j]] = ods[ord[j]] + (fitness[i, ord[j + 1L]] - fitness[i, ord[j]])
# 
#         dds[ord[j]] = dds[ord[j]] +
#           g.dist[ord[j], ord[j-1]] +
#           g.dist[ord[j], ord[j+1]]
# 
#       }
#     }
#   }
# 
#   idx = order(dds, decreasing = TRUE)[1:nr.solutions]
#   return(idx)
# }
# 




