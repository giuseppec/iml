fitness_fun = function(x, x.interest, target, predictor, range = NULL) {
  assertDataFrame(x)
  assertDataFrame(x.interest, nrows = 1, any.missing = FALSE)
  assertNumeric(target, min.len = 1, max.len = 2)
  assertNumeric(range, lower = 0, finite = TRUE, any.missing = TRUE,
    len = ncol(x.interest), null.ok = TRUE)
  if (!(is.null(range)) && !(all(names(x.interest) == names(range)))) {
    stop("range for gower distance needs same order and feature names as
      'x.interest'")
  }
  
  if (any(grepl("use.orig", names(x)))) {
    x = x[, -grep("use.orig", x = names(x))]
  }
  assert_data_frame(x, ncols = ncol(x.interest))
  
  if(!all(names(x) == names(x.interest))) {
    stop("x.interest and x need same column ordering and names")
  }
  equal.type = all(mapply(x, x.interest,
    FUN = function(x1, x2) {class(x1) == class(x2)}))
  if (!equal.type) {
    stop("original x and candidate need same feature types")
  }
  
  # Objective Functions
  pred = predictor$predict(newdata = x)[[1]]
  q1 = vapply(pred, numeric(1), FUN =  function(x) min(abs(x - target)))
  # q1 = ifelse(length(target) == 2 & (pred > target[1]) & (pred < target[2]),
  #   0, ifelse(length(target) == 2, Inf, abs(pred - target)))
  # q1[q1 == Inf] = dif[q1 == Inf]
  q1 = ifelse(length(target) == 2 & (pred > target[1]) & (pred < target[2]), 
    0, q1)
  q2 = StatMatch::gower.dist(data.x = x.interest, data.y = x, rngs = range)[1,]
  q3 = rowSums(x != x.interest[rep(row.names(x.interest), nrow(x)),])
  
  fitness = mapply(function(a,b,c) {
    c(a,b,c)
  }, q1, q2, q3)
  return(fitness)
  }


select_diverse = function (control, population, offspring, fitness, 
  fitness.offspring) {
  assertClass(control, "ecr_control")
  assertClass(control$selectForSurvival, "ecr_selector")
  assertList(population)
  assertList(offspring)
  assertMatrix(fitness, ncols = length(population), any.missing = FALSE, 
    min.rows = 1L)
  assertMatrix(fitness.offspring, ncols = length(offspring), any.missing = FALSE, 
    min.rows = 1L)
  merged.pop = c(population, offspring)
  merged.pop.df = mosmafs::listToDf(merged.pop, control$task$par.set)
  merged.pop.df[, grepl("use.orig", names(merged.pop.df))] = NULL
  merged.fit = cbind(fitness, fitness.offspring)
  surv.idx = control$selectForSurvival(merged.fit, n.select = length(population), 
    merged.pop.df) 
  fitness = merged.fit[, surv.idx, drop = FALSE]
  fitness = BBmisc::addClasses(fitness, "ecr_fitness_matrix")
  fitness = BBmisc::setAttribute(fitness, "minimize", control$task$min)
  return(list(population = merged.pop[surv.idx], fitness = fitness))
}


select_nondom = ecr::makeSelector(
  selector = function(fitness, n.select, population, 
    epsilon = .Machine$double.xmax, 
    extract.duplicates = TRUE) {
    
    # get indices of infeasible solutions with distance to target 
    # bigger epsilon
    infeasible.idx = which(fitness[1,] > epsilon)
    order.infeasible = order(fitness[1, infeasible.idx])
    
    # if duplicates should be substracted, get indices of unique elements
    if (extract.duplicates) {
      duplicated.idx = which(duplicated(t(fitness)))
    } else {
      duplicated.idx = integer(0)
    } 
    
    # normalize objectives
    mean.f = rowMeans(fitness)
    sd.f = apply(fitness, 1, sd)
    sd.f[sd.f == 0] = 1 
    # if any sd 0 --> transform to 1, this is what sklearn does as well.
    # https://github.com/scikit-learn/scikit-learn/blob/7389dbac82d362f296dc2746f10e43ffa1615660/sklearn/preprocessing/data.py#L70
    fitness = apply(fitness, 2, function(x) {
      (x-mean.f)/sd.f
    })
    
    # get domination layers 
    nondom.layers = ecr::doNondominatedSorting(fitness)
    ranks = nondom.layers$ranks
    
    # get number of domination layers
    max.rank = max(ranks)
    
    # change domination layer of infeasible solutions
    i = 0
    for (inf.id in infeasible.idx[order.infeasible]) {
      ranks[inf.id] = max.rank + i
      i = i + 1
    }
    max.rank = max(ranks)
    
    # Extract duplicated points, if number of unique elements does 
    # exceed n.select
    if (extract.duplicates) {
      ranks[duplicated.idx] = max.rank + 1
    } 
    
    # storage for indizes of selected individuals
    new.pop.idxs = integer()
    
    # Count number of points per domination layer and cumulate their numbers 
    front.len = table(ranks)
    front.len.cumsum = cumsum(front.len)
    
    # Get first front, that does exceed n.select
    front.first.nonfit = BBmisc::which.first(front.len.cumsum > n.select)[[1]]
    
    if (front.first.nonfit > 1) {
      new.pop.idxs = (1:length(ranks))[ranks < front.first.nonfit]
    }
    
    
    ########
    # # get the indizes of points for each domination layer
    # idxs.by.rank = lapply(seq(max.rank), function(r) which(nondom.layers$ranks == r))
    # 
    # if (extract.duplicates & (length(unique.idx) > n.select)) {
    #   idxs.by.rank = lapply(idxs.by.rank, function(x) {
    #     x = x[x %in% unique.idx]
    #   })
    # }
    # 
    # # get the number of points in each domination layer ...
    # front.len = sapply(idxs.by.rank, length)
    # 
    # # ... cumulate the number of points of the domination layers ...
    # cum.front.len = cumsum(front.len)
    # 
    # # ... and determine the first domination layer, which does not fit as a whole
    # front.first.nonfit = BBmisc::which.first(cum.front.len > n.select)
    # 
    # if (front.first.nonfit > 1L) {
    #   # in this case at least one nondominated front can be added
    #   new.pop.idxs = unlist(idxs.by.rank[1:(front.first.nonfit - 1L)])
    # }
    ############
    # how many points to select by second criterion, i.e., crowding distance?
    n.diff = n.select - length(new.pop.idxs)
    
    if (n.diff > 0L) {
      idxs.first.nonfit = which(ranks == front.first.nonfit)
      cds = computeCrowdingDistanceR(as.matrix(fitness[, idxs.first.nonfit]), 
        population[idxs.first.nonfit,]) 
      idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
      new.pop.idxs = c(new.pop.idxs, idxs.first.nonfit[idxs2])
    }
    
    # merge the stuff and return
    return(new.pop.idxs)
  },
  supported.objectives = "multi-objective")




#### BEFORE
select_nondom = ecr::makeSelector(
  selector = function(fitness, n.select, population,
  epsilon = .Machine$double.xmax,
  extract.duplicates = TRUE,
  consider.diverse.solutions = TRUE) {

  infeasible.idx = which(fitness[1,] > epsilon)
  order.infeasible = order(fitness[1, infeasible.idx])
  if (extract.duplicates) {
    unique.idx = which(!duplicated(t(fitness)))
  } else {
    unique.idx = integer(0)
  }
  
  if (length(unique.idx) < n.select) {
    print("unique elements < n.select")
  }

  mean.f = rowMeans(fitness)
  sd.f = apply(fitness,1,sd)
  sd.f[sd.f==0] = 1
  # if any sd 0 --> transform to 1, this is what sklearn does as well.
  # https://github.com/scikit-learn/scikit-learn/blob/7389dbac82d362f296dc2746f10e43ffa1615660/sklearn/preprocessing/data.py#L70

  fitness = apply(fitness, 2, function(x) {
    (x-mean.f)/sd.f
  })

  nondom.layers = ecr::doNondominatedSorting(fitness)

  # storage for indizes of selected individuals
  new.pop.idxs = integer()

  # get maximum rank, i.e., the number of domination layers
  max.rank = max(nondom.layers$ranks)

  # change domination layer of infeasible solutions
  i = 0
  for (inf.id in infeasible.idx[order.infeasible]) {
    nondom.layers$ranks[inf.id] = max.rank + i
    i = i + 1
  }
  max.rank = max(nondom.layers$ranks)

  # get the indizes of points for each domination layer
  idxs.by.rank = lapply(seq(max.rank), function(r) which(nondom.layers$ranks == r))

  if (extract.duplicates & (length(unique.idx) > n.select)) {
    idxs.by.rank = lapply(idxs.by.rank, function(x) {
      x = x[x %in% unique.idx]
    })
  }

  # get the number of points in each domination layer ...
  front.len = sapply(idxs.by.rank, length)

  # ... cumulate the number of points of the domination layers ...
  cum.front.len = cumsum(front.len)

  # ... and determine the first domination layer, which does not fit as a whole
  front.first.nonfit = BBmisc::which.first(cum.front.len > n.select)

  if (front.first.nonfit > 1L) {
    # in this case at least one nondominated front can be added
    new.pop.idxs = unlist(idxs.by.rank[1:(front.first.nonfit - 1L)])
  }

  # how many points to select by second criterion, i.e., crowding distance?
  n.diff = n.select - length(new.pop.idxs)

  if (n.diff > 0L) {
    idxs.first.nonfit = idxs.by.rank[[front.first.nonfit]]
    cds = computeCrowdingDistanceR_ver1(as.matrix(fitness[, idxs.first.nonfit]),
      population[idxs.first.nonfit,])
    idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
    new.pop.idxs = c(new.pop.idxs, idxs.first.nonfit[idxs2])
  }

  # merge the stuff and return
  return(new.pop.idxs)
},  supported.objectives = "multi-objective")


## Version 1
## Over all objectives 
computeCrowdingDistanceR_ver1 = function(fitness, candidates) {
  assertMatrix(fitness, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertDataFrame(candidates, nrows = ncol(fitness))
  
  n = ncol(fitness)
  max = apply(fitness, 1, max)
  min = apply(fitness, 1, min)
  dim = nrow(fitness)
  ods = numeric(n)
  dds = numeric(n)
  cds = numeric(n)
  # dat = lapply(candidates, function(x) {
  #   x$use.orig = NULL
  #   return(x)})
  # dat = list_to_df(candidates)
  # 
  numeric.ind = sapply(candidates, is.numeric)
  range = apply(candidates[numeric.ind], 2, function(x) max(x) - min(x))
  range[colnames(candidates)[!numeric.ind]]  = NA
  range = range[names(candidates)]
  
  g.dist = StatMatch::gower.dist(candidates, rngs = range)
  
  for (i in c(1,2,3)) {
    
    # get the order of the points when sorted according to the i-th objective
    #if (i == 1) {
    ord = order(fitness[i, ])
    #}
    # else if (i == 2) {
    #   ord = order(fitness[3, ], fitness[2, ])
    #   min.obj2 = which.min(fitness[2,])
    #   ord = c(min.obj2, ord[!ord %in% min.obj2])
    #   max.obj2 = which.max(fitness[2,])
    #   ord = c(ord[!ord %in% max.obj2], max.obj2)
    # }
    
    # set the extreme values to Inf
    ods[ord[1]] = Inf
    ods[ord[n]] = Inf
    dds[ord[1]] = Inf
    dds[ord[n]] = Inf
    
    #t = candidates[ord]
    # update the remaining crowding numbers
    if (n > 2L) {
      for (j in 2:(n - 1L)) {
        ods[ord[j]] = ods[ord[j]] +
          ((fitness[i, ord[j + 1L]] - fitness[i, ord[j - 1L]])/(max[i]-min[i]))
        #ods[ord[j]] = ods[ord[j]] +
        #((fitness[i, ord[j + 1L]] - fitness[i, ord[j]])/(max[i]-min[i]))
        
        dds[ord[j]] = dds[ord[j]] +
          g.dist[ord[j], ord[j-1]] +
          g.dist[ord[j], ord[j+1]]
        
      }
    }
  }
  
  cds = rank(ods) + rank(dds)
  return(cds)
}


## Version 2
## Seperated by number of features changed 

computeCrowdingDistanceR_ver2 = function(fitness, candidates) {
  assertMatrix(fitness, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertList(candidates)

  n = ncol(fitness)
  max = apply(fitness, 1, max)
  min = apply(fitness, 1, min)
  dim = nrow(fitness)
  ods = numeric(n)
  dds = numeric(n)
  cds = numeric(n)
  dat = lapply(candidates, function(x) {
    x$use.orig = NULL
    return(x)})
  dat = list_to_df(candidates)

  numeric.ind = sapply(dat, is.numeric)
  range = apply(dat[numeric.ind], 2, function(x) max(x) - min(x))
  range[colnames(dat)[!numeric.ind]]  = NA
  range = range[names(dat)]

  g.dist = StatMatch::gower.dist(dat, rngs = range)

  for (i in c(1,2)) {
    ord = order(fitness[3, ], fitness[i, ])
    min.changed = c(TRUE, diff(fitness[3, ord]) > 0)
    max.changed = rev(c(TRUE, diff(rev(fitness[3, ord])) < 0))
    ind.inf = min.changed|max.changed
    # set the extreme values to Inf for each nr.features.changed (objective 3)
    ods[ind.inf] = Inf
    dds[ind.inf] = Inf
    #ods[ord[1]] = Inf
    #ods[ord[n]] = Inf
    #dds[ord[1]] = Inf
    #dds[ord[n]] = Inf

    # update the remaining crowding numbers
    if (n > 2L) {
      for (j in 2:(n - 1L)) {
        ods[ord[j]] = ods[ord[j]] +
          ((fitness[i, ord[j + 1L]] - fitness[i, ord[j - 1L]])/(max[i] - min[i]))
        # ods[ord[j]] = ods[ord[j]] +
        #   ((fitness[i, ord[j + 1L]] - fitness[i, ord[j]])/(max[i] - min[i]))
        dds[ord[j]] = dds[ord[j]] +
          g.dist[ord[j], ord[j-1]] +
          g.dist[ord[j], ord[j+1]]
      }
    }
  }

  cds = rank(ods) + rank(dds)
  return(cds)
}




### Version 1
# computeCrowdingDistanceR = function(fitness, candidates) {
#   assertMatrix(fitness, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
#   assertList(candidates)
# 
#   n = ncol(fitness)
#   dim = nrow(fitness)
#   ods = numeric(n)
#   dds = numeric(n)
#   cds = numeric(n)
#   dat = lapply(candidates, function(x) {
#     x$use.orig = NULL
#     return(x)})
#   dat = list_to_df(candidates)
# 
#   numeric.ind = sapply(dat, is.numeric)
#   range = apply(dat[numeric.ind], 2, function(x) max(x) - min(x))
#   range[colnames(dat)[!numeric.ind]]  = NA
#   range = range[names(dat)]
# 
#   g.dist = StatMatch::gower.dist(dat, rngs = range)
# 
#   for (i in c(1,2,3)) {
# 
#     # get the order of the points when sorted according to the i-th objective
#     if (i %in% c(1, 2)) {
#       ord = order(fitness[i, ])
#     }
#     else if (i == 3) {
#       ord = order(fitness[i, ], fitness[i-1, ])
#       # min.obj2 = which.min(fitness[2,])
#       # ord = c(min.obj2, ord[!ord %in% min.obj2])
#       # max.obj2 = which.max(fitness[2,])
#       # ord = c(ord[!ord %in% max.obj2], max.obj2)
#     }
# 
#     # set the extreme values to Inf
#     ods[ord[1]] = Inf
#     ods[ord[n]] = Inf
#     dds[ord[1]] = Inf
#     dds[ord[n]] = Inf
# 
#     #t = candidates[ord]
#     # update the remaining crowding numbers
#     if (n > 2L) {
#       for (j in 2:(n - 1L)) {
#         #ods[ord[j]] = ods[ord[j]] + (fitness[i, ord[j + 1L]] - fitness[i, ord[j - 1L]])
#         ods[ord[j]] = ods[ord[j]] + (fitness[i, ord[j + 1L]] - fitness[i, ord[j]])
# 
#         dds[ord[j]] = dds[ord[j]] +
#           g.dist[ord[j], ord[j-1]] +
#           g.dist[ord[j], ord[j+1]]
# 
#       }
#     }
#   }
# 
#   cds = rank(ods) + rank(dds)
#   return(cds)
# }


### Version 2
# computeCrowdingDistanceR = function(fitness, candidates) {
#   assertMatrix(fitness, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
#   assertList(candidates)
# 
#   n = ncol(fitness)
#   max = apply(fitness, 1, max)
#   min = apply(fitness, 1, min)
#   dim = nrow(fitness)
#   ods = numeric(n)
#   dds = numeric(n)
#   cds = numeric(n)
#   dat = lapply(candidates, function(x) {
#     x$use.orig = NULL
#     return(x)})
#   dat = list_to_df(candidates)
# 
#   numeric.ind = sapply(dat, is.numeric)
#   range = apply(dat[numeric.ind], 2, function(x) max(x) - min(x))
#   range[colnames(dat)[!numeric.ind]]  = NA
#   range = range[names(dat)]
# 
#   g.dist = StatMatch::gower.dist(dat, rngs = range)
# 
#   for (i in c(1,2)) {
# 
#     # get the order of the points when sorted according to the i-th objective
#     if (i == 1) {
#       ord = order(fitness[i, ])
#     }
#     else if (i == 2) {
#       ord = order(fitness[3, ], fitness[2, ])
#       min.obj2 = which.min(fitness[2,])
#       ord = c(min.obj2, ord[!ord %in% min.obj2])
#       max.obj2 = which.max(fitness[2,])
#       ord = c(ord[!ord %in% max.obj2], max.obj2)
#     }
# 
#     # set the extreme values to Inf
#     ods[ord[1]] = Inf
#     ods[ord[n]] = Inf
#     dds[ord[1]] = Inf
#     dds[ord[n]] = Inf
# 
#     #t = candidates[ord]
#     # update the remaining crowding numbers
#     if (n > 2L) {
#       for (j in 2:(n - 1L)) {
#         ods[ord[j]] = ods[ord[j]] + 
#           ((fitness[i, ord[j + 1L]] - fitness[i, ord[j - 1L]])/(max[i] - min[i]))
#         #ods[ord[j]] = ods[ord[j]] + (fitness[i, ord[j + 1L]] - fitness[i, ord[j]])
#         dds[ord[j]] = dds[ord[j]] +
#           g.dist[ord[j], ord[j-1]] +
#           g.dist[ord[j], ord[j+1]]
#       }
#     }
#   }
# 
#   cds = rank(ods) + rank(dds)
#   return(cds)
# }



