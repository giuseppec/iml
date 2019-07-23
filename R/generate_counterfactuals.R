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
    stop("x.interest and x need same feature types")
  }
  
  # Objective Functions
  pred = predictor$predict(newdata = x)[[1]]
  q1 = vapply(pred, numeric(1), FUN =  function(x) min(abs(x - target)))
  # q1 = ifelse(length(target) == 2 & (pred > target[1]) & (pred < target[2]),
  #   0, ifelse(length(target) == 2, Inf, abs(pred - target)))
  # q1[q1 == Inf] = dif[q1 == Inf]
  q1 = ifelse(length(target) == 2 & (pred >= target[1]) & (pred <= target[2]), 
    0, q1)
  q2 = StatMatch::gower.dist(data.x = x.interest, data.y = x, rngs = range, 
    KR.corr = FALSE)[1,]
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
  browser(expr = anyNA(fitness))
  fitness = BBmisc::addClasses(fitness, "ecr_fitness_matrix")
  fitness = BBmisc::setAttribute(fitness, "minimize", control$task$min)
  return(list(population = merged.pop[surv.idx], fitness = fitness))
}


select_nondom = ecr::makeSelector(
  selector = function(fitness, n.select, population, 
    epsilon = .Machine$double.xmax, 
    extract.duplicates = TRUE, vers = 1) {
    
    assert_number(n.select)
    if (n.select > ncol(fitness)-1) {
      stop("'n.select' must be smaller than 'ncol(fitness)'")
    }
    assert_matrix(fitness, ncols = nrow(population))
    assert_numeric(epsilon, null.ok = TRUE)
    assert_logical(extract.duplicates)
    assert_true(vers %in% c(1, 2, 3))
    
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
    front.first.nonfit = as.numeric(names(front.len.cumsum)[
      BBmisc::which.first(front.len.cumsum > n.select)[[1]]])
    
    if (front.first.nonfit > 1) {
      new.pop.idxs = (1:length(ranks))[ranks < front.first.nonfit]
    }
    # how many points to select by second criterion, i.e., crowding distance?
    n.diff = n.select - length(new.pop.idxs)
    
    if (n.diff > 0L) {
      idxs.first.nonfit = which(ranks == front.first.nonfit)
      if (vers == 1) {
        cds = computeCrowdingDistanceR_ver1(as.matrix(fitness[, idxs.first.nonfit]), 
          population[idxs.first.nonfit,]) 
      }
      if (vers == 2) {
        cds = computeCrowdingDistanceR_ver2(as.matrix(fitness[, idxs.first.nonfit]), 
          population[idxs.first.nonfit,])
      }
      if (vers == 3) {
        cds = ecr::computeCrowdingDistance(as.matrix(fitness[, idxs.first.nonfit]))
      }
      idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
      new.pop.idxs = c(new.pop.idxs, idxs.first.nonfit[idxs2])
    }
    
    # new.pop.idxs.orig = select_nondomfirst(fitness, n.select, population)
    # # merge the stuff and return
    # if (!all(new.pop.idxs == new.pop.idxs.orig)) {
    #   browser()
    # }

    return(new.pop.idxs)
  },
  supported.objectives = "multi-objective")




#### BEFORE
# select_nondomfirst = ecr::makeSelector(
#   selector = function(fitness, n.select, population,
#   epsilon = .Machine$double.xmax,
#   extract.duplicates = TRUE,
#   consider.diverse.solutions = TRUE) {
# 
#   infeasible.idx = which(fitness[1,] > epsilon)
#   order.infeasible = order(fitness[1, infeasible.idx])
#   if (extract.duplicates) {
#     unique.idx = which(!duplicated(t(fitness)))
#   } else {
#     unique.idx = integer(0)
#   }
#   
#   if (length(unique.idx) < n.select) {
#     print("unique elements < n.select")
#   }
# 
#   mean.f = rowMeans(fitness)
#   sd.f = apply(fitness,1,sd)
#   sd.f[sd.f==0] = 1
#   # if any sd 0 --> transform to 1, this is what sklearn does as well.
#   # https://github.com/scikit-learn/scikit-learn/blob/7389dbac82d362f296dc2746f10e43ffa1615660/sklearn/preprocessing/data.py#L70
# 
#   fitness = apply(fitness, 2, function(x) {
#     (x-mean.f)/sd.f
#   })
# 
#   nondom.layers = ecr::doNondominatedSorting(fitness)
# 
#   # storage for indizes of selected individuals
#   new.pop.idxs = integer()
# 
#   # get maximum rank, i.e., the number of domination layers
#   max.rank = max(nondom.layers$ranks)
# 
#   # change domination layer of infeasible solutions
#   i = 0
#   for (inf.id in infeasible.idx[order.infeasible]) {
#     nondom.layers$ranks[inf.id] = max.rank + i
#     i = i + 1
#   }
#   max.rank = max(nondom.layers$ranks)
# 
#   # get the indizes of points for each domination layer
#   idxs.by.rank = lapply(seq(max.rank), function(r) which(nondom.layers$ranks == r))
# 
#   if (extract.duplicates & (length(unique.idx) > n.select)) {
#     idxs.by.rank = lapply(idxs.by.rank, function(x) {
#       x = x[x %in% unique.idx]
#     })
#   }
# 
#   # get the number of points in each domination layer ...
#   front.len = sapply(idxs.by.rank, length)
# 
#   # ... cumulate the number of points of the domination layers ...
#   cum.front.len = cumsum(front.len)
# 
#   # ... and determine the first domination layer, which does not fit as a whole
#   front.first.nonfit = BBmisc::which.first(cum.front.len > n.select)
# 
#   if (front.first.nonfit > 1L) {
#     # in this case at least one nondominated front can be added
#     new.pop.idxs = sort(unlist(idxs.by.rank[1:(front.first.nonfit - 1L)]))
#   }
# 
#   # how many points to select by second criterion, i.e., crowding distance?
#   n.diff = n.select - length(new.pop.idxs)
# 
#   if (n.diff > 0L) {
#     idxs.first.nonfit = idxs.by.rank[[front.first.nonfit]]
#     cds = computeCrowdingDistanceR_ver1(as.matrix(fitness[, idxs.first.nonfit]),
#       population[idxs.first.nonfit,])
#     idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
#     new.pop.idxs = c(new.pop.idxs, idxs.first.nonfit[idxs2])
#   }
# 
#   # merge the stuff and return
#   return(new.pop.idxs)
# },  supported.objectives = "multi-objective")


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
  
  g.dist = StatMatch::gower.dist(candidates, KR.corr = FALSE)
  
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
  
  cds = rank(ods, ties.method = "random") + 
    rank(dds, ties.method = "random")
  cds = jitter(cds, factor = 1)
  return(cds)
}


## Version 2
## Seperated by number of features changed 
# 
computeCrowdingDistanceR_ver2 = function(fitness, candidates) {
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

  g.dist = StatMatch::gower.dist(candidates, KR.corr = FALSE)

  for (i in c(1, 2)) {

    # get the order of the points when sorted according to the i-th objective
    ord = order(fitness[3,], fitness[i,])
    min.changed = c(TRUE, diff(fitness[3, ord]) > 0)
    max.changed = rev(c(TRUE, diff(rev(fitness[3, ord])) < 0))
    ind.inf = min.changed|max.changed
    # set the extreme values to Inf for each nr.features.changed (objective 3)
    dds[ord[ind.inf]] = Inf
    ods[ord[ind.inf]] = Inf

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
  cds = rank(ods, ties.method = "random") + 
    rank(dds, ties.method = "random")
  cds = jitter(cds, factor = 1)
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


# custom_generateOffspring = function (control, inds, fitness, lambda, p.recomb = 0.7, p.mut = 0.1) 
# {
#   if (is.null(control$mutate) & is.null(control$recombinate)) 
#     stopf("generateOffspring: At least a mutator or recombinator needs to be available.")
#   offspring = if (!is.null(control$recombine)) {
#     custom_recombinate(control, inds, fitness, lambda, p.recomb = p.recomb)
#   }
#   else {
#     mating.idx = custom_getMatingPool(control, inds, fitness, lambda = lambda)
#     offspring = inds[as.integer(mating.idx)]
#   }
#   offspring = ecr:::mutate(control, offspring, p.mut = p.mut)
#   return(offspring)
# }
# 
# 
# custom_getMatingPool = function(control, inds, fitness, lambda = length(inds), slot = "recombine") {
#   assertFunction(control$selectForMating)
#   recombinatorFun = control[[slot]]
#   
#   #FIXME: eventually drop this in order to come up with a simpler interface
#   #FIXME: why all the recombinator checks? If none is passed we cannot recombine!
#   # determine how many elements need to be chosen by parentSelector
#   # if no recombinator exists we select simply lambda elements
#   n.mating = lambda
#   n.parents = 1L
#   if (!is.null(recombinatorFun)) {
#     n.children = ecr:::getNumberOfChildren.ecr_recombinator(recombinatorFun)
#     n.parents = ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(recombinatorFun)
#     n.mating = ceiling(lambda * n.parents / n.children)
#     if (n.mating == 1L)
#       n.mating = n.parents
#     # if number of offspring is odd and number of mating
#     if (n.mating %% n.parents != 0L)
#       n.mating = n.mating + (n.mating %% n.parents)
#   }
#   # create mating pool. This a a matrix, where each row contains the indizes of
#   # a set of >= 2 parents
#   mating.idx = matrix(custom_selectForMating(control, fitness, n.select = n.mating, inds), 
#     ncol = n.parents)
#   return(mating.idx)
# }
# 
# 
# custom_selectForMating <- function (control, fitness, n.select, inds) 
# {
#   assertClass(control, "ecr_control")
#   assertClass(control$selectForMating, "ecr_selector")
#   assertMatrix(fitness, min.rows = 1L, any.missing = FALSE, 
#     all.missing = FALSE)
#   n.select = asInt(n.select, lower = 1L)
#   ecr:::checkIfSelectorMatchesObjectives(control$selectForMating, 
#     control, "selectForMating")
#   fitness = ecr:::transformFitness(fitness, control$task, control$selectForMating)
#   control$selectForMating(fitness, inds, n.select = n.select)
# }
# 
# 
# custom_selTournamentMO <- makeSelector(function(fitness, n.select, 
#   inds,  sorting = "crowding", 
#   ref.point, k = 2, return.unique = FALSE) {
#   assertMatrix(fitness, min.cols = 1, min.rows = 2)
#   assertFlag(return.unique)
#   assertInt(n.select, lower = 1, upper = if (return.unique) ncol(fitness) else Inf)
#   assertInt(k, lower = 1)
#   k <- min(k, ncol(fitness))
#   rank.all <- custom_overallRankMO(fitness, inds, sorting, ref.point)
#   
#   pool <- seq_len(ncol(fitness))
#   replicate(n.select, {
#     if (k >= length(pool)) {
#       competitors <- pool
#     } else {
#       competitors <- sample(pool, k, replace = FALSE)
#     }
#     choice <- competitors[which.min(rank.all[competitors])]
#     if (return.unique) {
#       pool <<- setdiff(pool, choice)
#     }
#     choice
#   })
# }, supported.objectives = "multi-objective")
# 
# 
# 
# custom_overallRankMO <- function(fitness, inds, sorting = "crowding", ref.point) {
#   assertChoice(sorting, c("crowding", "domhv"))
#   if (sorting == "domhv") {
#     assertNumeric(ref.point, finite = TRUE, any.missing = FALSE, len = nrow(fitness))
#   }
#   
#   assertMatrix(fitness, min.cols = 1, min.rows = 2)
#   ranksort0 <- doNondominatedSorting(fitness)$ranks
#   ranksort1 <- vector("numeric", length(ranksort0))
#   for (rnk in unique(ranksort0)) {
#     subfit <- fitness[, ranksort0 == rnk, drop = FALSE]
#     subfit <- subfit + runif(length(subfit), -1, 1) * .Machine$double.eps * 2^9 * subfit
#     subinds <- inds[ranksort0 == rnk]
#     if (sorting == "crowding") {
#       secondary <- computeCrowdingDistanceR_ver1(as.matrix(subfit[1:2,]), subinds)
#     } else {
#       if (ncol(subfit) == 1) {
#         # TODO: don't need this any more when https://github.com/jakobbossek/ecr2/issues/109 is fixed
#         secondary <- prod(ref.point - subfit)
#       } else {
#         secondary <- computeHVContr(subfit, ref.point)
#       }
#     }
#     ranksort1[ranksort0 == rnk] <- secondary
#   }
#   rankresult <- vector("integer", ncol(fitness))
#   rankresult[order(ranksort0, -ranksort1)] <- seq_len(ncol(fitness))
#   rankresult
# }
# 
