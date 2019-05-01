 custom.mutGauss = ecr::makeMutator(
  mutator = function (ind, p = 1L, sdev = 0.05, lower, upper) 
  {
    checkmate::assert_number(p, lower = 0, finite = TRUE, na.ok = FALSE)
    checkmate::assert_numeric(lower, any.missing = FALSE, all.missing = FALSE)
    checkmate::assert_numeric(lower, any.missing = FALSE, all.missing = FALSE)
    checkmate::assert_numeric(sdev, len = length(ind))
    if (length(lower) != length(upper)) {
      stopf("Gauss mutator: length of lower and upper bounds need to be equal!")
    }
    n = length(ind)
    mut.idx = runif(n) < p
    
    if (length(sdev)>0) {
      mut.noise = c()
      for (sd in sdev[mut.idx]) {
        mut = rnorm(1, mean = 0, sd = sd)
        mut.noise = c(mut.noise, mut)
      }
    }
    
    else {
      mut.noise = rnorm(sum(mut.idx), mean = 0, sd = sdev)
    }
    
    ind[mut.idx] = ind[mut.idx] + mut.noise
    ind = pmin(pmax(lower, ind), upper)
    return(ind)
  },
  supported = "float")


custom.mutGaussInt = mosmafs::intifyMutator(custom.mutGauss)



selNondom = ecr::makeSelector(
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
      if (!consider.diverse.solutions) {
        cds = ecr:::computeCrowdingDistanceR(fitness[1:2, idxs.first.nonfit])
      }
      if (consider.diverse.solutions) {
        cds = computeCrowdingDistanceR(as.matrix(fitness[, idxs.first.nonfit]), 
          population[idxs.first.nonfit]) 
      }
      idxs2 = order(cds, decreasing = TRUE)[1:n.diff]
      new.pop.idxs = c(new.pop.idxs, idxs.first.nonfit[idxs2])
    }
    
    # merge the stuff and return
    return(new.pop.idxs)
  },
  supported.objectives = "multi-objective")


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
#   dat = listToDf(candidates)
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
#   dat = listToDf(candidates)
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



# 
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
#   dat = listToDf(candidates)
# 
#   numeric.ind = sapply(dat, is.numeric)
#   range = apply(dat[numeric.ind], 2, function(x) max(x) - min(x))
#   range[colnames(dat)[!numeric.ind]]  = NA
#   range = range[names(dat)]
# 
#   g.dist = StatMatch::gower.dist(dat, rngs = range)
# 
#   for (i in c(1,2)) {
#     ord = order(fitness[3, ], fitness[i, ])
#     min.changed = c(TRUE, diff(fitness[3, ord]) > 0) 
#     max.changed = rev(c(TRUE, diff(rev(fitness[3, ord])) < 0))
#     ind.inf = min.changed|max.changed
#     # set the extreme values to Inf for each nr.features.changed (objective 3)
#     ods[ind.inf] = Inf
#     dds[ind.inf] = Inf
#     #ods[ord[1]] = Inf
#     #ods[ord[n]] = Inf
#     #dds[ord[1]] = Inf
#     #dds[ord[n]] = Inf
# 
#     # update the remaining crowding numbers
#     if (n > 2L) {
#       for (j in 2:(n - 1L)) {
#         ods[ord[j]] = ods[ord[j]] +
#           ((fitness[i, ord[j + 1L]] - fitness[i, ord[j - 1L]])/(max[i] - min[i]))
#         # ods[ord[j]] = ods[ord[j]] +
#         #   ((fitness[i, ord[j + 1L]] - fitness[i, ord[j]])/(max[i] - min[i]))
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

#### ORIGINAL
computeCrowdingDistanceR = function(fitness, candidates) {
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
  dat = listToDf(candidates)

  numeric.ind = sapply(dat, is.numeric)
  range = apply(dat[numeric.ind], 2, function(x) max(x) - min(x))
  range[colnames(dat)[!numeric.ind]]  = NA
  range = range[names(dat)]

  g.dist = StatMatch::gower.dist(dat, rngs = range)

  for (i in c(1,2)) {

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
