calculate.ale.num = function(dat, run.prediction, feature.name, grid.size){
  # from number of intervals to number of borders
  n.borders = grid.size + 1
  # Handling duplicated grid values
  grid.dt = unique(get.grid(dat[,feature.name, with = FALSE], n.borders, type = "quantile"))
  interval.index = findInterval(dat[[feature.name]], grid.dt[[1]], left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index[interval.index == 0] = 1
  X.lower = X.upper = dat
  X.lower[,feature.name] = grid.dt[interval.index,]
  X.upper[,feature.name] = grid.dt[interval.index + 1,]
  predictions.lower = run.prediction(X.lower)
  predictions.upper =  run.prediction(X.upper)
  predictions = predictions.upper - predictions.lower
  res = cbind(X.lower[,feature.name, with=FALSE], predictions, data.frame(.interval = interval.index))
  y.hat.names = setdiff(colnames(res), c(colnames(dat), ".interval"))
  res = melt(res, variable.name = ".class", 
    value.name = ".y.hat", measure.vars = y.hat.names)
  res = res[order(.class, .interval), .(.y.hat = mean(.y.hat)), by = list(.interval, .class)]
  res = res[,.(.y.hat.cumsum = cumsum_na(c(0, .y.hat))), by = .class]
  interval.sizes = as.numeric(table(interval.index))
  res = res[, .(.ale = .y.hat.cumsum - sum((res$.y.hat.cumsum[1:(nrow(.SD) - 1)] + res$.y.hat.cumsum[2:nrow(.SD)])/2 * interval.sizes)/sum(interval.sizes), .id = 1:nrow(.SD)), by = .class]
  res$.type = "ale"
  grid.dt$.id = 1:nrow(grid.dt)
  res = merge(res, grid.dt, by = ".id")
  data.frame(res)
}

calculate.ale.num.num = function(dat, run.prediction, feature.name, grid.size){
  ## Create ALE for feature 1
  grid.dt1 = unique(get.grid(dat[,feature.name[1], with = FALSE], grid.size = grid.size[1] + 1, type = "quantile"))
  colnames(grid.dt1)= feature.name[1]
  interval.index1 = findInterval(dat[[feature.name[1]]], grid.dt1[[1]] , left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index1[interval.index1 == 0] = 1
  ## Create ALE for feature 2
  grid.dt2 = unique(get.grid(dat[,feature.name[2], with = FALSE], grid.size  =grid.size[2] + 1, type = "quantile"))
  colnames(grid.dt2)= feature.name[2]
  interval.index2 = findInterval(dat[[feature.name[2]]], grid.dt2[[1]], left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index2[interval.index2 == 0] = 1
  X.low1.low2 = X.up1.low2 = X.low1.up2 = X.up1.up2 = copy(dat)
  X.low1.low2[,feature.name] = data.table(grid.dt1[interval.index1,], grid.dt2[interval.index2,])
  X.up1.low2[,feature.name] = data.table(grid.dt1[interval.index1 + 1,], grid.dt2[interval.index2,])
  X.low1.up2[,feature.name] = data.table(grid.dt1[interval.index1,], grid.dt2[interval.index2 + 1,])
  X.up1.up2[,feature.name] = data.table(grid.dt1[interval.index1 + 1,], grid.dt2[interval.index2 + 1,])
  # Getting all predictions from the model
  predictions.11 = run.prediction(X.low1.low2)
  predictions.21 = run.prediction(X.up1.low2)
  predictions.12 = run.prediction(X.low1.up2)
  predictions.22 = run.prediction(X.up1.up2)
  predictions = (predictions.22 - predictions.21) - (predictions.12 - predictions.11) 
  res = cbind(dat[,feature.name, with=FALSE], predictions, data.frame(.interval1 = interval.index1, .interval2 = interval.index2))
  y.hat.names = setdiff(colnames(res), c(colnames(dat), c(".interval1", ".interval2")))
  # instead of a matrix, we work with melted version of the data
  # This allows us to work with multi-dimensional predictions
  res = melt(res, variable.name = ".class", 
    value.name = ".y.hat", measure.vars = y.hat.names)
  # Make sure all intervals are included
  interval_grid = expand.grid(.interval1 = unique(res$.interval1), .interval2 = unique(res$.interval2),
    .class = unique(res$.class))
  res = merge(res, interval_grid, on = c(".interval1", ".interval2", ".class"), all.y = TRUE)
  res = res[order(.class, .interval1, .interval2), .(.yhat.diff = mean(.y.hat)), by = list(.interval1, .interval2, .class)]
  
  
  # fill empty cells with the closest neighbour cells value
  # remember cell status for later
  res.na.cell = copy(res)
  res.na.cell[, missing := is.na(.yhat.diff)]
  res.na.cell[, .yhat.diff := NULL]
  # replace the missing ones with the closest non-missing difference (measured in number of intervals)
  res = impute_cells(res, grid1 = grid.dt1, grid2 = grid.dt2, 
    x1.ind = ".interval1", x2.ind = ".interval2")
  
  # Acumulate the predictions from bottom left to top right 
  res = res[,.(.y.hat.cumsum = cumsum_na(c(0, .yhat.diff)), .interval2 = c(0, .interval2)), by = .(.class, .interval1)]
  res = res[,.(.y.hat.cumsum = cumsum_na(c(0, .y.hat.cumsum)), .interval1 = c(0, .interval1)), by = .(.class, .interval2)]
  # Number of cells are need for weighting later
  cell.counts = as.matrix(table(interval.index1, interval.index2))
  cell.counts.m = melt(cell.counts)
  colnames(cell.counts.m) = c(".interval1", ".interval2",  ".count")
  cell.counts.m$.interval1 = as.numeric(as.character(cell.counts.m$.interval1))
  cell.counts.m$.interval2 = as.numeric(as.character(cell.counts.m$.interval2))
  res = merge(res, cell.counts.m, on = c(".interval1", ".interval2"), all.x = TRUE)
  
  # Computing the first-order effect of feature 1
  # First, take the differences across feature 1
  res1 = res[, .(.y.hat.diffs = .y.hat.cumsum[2:nrow(.SD)] - .y.hat.cumsum[1:(nrow(.SD) - 1)], .interval1 = .interval1[2:(nrow(.SD))], .count = .count[2:(nrow(.SD))]),
    by = list(.class, .interval2)]
  # Then take the prediction at the mid point of each interval, which is the mean of the prediction at the end points
  #     and take calculate the mean, weighted by the number of data instances per cell
  res1 = res1[, .(.fJ1 = sum(.count[2:nrow(.SD)] * (.y.hat.diffs[1:(nrow(.SD) - 1)] + 
      .y.hat.diffs[2:(nrow(.SD))]) / 2) / sum(.count[2:nrow(.SD)])), by = list(.class, .interval1)]
  fJ1 = res1[, .(.fJ1 = c(0, cumsum_na(.fJ1)), .interval1 = c(0, .interval1)), by = list(.class)]
  # Computing the first-order effect of feature 2
  # First, take the differences across feature 2
  res2 = res[, .(.y.hat.diffs = .y.hat.cumsum[2:nrow(.SD)] - .y.hat.cumsum[1:(nrow(.SD) - 1)], .interval2 = .interval2[2:(nrow(.SD))], .count = .count[2:(nrow(.SD))]),
    by = list(.class, .interval1)]
  # Then take the prediction at the mid point of each interval, which is the mean of the prediction at the end points
  #     and take calculate the mean, weighted by the number of data instances per cell
  res2 = res2[, .(.fJ2 = sum(.count[2:nrow(.SD)] * (.y.hat.diffs[1:(nrow(.SD) - 1)] + 
      .y.hat.diffs[2:(nrow(.SD))]) / 2) / sum(.count[2:nrow(.SD)])), by = list(.class, .interval2)]
  fJ2 = res2[, .(.fJ2 = c(0, cumsum_na(.fJ2)), .interval2 = c(0, .interval2)), by = list(.class)]
  # for each cells computes the average prediction through mean of the cell corners
  # then again a mean over all cells, weighted by the number of data points in the cell
  cls = unique(res$.class)
  fJ0 = unlist(lapply(cls, function(cl){
    res.cl = res[.class == cl,]
    fJ1.cl = fJ1[.class == cl,]
    fJ2.cl = fJ2[.class == cl,]
    dd = as.matrix(dcast(res.cl, .interval1 ~ .interval2, value.var = ".y.hat.cumsum", drop = FALSE))[,-1]
    dd = dd  - outer(fJ1.cl$.fJ1,rep(1,nrow(fJ2.cl))) - outer(rep(1,nrow(fJ1.cl)),fJ2.cl$.fJ2)
    sum(cell.counts *(dd[1:(nrow(dd)-1),1:(ncol(dd)-1)] + dd[1:(nrow(dd)-1),2:ncol(dd)] + dd[2:nrow(dd),1:(ncol(dd)-1)] + dd[2:nrow(dd), 2:ncol(dd)])/4, na.rm = TRUE)/sum(cell.counts)
  }))
  fJ0 = data.frame(.fJ0 = fJ0, .class = cls)
  res = merge(res, fJ0, by = c(".class"))
  res = merge(res, fJ1, by = c(".class", ".interval1"))
  res = merge(res, fJ2, by = c(".class", ".interval2"))
  res = res[, .ale := .y.hat.cumsum - .fJ1 - .fJ2 - .fJ0]
  # For later plotting, define the rectangles
  # These are not the same as the cells, which is a bit counterintuitive
  # each value in fJ is where the cells cross
  # instead of coloring the cells, we color a rectangle around each cell cross point
  # and for this we need to compute rectangles around these cross points
  # in image() this happens automatically (see ALEPlot::ALEPlot)
  # for the edges, simply use the grid value as the outer values
  interval.dists = diff(grid.dt1[c(1, 1:nrow(grid.dt1), nrow(grid.dt1))][[1]])
  interval.dists = 0.5 *  interval.dists
  res$.right = grid.dt1[res$.interval1 + 1, ] + interval.dists[res$.interval1 + 2]
  res$.left = grid.dt1[res$.interval1 + 1, ] - interval.dists[res$.interval1 + 1]
  interval.dists2 = diff(grid.dt2[c(1, 1:nrow(grid.dt2), nrow(grid.dt2))][[1]])
  interval.dists2 = 0.5 *  interval.dists2
  res$.bottom = grid.dt2[res$.interval2 + 1, ] + interval.dists2[res$.interval2 + 2]
  res$.top = grid.dt2[res$.interval2 + 1, ] - interval.dists2[res$.interval2 + 1]
  res[,feature.name[1]] =  grid.dt1[res$.interval1 + 1, ]
  res[,feature.name[2]] =  grid.dt2[res$.interval2 + 1, ]
  
  res = res[, setdiff(colnames(res), c(".fJ0", ".fJ1", ".fJ2", ".y.hat.cumsum", ".count", 
    ".interval1", ".interval2")), with = FALSE]
  res$.type = "ale"
  
  data.frame(res)
}




calculate.ale.cat = function(dat, run.prediction, feature.name){
  x = dat[,feature.name, with = FALSE][[1]]
  levels.original = levels(x)
  # if ordered, than already use that
  if(inherits(x, "ordered")){
    level_order = 1:nlevels(x)
  } else {
    # reorders according to the distance of the levels in the other features
    level_order = order_levels(dat, feature.name)
  }
  # The new order of the levels
  levels.ordered = levels.original[level_order]
  # The feature with the levels in the new order
  x.ordered  = order(level_order)[as.numeric(x)]
  X.lower = X.upper = dat
  
  # We only want to increase/decrease the ones that are not already the minimum or maximum level
  row.ind.increase = (1:nrow(dat))[x.ordered < nlevels(x)]  
  row.ind.decrease = (1:nrow(dat))[x.ordered > 1]  
  
  
  # increase / decrease the level where not at minium or maxium already
  X.lower[row.ind.decrease, feature.name] = levels.ordered[x.ordered[row.ind.decrease] - 1]
  X.upper[row.ind.increase, feature.name] = levels.ordered[x.ordered[row.ind.increase] + 1]
  y.hat = run.prediction(dat)
  y.hat.increase = run.prediction(X.upper[row.ind.increase,])
  y.hat.decrease = run.prediction(X.lower[row.ind.decrease,])
  
  # To measure the difference or 'jump' in prediction between categories, we come from both directions:
  # We measure the difference in prediction when we increase category k to k+1 for instances with category k
  # We also measure the difference in prediction when we decrease category k+1 to k for instance with category k+1
  d.increase = y.hat.increase - y.hat[row.ind.increase,]
  d.decrease = y.hat[row.ind.decrease,] - y.hat.decrease
  # Compute the differences and the ALE
  deltas = rbind(d.increase, d.decrease)
  deltas = cbind(deltas, .level.jump = c(x.ordered[row.ind.increase], x.ordered[row.ind.decrease] - 1))
  y.hat.names = colnames(y.hat)
  deltas = data.table(melt(deltas, id.vars = c(".level.jump"), variable.name = ".class", value.name = ".yhat.diff"))
  
  # All those difference are aggregated (averaged) grouped by the jump between levels (and by .class for multi output)
  deltas = deltas[order(.level.jump),.(.yhat.diff = mean(.yhat.diff)),by = list(.class, .level.jump)]
  deltas = deltas[, .(.ale = c(0, cumsum(.yhat.diff))), by = list(.class)]
  x.count = as.numeric(table(x))
  x.prob = x.count/sum(x.count) 
  deltas = deltas[, .(.ale = .ale - sum(.ale * x.prob[level_order]), .level = levels.ordered), by = ".class"]
  colnames(deltas) = c(".class", ".ale", feature.name)
  deltas[, feature.name] = factor(deltas[,feature.name,with=FALSE][[1]], levels = levels.ordered)
  # make sure the rows are ordered by the new level order
  data.frame(deltas[order(deltas[[feature.name]]),])
}



calculate.ale.num.cat = function(dat, run.prediction, feature.name, grid.size){
  
  # Figure out which feature is numeric and which categeorical
  x.num.index = ifelse(inherits(dat[,feature.name,with = FALSE][[1]], "numeric"), 1, 2)
  x.cat.index = setdiff(c(1,2), x.num.index)
  x.num = dat[,feature.name[x.num.index],with = FALSE][[1]]
  x.cat = dat[,feature.name[x.cat.index],with = FALSE][[1]]
  
  levels.original = levels(x.cat)
  # if ordered, than already use that
  if(inherits(x.cat, "ordered")){
    level_order = 1:nlevels(x.cat)
  } else {
    # reorders according to the distance of the levels in the other features
    level_order = order_levels(dat, feature.name[x.cat.index])
  }
  # The new order of the levels
  levels.ordered = levels.original[level_order]
  # The feature with the levels in the new order
  x.cat.ordered  = order(level_order)[as.numeric(x.cat)]
  # The rows for which the category can be increased
  row.ind.increase = (1:nrow(dat))[x.cat.ordered < nlevels(x.cat)]  
  row.ind.decrease <- (1:nrow(dat))[x.cat.ordered > 1]  

  
  ## Create ALE for increasing categorical feature
  grid.dt = unique(get.grid(dat[,feature.name[x.num.index], with = FALSE], 
    grid.size  = grid.size[x.num.index] + 1, type = "quantile"))
  colnames(grid.dt)= feature.name[x.num.index]
  interval.index = findInterval(dat[[feature.name[x.num.index]]], grid.dt[[1]], left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index[interval.index == 0] = 1
  
  
  # We have to do this differencing thing twice, since we have a categorical feature involved
  # The categorical feature is treated by increase and decreasing the levels
  
  X.low1.low2 = X.up1.low2 = X.low1.up2 = X.up1.up2 = copy(dat)
  
  # put category as first dimension without loss of generality
  X.low1.low2[row.ind.increase, feature.name[x.num.index]] = 
    grid.dt[interval.index,][[1]][row.ind.increase]
  X.low1.up2[row.ind.increase, feature.name[x.num.index]] = 
    grid.dt[interval.index + 1,][[1]][row.ind.increase]
  X.up1.low2[row.ind.increase, feature.name[x.cat.index]] = 
    levels.ordered[x.cat.ordered[row.ind.increase] + 1]
  X.up1.low2[row.ind.increase, feature.name[x.num.index]] = 
    grid.dt[interval.index,][[1]][row.ind.increase]
  X.up1.up2[row.ind.increase, feature.name[x.cat.index]] =  
    levels.ordered[x.cat.ordered[row.ind.increase] + 1]
  X.up1.up2[row.ind.increase, feature.name[x.num.index]] = 
    grid.dt[interval.index+1,1][[1]][row.ind.increase]
  
  
  # Getting all predictions from the model
  predictions.11 = run.prediction(X.low1.low2[row.ind.increase,])
  predictions.21 = run.prediction(X.up1.low2[row.ind.increase,])
  predictions.12 = run.prediction(X.low1.up2[row.ind.increase,])
  predictions.22 = run.prediction(X.up1.up2[row.ind.increase,])
  
  d.increase = (predictions.22  - predictions.21) - (predictions.12 - predictions.11)
  
  X.low1.low2 = X.up1.low2 = X.low1.up2 = X.up1.up2 = copy(dat)
  
  # put category as first dimension without loss of generality
  X.low1.low2[row.ind.decrease, feature.name[x.cat.index]] = 
    levels.ordered[x.cat.ordered[row.ind.decrease] - 1]
  X.low1.low2[row.ind.decrease, feature.name[x.num.index]] = 
    grid.dt[interval.index,][[1]][row.ind.decrease]
  X.low1.up2[row.ind.decrease, feature.name[x.cat.index]] = 
    levels.ordered[x.cat.ordered[row.ind.decrease] - 1]
  X.low1.up2[row.ind.decrease, feature.name[x.num.index]] = 
    grid.dt[interval.index  + 1,][[1]][row.ind.decrease]
  X.up1.low2[row.ind.decrease, feature.name[x.num.index]] = 
    grid.dt[interval.index,][[1]][row.ind.decrease]
  X.up1.up2[row.ind.decrease, feature.name[x.num.index]] = 
    grid.dt[interval.index + 1,1][[1]][row.ind.decrease]
  
  
  # Getting all predictions from the model
  predictions.11 = run.prediction(X.low1.low2[row.ind.decrease,])
  predictions.21 = run.prediction(X.up1.low2[row.ind.decrease,])
  predictions.12 = run.prediction(X.low1.up2[row.ind.decrease,])
  predictions.22 = run.prediction(X.up1.up2[row.ind.decrease,])
  
  d.decrease = (predictions.22  - predictions.21) - (predictions.12 - predictions.11)
  
  
  # Compute the differences and the ALE
  deltas = rbind(d.increase, d.decrease)
  deltas = cbind(deltas, .level = c(x.cat.ordered[row.ind.increase], x.cat.ordered[row.ind.decrease] - 1), 
    .num = interval.index[c(row.ind.increase, row.ind.decrease)])
  y.hat.names = colnames(predictions.22)
  deltas = data.table(melt(deltas, measure.vars = y.hat.names, variable.name = ".class", value.name = ".yhat.diff"))
  # add empty cells
  interval_grid = expand.grid(.level = unique(deltas$.level), .num = unique(deltas$.num),
    .class = unique(deltas$.class))
  deltas = merge(deltas, interval_grid, on = c(".level", ".num", ".class"), all.y = TRUE)
  deltas = deltas[order(.class, .level, .num),.(.yhat.diff = mean(.yhat.diff)),by = list(.class, .level, .num)]
  
  # fill empty cells with the closest neighbour cells value
  # remember cell status for later
  deltas.na.cell = copy(deltas)
  deltas.na.cell[, missing := is.na(.yhat.diff)]
  deltas.na.cell[, .yhat.diff := NULL]
  # replace the missing ones with the closest non-missing difference (measured in number of intervals)
  deltas = impute_cells(deltas, grid2 = grid.dt, x1.ind = ".level", x2.ind = ".num")
  
  # Acumulate the predictions from bottom left to top right 
  deltas = deltas[, .(.ale = cumsum(c(0, .yhat.diff)), .num = c(0, .num)), by = list(.class, .level)]
  deltas = deltas[, .(.ale = cumsum(c(0, .ale)), .level = c(0, .level)), by = list(.class, .num)]
  
  # Number of cells are need for weighting later
  cell.counts = as.matrix(table(x.cat.ordered, interval.index))
  cell.counts.m = melt(cell.counts)
  colnames(cell.counts.m) = c(".level", ".num",  ".count")
  cell.counts.m$.level = as.numeric(as.character(cell.counts.m$.level)) - 1
  cell.counts.m$.num = as.numeric(as.character(cell.counts.m$.num)) 
  deltas = merge(deltas, cell.counts.m, on = c(".level", ".num"), all.x = TRUE)
  deltas[is.na(.count), .count := 0] 
  
  # Computing the first-order effect of feature 2, the numerical feature.
  # First, take the differences across feature 2
  deltas2 = deltas[, .(.ale2 = .ale[2:nrow(.SD)] - .ale[1:(nrow(.SD) - 1)], .num = .num[2:(nrow(.SD))], .count = .count[2:(nrow(.SD))]),
    by = list(.class, .level)]
  # Then take the weighted average over the categories to get the effect at each numerical grid point.
  deltas2 = deltas2[, .(.ale2 = sum(.count * .ale2) / sum(.count)), by = list(.class, .num)]
  fJ2 = deltas2[order(.num), .(.ale2 = c(0, cumsum(.ale2)), .num = c(0, .num)), by = list(.class)]
  
  
  deltas1 = deltas[, .(.ale1 = .ale[2:nrow(.SD)] - .ale[1:(nrow(.SD) - 1)], .level = .level[2:(nrow(.SD))], 
     .count = (.count[2:nrow(.SD)] + .count[1:(nrow(.SD)-1)]) / 2), by = list(.class, .num)]
  # mid points between numerical intervals
  deltas1 = deltas1[, .(
    .ale1 = (.ale1[2:nrow(.SD)] + .ale1[1:(nrow(.SD)-1)]) / 2,
    .count = .count[2:nrow(.SD)],
    .num = .num[2:nrow(.SD)]), by = list(.class, .level)]
  
  deltas1  = deltas1[,.(.ale1 = sum(.ale1 * .count) / sum(.count)), by = list(.class, .level)]
  fJ1 = deltas1[order(.level), .(.ale1 = c(0, cumsum_na(.ale1)), .level = c(0, .level)), by = list(.class)]
  
  cls = unique(deltas$.class)
  fJ0 = unlist(lapply(cls, function(cl){
    deltas.cl = deltas[.class == cl,]
    fJ1.cl = fJ1[.class == cl,]
    fJ2.cl = fJ2[.class == cl,]
    dd = as.matrix(dcast(deltas.cl, .level ~ .num, value.var = ".ale", drop = FALSE))[,-1]
    dd = dd  - outer(fJ1.cl$.ale1,rep(1,nrow(fJ2.cl))) - outer(rep(1,nrow(fJ1.cl)),fJ2.cl$.ale2)
    sum(cell.counts *(dd[,1:(ncol(dd)-1)] + dd[,2:ncol(dd)])/2, na.rm = TRUE)/sum(cell.counts)
  }))
  
  fJ0 = data.frame(.fJ0 = fJ0, .class = cls)
  deltas = merge(deltas, fJ0, by = c(".class"))
  deltas = merge(deltas, fJ1, by = c(".class", ".level"))
  deltas = merge(deltas, fJ2, by = c(".class", ".num"))
  deltas = deltas[, .ale := .ale - .ale1 - .ale2 - .fJ0]
  
  # For later plotting, define the rectangles
  # These are not the same as the cells, which is a bit counterintuitive
  # each value in fJ is where the cells cross
  # instead of coloring the cells, we color a rectangle around each cell cross point
  # and for this we need to compute rectangles around these cross points
  # in image() this happens automatically (see ALEPlot::ALEPlot)
  # for the edges, simply use the grid value as the outer values

  deltas$.right = 1 + deltas$.level + 0.5
  deltas$.left = 1 + deltas$.level - 0.5
  interval.dists2 = diff(grid.dt[c(1, 1:nrow(grid.dt), nrow(grid.dt))][[1]])
  interval.dists2 = 0.5 *  interval.dists2
  deltas$.bottom = grid.dt[deltas$.num + 1, ] + interval.dists2[deltas$.num + 2]
  deltas$.top = grid.dt[deltas$.num + 1, ] - interval.dists2[deltas$.num + 1]
  deltas[,feature.name[x.num.index]] =  grid.dt[deltas$.num + 1, ]
  deltas[,feature.name[x.cat.index]] =  factor(levels.ordered[deltas$.level + 1], levels = levels.ordered)
  
  deltas = deltas[, setdiff(colnames(deltas), c(".fJ0", ".ale1", ".ale2", ".count", 
    ".level", ".num")), with = FALSE]
  deltas$.type = "ale"
  
  data.frame(deltas)
}




# by default assumes first column of cell.dat is x1 and second is x2
# leave grid1 NULL if feature x1 is a factor
# the difference variable has to be named .yhat.diff
impute_cells = function(cell.dat, grid1 = NULL, grid2, x1.ind = 1, x2.ind = 2){
  assert_data_table(cell.dat)
  assert_data_table(grid1, null.ok = TRUE)
  assert_data_table(grid2)
  d.miss.ind = is.na(cell.dat$.yhat.diff)
  if(!any(d.miss.ind)) return(cell.dat)
  
  if(is.null(grid1)) {
    range.x1 = length(unique(cell.dat[[x1.ind]]))
    x1.normalized = cell.dat[[x1.ind]] / range.x1
  } else {
    range.x1 = max(grid1[[1]]) - min(grid1[[1]])
    x1.normalized = (grid1[cell.dat[[x1.ind]],][[1]] + grid1[cell.dat[[x1.ind]] + 1,][[1]]) / (2 * range.x1)
  }
  range.x2 =  max(grid2[[1]]) - min(grid2[[1]])
  x2.normalized =   (grid2[cell.dat[[x2.ind]],][[1]] + grid2[cell.dat[[x2.ind]] + 1,][[1]]) / (2 * range.x2)
  
  z.na = cbind(x1.normalized[d.miss.ind], x2.normalized[d.miss.ind])
  z.non.na = cbind(x1.normalized[!d.miss.ind], x2.normalized[!d.miss.ind])
  
  nbrs <- yaImpute::ann(as.matrix(z.non.na), as.matrix(z.na), k=1, verbose = FALSE)$knnIndexDist[,1] 
  cell.dat[d.miss.ind, ".yhat.diff"] = cell.dat[which(!d.miss.ind)[nbrs], .yhat.diff] 
  cell.dat
}


