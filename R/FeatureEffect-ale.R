# for suppressing R CMD NOTE:  
.SD = .ale = .ale0 = .ale1 = .ale2 = .class = .count = .interval = .interval1 = 
  .interval2 = .level = .num = .y.hat = .y.hat.cumsum = .yhat.diff = NULL


#' Compute ALE for 1 numerical feature
#' 
#' @param dat the data.frame with same columns as training data
#' @param run.prediction Predict function of type: f(newdata)
#' @param feature.name The column name of the feature for which to compute ALE
#' @param grid.size The number of intervals
calculate.ale.num = function(dat, run.prediction, feature.name, grid.size){
  # from number of intervals to number of borders
  n.borders = grid.size + 1
  # Handling duplicated grid values
  grid.dt = unique(get.grid(dat[,feature.name, with = FALSE], n.borders, type = "quantile"))
  # Matching data instances to intervals
  interval.index = findInterval(dat[[feature.name]], grid.dt[[1]], left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index[interval.index == 0] = 1
  # Getting the predictions for instances of upper and lower interval limits
  X.lower = X.upper = dat
  X.lower[,feature.name] = grid.dt[interval.index,]
  X.upper[,feature.name] = grid.dt[interval.index + 1,]
  predictions.lower = run.prediction(X.lower)
  predictions.upper =  run.prediction(X.upper)
  # finite differences
  prediction.deltas = predictions.upper - predictions.lower
  deltas = cbind(X.lower[,feature.name, with=FALSE], prediction.deltas, data.frame(".interval" = interval.index))
  y.hat.names = setdiff(colnames(deltas), c(colnames(dat), ".interval"))
  deltas = melt(deltas, variable.name = ".class", 
    value.name = ".yhat.diff", measure.vars = y.hat.names)
  # average over instances within each interval
  setkeyv(deltas, c(".class", ".interval"))
  deltas = deltas[, list(.yhat.diff = mean(.yhat.diff)), by = c(".interval", ".class")]
  # accumulate over the intervals
  deltas.accumulated = deltas[,list(.y.hat.cumsum = cumsum_na(c(0, .yhat.diff))), by = ".class"]
  interval.n.dat = as.numeric(table(interval.index))
  # the mean effect is the weighted mean of the interval mid point effects
  # weighted by the number of points in the interval
  fJ0 =  deltas.accumulated[, list(.ale0 = sum(((deltas.accumulated$.y.hat.cumsum[1:(nrow(.SD) - 1)] + 
      deltas.accumulated$.y.hat.cumsum[2:nrow(.SD)])/2) * interval.n.dat)/sum(interval.n.dat)), 
    by = ".class"]
  # centering the ALEs
  deltas.accumulated = merge(deltas.accumulated, fJ0, all.x = TRUE, by = ".class")
  fJ = deltas.accumulated[, list(.ale = .y.hat.cumsum - .ale0, .id = 1:nrow(.SD), .type = "ale"), 
    by = ".class"]
  grid.dt$.id = 1:nrow(grid.dt)
  fJ = merge(fJ, grid.dt, by = ".id")
  # adding the centering constant to the output
  # fJ0$.type = "ale0"
  # fJ0$.ale = fJ0$.ale0
  # fJ0$.ale0 = NULL
  # fJ = rbind(fJ, fJ0, fill = TRUE)
  fJ$.id = NULL
  data.frame(fJ)
}


#' Compute ALE for 2 numerical features
#' 
#' @param dat the data.frame with same columns as training data
#' @param run.prediction Predict function of type: f(newdata)
#' @param feature.name The column names of the feature for which to compute ALE
#' @param grid.size The number of cells
calculate.ale.num.num = function(dat, run.prediction, feature.name, grid.size){
  # Create grid for feature 1
  grid.dt1 = unique(get.grid(dat[,feature.name[1], with = FALSE], grid.size = grid.size[1] + 1, type = "quantile"))
  colnames(grid.dt1)= feature.name[1]
  # Matching instances to the grid of feature 1
  interval.index1 = findInterval(dat[[feature.name[1]]], grid.dt1[[1]] , left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index1[interval.index1 == 0] = 1
  ## Create grid for feature 2
  grid.dt2 = unique(get.grid(dat[,feature.name[2], with = FALSE], grid.size  =grid.size[2] + 1, type = "quantile"))
  colnames(grid.dt2)= feature.name[2]
  # Matching instances to the grid of feature 2
  interval.index2 = findInterval(dat[[feature.name[2]]], grid.dt2[[1]], left.open = TRUE)
  # Data point in the left most interval should be in interval 1, not zero
  interval.index2[interval.index2 == 0] = 1
  # Preparation for predictions of cell corners
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
  # Second order differences per cell and instance
  # (top right corner - top left corner) - (bottom right corner  - bottom left corner)
  predictions = (predictions.22 - predictions.21) - (predictions.12 - predictions.11) 
  deltas = cbind(dat[,feature.name, with=FALSE], predictions, data.frame(".interval1" = interval.index1, ".interval2" = interval.index2))
  y.hat.names = setdiff(colnames(deltas), c(colnames(dat), c(".interval1", ".interval2")))
  # instead of a matrix, we work with melted version of the data
  # This allows us to work with multi-dimensional predictions
  deltas = melt(deltas, variable.name = ".class", 
    value.name = ".y.hat", measure.vars = y.hat.names)
  # Make sure all intervals are included
  interval_grid = expand.grid(.interval1 = unique(deltas$.interval1), .interval2 = unique(deltas$.interval2),
    .class = unique(deltas$.class))
  deltas = merge(deltas, interval_grid, on = c(".interval1", ".interval2", ".class"), all.y = TRUE)
  # Average over all instances within a cell
  setkeyv(deltas, c(".class", ".interval1", ".interval2"))
  deltas = deltas[, list(.yhat.diff = mean(.y.hat)), by = c(".interval1", ".interval2", ".class")]
  
  
  # fill empty cells with the closest neighbour cells value
  # remember cell status for later
  deltas.na.cell = copy(deltas)
  deltas.na.cell$missing = is.na(deltas.na.cell$.yhat.diff)
  deltas.na.cell$.yhat.diff = NULL
  # replace the missing ones with the closest non-missing difference (measured in number of intervals)
  deltas = rbindlist(lapply(unique(deltas$.class), function(cl) {
    impute_cells(deltas[.class == cl, ], grid1 = grid.dt1, grid2 = grid.dt2, 
      x1.ind = ".interval1", x2.ind = ".interval2")
  }))
  
  # Acumulate the predictions from bottom left to top right 
  ale = deltas[,list(.y.hat.cumsum = cumsum_na(c(0, .yhat.diff)), .interval2 = c(0, .interval2)), by = c(".class", ".interval1")]
  ale = ale[,list(.y.hat.cumsum = cumsum_na(c(0, .y.hat.cumsum)), .interval1 = c(0, .interval1)), by = c(".class", ".interval2")]
  # Number of cells are need for weighting later
  cell.counts = as.matrix(table(interval.index1, interval.index2))
  cell.counts.m = melt(cell.counts)
  colnames(cell.counts.m) = c(".interval1", ".interval2",  ".count")
  cell.counts.m$.interval1 = as.numeric(as.character(cell.counts.m$.interval1))
  cell.counts.m$.interval2 = as.numeric(as.character(cell.counts.m$.interval2))
  ale = merge(ale, cell.counts.m, on = c(".interval1", ".interval2"), all.x = TRUE)
  
  # Computing the first-order effect of feature 1
  # First, take the differences across feature 1
  ale1 = ale[, list(.yhat.diff = .y.hat.cumsum[2:nrow(.SD)] - .y.hat.cumsum[1:(nrow(.SD) - 1)], .interval1 = .interval1[2:(nrow(.SD))], .count = .count[2:(nrow(.SD))]),
    by = c(".class", ".interval2")]
  # Then take the prediction at the mid point of each interval, which is the mean of the prediction at the end points
  #     and take calculate the mean, weighted by the number of data instances per cell
  ale1 = ale1[, list(.ale1 = sum(.count[2:nrow(.SD)] * (.yhat.diff[1:(nrow(.SD) - 1)] + 
      .yhat.diff[2:(nrow(.SD))]) / 2) / sum(.count[2:nrow(.SD)])), by = c(".class", ".interval1")]
  ale1 = ale1[, list(.ale1 = c(0, cumsum_na(.ale1)), .interval1 = c(0, .interval1)), by = c(".class")]
  # Computing the first-order effect of feature 2
  # First, take the differences across feature 2
  ale2 = ale[, list(.yhat.diff = .y.hat.cumsum[2:nrow(.SD)] - .y.hat.cumsum[1:(nrow(.SD) - 1)], .interval2 = .interval2[2:(nrow(.SD))], .count = .count[2:(nrow(.SD))]),
    by = c(".class", ".interval1")]
  # Then take the prediction at the mid point of each interval, which is the mean of the prediction at the end points
  #     and take calculate the mean, weighted by the number of data instances per cell
  ale2 = ale2[, list(.ale2 = sum(.count[2:nrow(.SD)] * (.yhat.diff[1:(nrow(.SD) - 1)] + 
      .yhat.diff[2:(nrow(.SD))]) / 2) / sum(.count[2:nrow(.SD)])), by = c(".class", ".interval2")]
  ale2 = ale2[, list(.ale2 = c(0, cumsum_na(.ale2)), .interval2 = c(0, .interval2)), by = ".class"]
  # for each cells computes the average prediction through mean of the cell corners
  # then again a mean over all cells, weighted by the number of data points in the cell
  cls = unique(ale$.class)
  fJ0 = unlist(lapply(cls, function(cl){
    ale.cl = ale[.class == cl,]
    ale1.cl = ale1[.class == cl,]
    ale2.cl = ale2[.class == cl,]
    dd = as.matrix(dcast(ale.cl, .interval1 ~ .interval2, value.var = ".y.hat.cumsum", drop = FALSE))[,-1]
    dd = dd  - outer(ale1.cl$.ale1,rep(1,nrow(ale2.cl))) - outer(rep(1,nrow(ale1.cl)),ale2.cl$.ale2)
    sum(cell.counts *(dd[1:(nrow(dd)-1),1:(ncol(dd)-1)] + dd[1:(nrow(dd)-1),2:ncol(dd)] + dd[2:nrow(dd),1:(ncol(dd)-1)] + dd[2:nrow(dd), 2:ncol(dd)])/4, na.rm = TRUE)/sum(cell.counts)
  }))
  fJ0 = data.frame(".fJ0" = fJ0, .class = cls)
  ale = merge(ale, fJ0, by = c(".class"))
  ale = merge(ale, ale1, by = c(".class", ".interval1"))
  ale = merge(ale, ale2, by = c(".class", ".interval2"))
  ale$.ale =  ale$.y.hat.cumsum - ale$.ale1 - ale$.ale2 - ale$.fJ0
  # For later plotting, define the rectangles
  # These are not the same as the cells, which is a bit counterintuitive
  # each value in fJ is where the cells cross
  # instead of coloring the cells, we color a rectangle around each cell cross point
  # and for this we need to compute rectangles around these cross points
  # in image() this happens automatically (see ALEPlot::ALEPlot)
  # for the edges, simply use the grid value as the outer values
  interval.dists = diff(grid.dt1[c(1, 1:nrow(grid.dt1), nrow(grid.dt1))][[1]])
  interval.dists = 0.5 *  interval.dists
  ale$.right = grid.dt1[ale$.interval1 + 1, ] + interval.dists[ale$.interval1 + 2]
  ale$.left = grid.dt1[ale$.interval1 + 1, ] - interval.dists[ale$.interval1 + 1]
  interval.dists2 = diff(grid.dt2[c(1, 1:nrow(grid.dt2), nrow(grid.dt2))][[1]])
  interval.dists2 = 0.5 *  interval.dists2
  ale$.bottom = grid.dt2[ale$.interval2 + 1, ] + interval.dists2[ale$.interval2 + 2]
  ale$.top = grid.dt2[ale$.interval2 + 1, ] - interval.dists2[ale$.interval2 + 1]
  ale[,feature.name[1]] =  grid.dt1[ale$.interval1 + 1, ]
  ale[,feature.name[2]] =  grid.dt2[ale$.interval2 + 1, ]
  
  ale = ale[, setdiff(colnames(ale), c(".fJ0", ".ale1", ".ale2", ".y.hat.cumsum", ".count", 
    ".interval1", ".interval2")), with = FALSE]
  ale$.type = "ale"
  
  data.frame(ale)
}


#' Compute ALE for 1 categorical feature
#' @param dat the data.frame with same columns as training data
#' @param run.prediction Predict function of type: f(newdata)
#' @param feature.name The column name of the feature for which to compute ALE
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
  deltas = cbind(deltas, ".level.jump" = c(x.ordered[row.ind.increase], x.ordered[row.ind.decrease] - 1))
  y.hat.names = colnames(y.hat)
  deltas = data.table(melt(deltas, id.vars = c(".level.jump"), variable.name = ".class", value.name = ".yhat.diff"))
  
  # All those difference are aggregated (averaged) grouped by the jump between levels (and by .class for multi output)
  setkeyv(deltas, ".level.jump")
  deltas = deltas[,list(.yhat.diff = mean(.yhat.diff)),by = c(".class", ".level.jump")]
  # We accumulate the jumps, starting at 0 for the first category
  deltas = deltas[, list(.ale = c(0, cumsum(.yhat.diff))), by = c(".class")]
  # Results are centered by the mean interval effect, weighted by number of instances in the interval
  x.count = as.numeric(table(x))
  x.prob = x.count/sum(x.count) 
  deltas = deltas[, list(.ale = .ale - sum(.ale * x.prob[level_order]), .level = levels.ordered), by = ".class"]
  colnames(deltas) = c(".class", ".ale", feature.name)
  deltas[, feature.name] = factor(deltas[,feature.name,with=FALSE][[1]], levels = levels.ordered)
  # make sure the rows are ordered by the new level order
  data.frame(deltas[order(deltas[[feature.name]]),])
}


#' Compute ALE for 2 features, one numerical, one categorical
#' 
#' @param dat the data.frame with same columns as training data
#' @param run.prediction Predict function of type: f(newdata)
#' @param feature.name The column name of the features for which to compute ALE
#' @param grid.size The number of intervals for the numerical feature
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
  
  
  # Since a categorical feature is involved, we do the second-order differences twice
  # Once for the instances that jump from k to k+1 and once for those that jump from category k+1 to k
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
  
  # second-order differences for instances jumping from category k to k+1
  d.increase = (predictions.22  - predictions.21) - (predictions.12 - predictions.11)
  
  X.low1.low2 = X.up1.low2 = X.low1.up2 = X.up1.up2 = copy(dat)
  
  # put category as first dimension without loss of generality
  X.low1.low2[row.ind.decrease, feature.name[x.num.index]] = 
    grid.dt[interval.index,][[1]][row.ind.decrease]
  X.low1.low2[row.ind.decrease, feature.name[x.cat.index]] = 
    levels.ordered[x.cat.ordered[row.ind.decrease] - 1]
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
  
  # second-order differences for instances jumping from category k+1 to k
  d.decrease = (predictions.22  - predictions.21) - (predictions.12 - predictions.11)
  
  
  # Combine the deltas over k to k+1 and k+1 to k to the same jump
  deltas = rbind(d.increase, d.decrease)
  deltas = cbind(deltas, .level = c(x.cat.ordered[row.ind.increase], x.cat.ordered[row.ind.decrease] - 1), 
    .num = interval.index[c(row.ind.increase, row.ind.decrease)])
  y.hat.names = colnames(predictions.22)
  deltas = data.table(melt(deltas, measure.vars = y.hat.names, variable.name = ".class", value.name = ".yhat.diff"))
  # add empty cells
  interval_grid = expand.grid(.level = unique(deltas$.level), .num = unique(deltas$.num),
    .class = unique(deltas$.class))
  deltas = merge(deltas, interval_grid, on = c(".level", ".num", ".class"), all.y = TRUE)
  # Average results per cell
  setkeyv(deltas, c(".class", ".level", ".num"))
  deltas = deltas[ ,list(.yhat.diff = mean(.yhat.diff)), by = c(".class", ".level", ".num")]
  
  # fill empty cells with the closest neighbour cells value
  # remember cell status for later
  deltas.na.cell = copy(deltas)
  deltas.na.cell$missing = is.na(deltas$.yhat.diff)
  deltas.na.cell$.yhat.diff = NULL
  # replace the missing ones with the closest non-missing difference (measured in number of intervals)
  deltas = rbindlist(lapply(unique(deltas$.class), function(cl) {
    impute_cells(deltas[.class == cl, ], grid2 = grid.dt, 
      x1.ind = ".level", x2.ind = ".num")
  }))
  # Acumulate the predictions from bottom left to top right 
  deltas = deltas[, list(.ale = cumsum(c(0, .yhat.diff)), .num = c(0, .num)), by = c(".class", ".level")]
  deltas = deltas[, list(.ale = cumsum(c(0, .ale)), .level = c(0, .level)), by = c(".class", ".num")]
  
  # Number of cells are need for weighting later
  cell.counts = as.matrix(table(x.cat.ordered, interval.index))
  cell.counts.m = melt(cell.counts)
  colnames(cell.counts.m) = c(".level", ".num",  ".count")
  cell.counts.m$.level = as.numeric(as.character(cell.counts.m$.level)) - 1
  cell.counts.m$.num = as.numeric(as.character(cell.counts.m$.num)) 
  deltas = merge(deltas, cell.counts.m, on = c(".level", ".num"), all.x = TRUE)
  deltas$.count[is.na(deltas$.count)] =  0
  
  # Computing the first-order effect of feature 2, the numerical feature.
  # First, take the differences across feature 2
  deltas2 = deltas[, list(.ale2 = .ale[2:nrow(.SD)] - .ale[1:(nrow(.SD) - 1)], .num = .num[2:(nrow(.SD))], .count = .count[2:(nrow(.SD))]),
    by = c(".class", ".level")]
  # Then take the weighted average over the categories to get the effect at each numerical grid point.
  deltas2 = deltas2[, list(.ale2 = sum(.count * .ale2) / sum(.count)), by = c(".class", ".num")]
  ale2 = deltas2[, list(.ale2 = c(0, cumsum(.ale2)), .num = c(0, .num)), by = c(".class")]
  
  # Computing the first-order effect of feature 1, the categorical feature.
  # take the differences
  deltas1 = deltas[, list(.ale1 = .ale[2:nrow(.SD)] - .ale[1:(nrow(.SD) - 1)], .level = .level[2:(nrow(.SD))], 
    .count = (.count[2:nrow(.SD)] + .count[1:(nrow(.SD)-1)]) / 2), by = c(".class", ".num")]
  # mid points between numerical intervals
  deltas1 = deltas1[, list(
    .ale1 = (.ale1[2:nrow(.SD)] + .ale1[1:(nrow(.SD)-1)]) / 2,
    .count = .count[2:nrow(.SD)],
    .num = .num[2:nrow(.SD)]), by = c(".class", ".level")]
  deltas1  = deltas1[,list(.ale1 = sum(.ale1 * .count) / sum(.count)), by = c(".class", ".level")]
  ale1 = deltas1[, list(.ale1 = c(0, cumsum_na(.ale1)), .level = c(0, .level)), by = c(".class")]
  
  cls = unique(deltas$.class)
  fJ0 = unlist(lapply(cls, function(cl){
    deltas.cl = deltas[.class == cl,]
    ale1.cl = ale1[.class == cl,]
    ale2.cl = ale2[.class == cl,]
    dd = as.matrix(dcast(deltas.cl, .level ~ .num, value.var = ".ale", drop = FALSE))[,-1]
    dd = dd  - outer(ale1.cl$.ale1,rep(1,nrow(ale2.cl))) - outer(rep(1,nrow(ale1.cl)),ale2.cl$.ale2)
    sum(cell.counts *(dd[,1:(ncol(dd)-1)] + dd[,2:ncol(dd)])/2, na.rm = TRUE)/sum(cell.counts)
  }))
  
  fJ0 = data.frame(".fJ0" = fJ0, ".class" = cls)
  deltas = merge(deltas, fJ0, by = c(".class"))
  deltas = merge(deltas, ale1, by = c(".class", ".level"))
  deltas = merge(deltas, ale2, by = c(".class", ".num"))
  # remove first-order ALEs and center globally
  deltas$.ale = deltas$.ale - deltas$.ale1 - deltas$.ale2 - deltas$.fJ0
  
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



#' Impute missing cells of grid
#' 
#' by default assumes first column of cell.dat is x1 and second is x2
#' leave grid1 NULL if feature x1 is a factor
#' the difference variable has to be named .yhat.diff
#' 
#' @param cell.dat data.table with at least 4 columns: .yhat.diff and the two interval indices. 
#' Make sure that empty cells are also included and cell.dat is not the sparse representation.
#' @param grid1 data.frame where each row is the actual value for a given interval index for feature 1. 
#' If empty impute_cells  assumes that the feature is categorical (factor).
#' @param grid2 data.frame where each row is the actual value for a given interval index for feature 2
#' @param x1.ind column number or name of cell.dat for feature 1. If one feature is categorical, has to be x1
#' @param x2.ind column number or name of cell.dat for feature 2
impute_cells = function(cell.dat, grid1 = NULL, grid2, x1.ind = 1, x2.ind = 2){
  assert_data_table(cell.dat)
  assert_data_table(grid1, null.ok = TRUE)
  assert_data_table(grid2)
  # Making sure cell.dat contains all possible cells
  stopifnot(nrow(cell.dat) == length(unique(cell.dat[,x1.ind,with=FALSE][[1]])) * length(unique(cell.dat[,x2.ind,with=FALSE][[1]])))
  d.miss.ind = is.na(cell.dat$.yhat.diff)
  
  # We don't have to impute anything when all cells are missing.
  if(!any(d.miss.ind)) return(cell.dat)
  
  # Distinguishes for normalization between categorical and numerical feature x1
  if(is.null(grid1)) {
    # For categorical feature, the range is the number of levels, and
    # scaled to range 1/n.categories to 1
    range.x1 = length(unique(cell.dat[[x1.ind]]))
    x1.normalized = cell.dat[[x1.ind]] / range.x1
  } else {
    # For numerical feature range is from min to max
    # normalizing means taking the mean of neighbours and dividing by range
    range.x1 = max(grid1[[1]]) - min(grid1[[1]])
    x1.normalized = (grid1[cell.dat[[x1.ind]],][[1]] + grid1[cell.dat[[x1.ind]] + 1,][[1]]) / (2 * range.x1)
  }
  # same for the second numerical feature
  range.x2 =  max(grid2[[1]]) - min(grid2[[1]])
  x2.normalized =   (grid2[cell.dat[[x2.ind]],][[1]] + grid2[cell.dat[[x2.ind]] + 1,][[1]]) / (2 * range.x2)
  # preparation for yaImpute::ann function
  z.na = cbind(x1.normalized[d.miss.ind], x2.normalized[d.miss.ind])
  z.non.na = cbind(x1.normalized[!d.miss.ind], x2.normalized[!d.miss.ind])
  # matches closest non-missing neighbour
  nbrs <- yaImpute::ann(as.matrix(z.non.na), as.matrix(z.na), k=1, verbose = FALSE)$knnIndexDist[,1] 
  cell.dat[d.miss.ind, ".yhat.diff"] = cell.dat[which(!d.miss.ind)[nbrs], .yhat.diff] 
  cell.dat
}


