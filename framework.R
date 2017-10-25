
X = iris[-which(names(iris) == 'Species')]
y = iris$Species



## Generate the task
task = makeClassifTask(data = iris, target = "Species")

## Generate the learner
lrn = makeLearner("classif.rpart", predict.type = 'prob')

## Train the learner
mod = train(lrn, task)
mod

pred = predict(mod, newdata = iris)

getPredictionProbabilities(pred)

predict.fun = function(dat){
  res = predict(mod, newdata = dat)
  getPredictionProbabilities(res)[3]
}

predict.fun(iris)
# Implement permutation feature importance

# select background and instances of interest
background = X
interest = X


 
sample.background = function(X){
  return(X)
}

weight.background = function(background, interest){
  return(rep(1, times = nrow(background)))
}


# parametere feature.index is specific to variable importance measure
recombine.dat.once = function(background, interest, feature.index){
  interest[,feature.index] = sample(interest[,feature.index])
  interest
}
recombine.dat = function(background, interest, feature.index, n.times=100){
  rec = lapply(1:n.times, function(x){recombine.dat.once(background, interest, feature.index)})
  data.table::rbindlist(rec)
}

recombined = data.frame(recombine.dat(iris, iris, 4, 4))
recombined
xx = predict.fun(recombined)
table(xx)


