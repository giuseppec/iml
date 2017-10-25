library(profvis)
# Implementation of paper: A General Method for Visualizing and Explaining Black-Box Regression Models
# from Erik Strumbelj and Igor Kononenko
## Generate or get data
source('~/repos/xai-book/src/initialize.R')
source('~/repos/xai-book/src/get_bike_sharing_dataset.R')

## Fit model
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task.train)
pred = predict(mod.bike, bike.task.train)
head(pred$data)

predict.fun = function(dat){
  predict(mod.bike, newdata = dat)$data$response
}

predict.fun(getTaskData(bike.task.train))

## Implement shapley
shapley = function(instance.x, f, dat, n.runs=100){
  # TODO: Remove instance.x from dat
  stopifnot(names(instance.x) == names(dat))
  n.features = ncol(instance.x)
  feature.names = names(dat)
  
  ## Assemble datasets and push all through predict.fun
  # Dimensions: with/without k, feature, sample size
  # Size              2 * K *  m x K
  # create two datasets 
  
  runs = lapply(1:n.runs, function(m){
    # randomly order features
    new.feature.order = sample(1:n.features)
    # randomly choose sample instance from dat
    sample.instance.shuffled = dat[sample(1:nrow(dat), 1), new.feature.order]
    instance.x.shuffled = instance.x[new.feature.order]
    
    lapply(1:n.features, function(k){
      k.at.index = which(new.feature.order == k)
      instance.with.k = instance.x.shuffled
      if(k.at.index < ncol(instance.x)){
        instance.with.k[(k.at.index + 1):ncol(instance.with.k)] = 
          sample.instance.shuffled[(k.at.index + 1):ncol(instance.with.k)]
      }
      instance.without.k = instance.with.k
      instance.without.k[k.at.index] = sample.instance.shuffled[k.at.index]
      cbind(instance.with.k[feature.names], instance.without.k[feature.names])
    }) %>% data.table::rbindlist()
    
  }) %>% data.table::rbindlist()
  dat.with.k = data.frame(runs[,1:(ncol(runs)/2)])
  dat.without.k = data.frame(runs[,(ncol(runs)/2):ncol(runs)])
  
  # given by the outcome of the two lapply
  res.frame = data.table::data.table(feature = rep(feature.names, times = n.runs), 
                                     run = rep(1:n.runs, each = n.features))
  res.frame$predictions.with.k = predict.fun(dat.with.k)
  res.frame$predictions.without.k = predict.fun(dat.without.k)
  res.frame$pred.diff = res.frame$predictions.with.k - res.frame$predictions.without.k
  #res.frame[,list(shapley.value = mean(pred.diff)),by=feature]
  res.frame
}

## Test out
dat = getTaskData(bike.task.train, target.extra=TRUE)$data
instance.x = dat[24,]
instance.x
predict.fun(instance.x)
mean(predict.fun(dat))
predict.fun(instance.x) - mean(predict.fun(dat))
dat.dist = dat[2:nrow(dat),]

res =  shapley(instance.x, predict.fun, dat.dist, n.runs=200)
res[,list(shapley.value = mean(pred.diff), m = run),by=feature]

phi.by.runs.l = res[,list(shapley.value = cummean(pred.diff), m = run),by=feature]

ggplot(phi.by.runs.l) + geom_line(aes(y = shapley.value, group = feature, color = feature, x = m))

ggplot(phi.by.runs.l) + 
  geom_line(aes(y = shapley.value, group = feature, color = feature, x = m)) + 
  facet_wrap('feature')


#plot(cummean(rowSums(res)))
colMeans(res)
res2 =  shapley.blocked(instance.x, predict.fun, dat.dist, n.runs=10000)
res2[,list(shapley.value = mean(pred.diff)),by=feature]

# IDEA: Function to compare to predictions, based on shapley


compare.pred.shapley = function(instance.ground, 
                                instance.compare, 
                                f, 
                                n.runs){
  contributions = shapley(instance.compare, f, instance.ground, n.runs)[,list(shapley.value = mean(pred.diff)),by=feature]
  data.frame(features = names(instance.ground), 
             values.ground = unlist(lapply(instance.ground[1,,drop=TRUE], as.character)),
             values.compare = unlist(lapply(instance.compare[1,,drop=TRUE], as.character)),
             contribution = contributions$shapley.value)
}

compare.pred.shapley(dat.dist[1,], instance.x, predict.fun, 50)



library(archetypes)
dat.a = na.omit(model.matrix(cnt ~ ., data = bike.train))
dat.a = dat.a[,2:ncol(dat.a)]
aa<-stepArchetypes(dat.a, k=1:10, nrep=5)
screeplot(aa)
rss(aa)


a3 = archetypes(dat.a, k = 3)
head(a3)
a3$archetypes




# Find archetype from dataset
a1.id = which(a3$alphas[,1] == max(a3$alphas[,1]))
archetype1 = dat[a1.id,]
res1 = shapley(archetype1, predict.fun, dat = dat)
res1[,list(shapley.value = mean(pred.diff)),by=feature]

# Find archetype from dataset
a2.id = which(a3$alphas[,2] == max(a3$alphas[,2]))
archetype2 = dat[a2.id,]
res2 = shapley(archetype2, predict.fun, dat = dat)
res2[,list(shapley.value = mean(pred.diff)),by=feature]


# Find archetype from dataset
a3.id = which(a3$alphas[,3] == max(a3$alphas[,3]))
archetype3 = dat[a3.id,]
res3 = shapley(archetype3, predict.fun, dat = dat)
res3[,list(shapley.value = mean(pred.diff)),by=feature]




