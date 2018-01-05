library('mlr')
library('dplyr')
library('ggplot2')
library('digest')
library('iml')


X = iris[-which(names(iris) == 'Species')]
y = iris$Species

## Generate the task
task = makeClassifTask(data = iris, target = "Species")

## Generate the learner
lrn = makeLearner("classif.randomForest", predict.type = 'prob')

## Train the learner
mod = train(lrn, task)

f.res = function(X){
  res = predict(mod, newdata = X)
  getPredictionProbabilities(res)
}

f = function(X){
  f.res(X)[,1]
}

x = prediction.model(f.res, multi.class = TRUE)
p =  x$predict(X[10:20,])
p

x = prediction.model(f.res, class = 1)
p =  x$predict(X[10:20,])
p

x = prediction.model(mod, class = 1)
p =  x$predict(X[10:20,])
p
x

data('Boston', package  = 'MASS')
Boston$chas = factor(Boston$chas, levels = c(0,1), labels = c('a', 'b'))
Boston$rad = factor(Boston$rad)
X = Boston[-which(names(Boston) == 'medv')]
y = Boston$medv

## Generate the task
task = makeRegrTask(data = Boston, target = "medv")

## Generate the learner
lrn = makeLearner("regr.randomForest")

## Train the learner
mod = train(lrn, task)

f = function(X){
  predict(mod, newdata = X)$data$response
}





## PDP
pdp(object = mod, X=X, feature = c(1, 2))  

pdp(mod, X=X, feature = c(9, 2))  
pdp(mod, X=X, feature = c(9, 4))  



pdp1 = pdp(f = f, X=X, feature = c(9,4), grid.size  = 10)  
plot(pdp1) 
pdp1$grid.size

pdp1$plot()
plot(pdp1)

pdp1$feature = 5
pdp1$plot() 

## ICE

ice(object = mod, X=X, feature = 4)

ice1 = ice(mod, X=X, feature = 1)
ice1$plot()

ice1$data()
ice1$feature = 1
ice1$plot()

## ICE centered
ice.c(f = f, X=X, feature = 1, anchor = 0)



ice1.c = ice(mod, X=X, feature = 1, center.at = 10)
ice1.c$plot()
ice1.c$data()
ice1.c$feature = 2
ice1.c$plot()
ice1.c$anchor = 50
ice1.c$plot()



## LIME

i = 120
x.interest = X[i,]

lime(mod, X,  1000, x.interest=x.interest)

lime1 = lime(f, X,  1000)
lime1$x <- x.interest
lime1$data()

lime1$x <- X[i+1,]
lime1$run(rerun = TRUE)$print()


## Shapley
shapley(mod, X, x.interest, 100)
shapley1 = shapley(f, X, x.interest, 100, class=2)

shapley1

shapley1$x = X[i+2,]
shapley1

## Permutation feature importance
perm.imp(mod, X, feature.index = 4, y=(y=='virginica'))
perm.imp(f, X, feature.index = 4,  y=(y=='virginica'))

permimp = perm.imp(mod, X, feature.index = 4,  y=(y=='virginica'))
#permimp = perm.imp(f, X, feature.index = 4, y=(y=='virginica'))

permimp$plot()
permimp$data()
permimp$run(force=TRUE)$data()

## Sobol (first order)
set.seed(1)
sobol(f, X, sample.size = 10000)
set.seed(1)
sobol(mod, X, sample.size = 10000)

sobol1 = sobol(f, X, sample.size = 10000)
sobol1$data()
sensitivity::soboljansen(f, X1 = sobol1$X.sample$X1, X2=sobol1$X.sample$X2)

## Sobol (total)
sobol(f, X, sample.size = 100000, type = 'total')
sobol2 = sobol(f, X, sample.size = 100000, type = 'total')

sobol2$data()
sobol2$plot()


## tree surrogate model, centered
tree.surrogate(f, X, 10000)
tree = tree.surrogate(f, X)
tree$plot()
