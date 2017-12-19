# framework-shapley

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

f = function(X){
  res = predict(mod, newdata = X)
  getPredictionProbabilities(res)[,3]
}



data('Boston', package  = 'MASS')
Boston$chas = factor(Boston$chas, levels = c(0,1), labels = c('a', 'b'))
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
pdp1 = generate.pdp(f = f, X=X, feature = 4)
pdp1$plot() + scale_y_continuous(limits = c(0, NA))
pdp1$feature = 5
pdp1$plot() 

## ICE
ice = ICE$new(f = f, X=X, feature = 4)
ice$plot()
ice$data()
ice$feature = 1
ice$plot()

## ICE centered
ice.c = ICE.centered$new(f = f, X=X, feature = 1, anchor = 0)
ice.c$plot()
ice.c$data()
ice.c$feature = 2
ice.c$plot()
ice.c$anchor = 50
ice.c$plot()



## LIME
i = 120
x.interest = X[i,]

lime = LIME$new(f, X,  1000)
lime$x <- x.interest
lime$data()
lime$run()$summary()

lime$x <- X[i+1,]
lime$run()$print()


## Shapley
shapley = Shapley$new(f, X, x.interest, 100)
shapley$run()
shapley$data()

## Permutation feature importance
#permimp = PermImp$new(f, X, feature.index = 4, y=(y=='virginica'))
permimp = PermImp$new(f, X, feature.index = 4, y=y)
permimp$plot()
permimp$data()
permimp$run(force=TRUE)$data()

## Sobol (first order)
sobol = Sobol$new(f, X, sample.size = 10000)
sobol$data()
sensitivity::soboljansen(f, X1 = sobol$X.sample$X1, X2=sobol$X.sample$X2)

## Sobol (total)
sobol = Sobol$new(f, X, sample.size = 100000, type = 'total')
sobol$data()
sobol$plot()


## tree surrogate model, centered
tree = TreeSurrogate$new(f, X)
tree$plot()
