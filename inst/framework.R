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


## PDP
pdp = PDP$new(f = f, X=X, feature.index = 4)
pdp$conduct()$plot()


## ICE
ice = ICE$new(f = f, X=X, feature.index = 4)
ice$conduct()$plot()


## ICE centered
ice.c = ICE.centered$new(f = f, X=X, feature.index = 4, anchor = 1.5)
ice.c$conduct()$plot()


## LIME
i = 120
x.interest = X[i,]

lime = LIME$new(f, X,  1000)
lime$x <- x.interest
lime$conduct()$summary()

lime$x <- X[i+1,]
lime$conduct()$print()


## Shapley
shapley = Shapley$new(f, X, x.interest, 100)
shapley$conduct()
shapley$data()

## Permutation feature importance
permimp = PermImp$new(f, X, feature.index = 4, y=(y=='virginica'))
permimp$conduct()$data()


## Sobol (first order)
sobol = Sobol$new(f, X, sample.size = 10000)
sobol$conduct()$data()
sensitivity::soboljansen(f, X1 = sobol$X.sample$X1, X2=sobol$X.sample$X2)

## Sobol (total)
sobol = Sobol$new(f, X, sample.size = 1000, type = 'total')
sobol$conduct()$data()



## tree surrogate model, centered
trees = TreeSurrogate$new(f, X)
trees$conduct()$plot()
