# framework-shapley

library('mlr')
library('dplyr')
library('ggplot2')
library('digest')

X = iris[-which(names(iris) == 'Species')]
y = iris$Species


background = X

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


weight.samples.generic = function(X, ...){
  rep(1, times = nrow(X))
}



id = function(X, ...){
  X
}

explain = function(f, generate.fun, intervene, aggregate, display = id, weight.samples = weight.samples.generic,  ...){
  ## intervention
  ## Maybe generate.fun should not be called within intervene, but before. Then it is easier to generalise 
  ## the Quantity of interest function Q(X.intervention, X, ...) that goes into the aggregation. 
  ## But maybe quantity of interest does not need to be seperate, because mostly its deeply intertwined with aggregate
  X.intervention = intervene(generate.fun, ...)
  ## predict 
  y.hat = f(X = X.intervention)
  w = weight.samples(X.intervention, ...)
  ## aggregate
  res = aggregate(X=X.intervention, y.hat = y.hat, w = w,...)
  ## explain
  display(res)
}

## PDP
res = explain(f=f, generate=generate.pdp, intervene=intervene.pdp, 
              aggregate = aggregate.pdp, display = display.pdp, feature.index = 4, grid.size = 50, n = 1000)
print(res)

## ICE
res = explain(f=f, generate=generate.pdp, intervene=intervene.ice, 
              aggregate = aggregate.ice, display = display.ice, feature.index = 4, grid.size = 20, n = 1000)
print(res)

## ICE, centered
res = explain(f=f, generate=generate.ice, intervene=intervene.ice, 
              aggregate = aggregate.ice.centered, display = display.ice, feature.index = 4, grid.size = 20, n = 1000, anchor = 0)

print(res)

## ICE with shapley
res = explain(f=f, generate=generate.pdp, intervene=intervene.ice, 
              aggregate = aggregate.shapley, display = display.ice, feature.index = 4, grid.size = 20, n = 1000)
print(res)

## LIME
i = 120
x.interest = background[i,]

res =  explain(f=f, generate=generate.lime, intervene=intervene.lime, 
               aggregate= aggregate.lime, display = display.lime, 
               n = 1000, x.interest = x.interest, weight.samples = weight.samples)
print(res)


## Shapley
res =  explain(f=f, generate=generate.shapley, intervene=intervene.shapley, 
               aggregate = aggregate.shapley, x.interest = background[i,], n = 100)
res


## Permutation feature importance 
res =  explain(f=f, generate=generate.permimp, intervene=intervene.permimp, 
               aggregate = aggregate.permimp, n = 1000, y=(y=='virginica'), feature.index=4)
res


## Sobol (first order)
res =  explain(f=f, generate=generate.sobol, intervene=intervene.sobol, 
               aggregate = aggregate.sobol.first, n = 500)
res
## compare with sensitivty implementation
sensitivity::sobol(f, X1 = generate.sobol(n), X2=generate.sobol(n), order=1)


## Sobol (total)
res =  explain(f=f, generate=generate.sobol, intervene=intervene.sobol, 
               aggregate = aggregate.sobol.total, n = 1500)
res
n=100
soboljansen(f, X1 = generate.sobol(n), X2=generate.sobol(n))

soboljansen(f, X1 = generate.sobol(n), X2=generate.sobol(n))




