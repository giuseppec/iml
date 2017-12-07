# framework-shapley

library('mlr')
library('dplyr')
library('ggplot2')
library('digest')
library('iml')


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


generate.mc = get.generate.fun.mc(background)


## PDP
res = iml::explain(f=f, generate=generate.mc, intervene=intervene.pdp, 
              aggregate = aggregate.pdp, display = display.pdp, feature.index = 4, grid.size = 50, n = 1000)
print(res)

pdp = PDP$new(f = f, sampler = generate.mc, feature.index = 4)
pdp$conduct(, grid.size = 5, n = 100)$present()





## ICE
res = iml::explain(f=f, generate=generate.mc, intervene=intervene.ice, 
              aggregate = aggregate.ice, display = display.ice, feature.index = 4, grid.size = 20, n = 1000)
print(res)

## ICE, centered
res = iml::explain(f=f, generate=generate.mc, intervene=intervene.ice, 
              aggregate = aggregate.ice.centered, display = display.ice, feature.index = 4, grid.size = 20, n = 1000, anchor = 0)

print(res)

## LIME
i = 120
x.interest = background[i,]

res =  iml::explain(f=f, generate=generate.lime, intervene=intervene.lime, 
               aggregate= aggregate.lime, display = display.lime, 
               x.interest = x.interest, weight.samples = weight.samples.lime)
print(res)


## Shapley
res =  iml::explain(f=f, generate=generate.mc, intervene=intervene.shapley, 
               aggregate = aggregate.shapley, x.interest = background[i,], n = 100)
res


## Permutation feature importance 
res =  iml::explain(f=f, generate=generate.mc, intervene=intervene.permimp, 
               aggregate = aggregate.permimp, n = 1000, y=(y=='virginica'), feature.index=4)
res


## Sobol (first order)
res =  iml::explain(f=f, generate=generate.mc, intervene=intervene.sobol, 
               aggregate = aggregate.sobol.first, n = 500)
res
## compare with sensitivty implementation
sensitivity::sobol(f, X1 = generate.mc(n), X2=generate.mc(n), order=1)


## Sobol (total)
res =  iml::explain(f=f, generate=generate.mc, intervene=intervene.sobol, 
               aggregate = aggregate.sobol.total, n = 1500)
res
n=100
soboljansen(f, X1 = generate.sobol(n), X2=generate.sobol(n))

soboljansen(f, X1 = generate.sobol(n), X2=generate.sobol(n))



## tree surrogate model, centered
res = iml::explain(f=f, generate=generate.mc, intervene=intervene.lime, 
              aggregate = aggregate.surrogate, display = display.surrogate)

print(res)


