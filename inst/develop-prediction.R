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

f.res = function(X){
  res = predict(mod, newdata = X)
  getPredictionProbabilities(res)
}


x = prediction.model(f.res, class = 1)

p =  x$predict(X[10:20,])
p
x