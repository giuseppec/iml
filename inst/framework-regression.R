# framework-shapley

library('mlr')
library('dplyr')
library('ggplot2')
library('digest')
library('iml')


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
