library('mlr')
library('dplyr')
library('ggplot2')
library('digest')
library('iml')
library('caret')


X = iris[-which(names(iris) == 'Species')]
y = iris$Species

## Generate the task
task = makeClassifTask(data = iris, target = "Species")

## Generate the learner
lrn = makeLearner("classif.randomForest", predict.type = 'prob')

## Train the learner
#mod = train(lrn, task)
mod = randomForest::randomForest(Species ~ ., data= iris)

mod <- caret::train(Species ~ ., data = iris, method = "knn",trControl = caret::trainControl(method = "cv"))


## PDP
pdp.obj = pdp(object = mod, X=X, feature = c(1, 3), grid.size = 50)  
pdp.obj
pdp.obj$data()

plot(pdp.obj) + theme_bw()


plot(pdp.obj) + scale_fill_continuous(low='white', high='red')


pdp.obj$feature = c(3,4)
pdp.obj$plot()

pdp.obj$feature = 1
pdp.obj$plot()


pdp(mod, X, feature = 1, class = 2)$plot()

## ICE
ice(object = mod, X=X, feature = 2, predict.args = list(type='prob'))$plot()  

ice1 = ice(mod, X=X, feature = 2, predict.args = list(type='prob'))  
plot(ice1)

ice1$data()
ice1$feature = 3
ice1$plot()

## ICE centered
ice1 = ice(mod, X=X, feature = 1, center.at = 4)
ice1$plot()


ice1 = ice(mod, X=X, feature = 1, center.at = 4, class = 2)
plot(ice1)
ice1$center.at = 4
plot(ice1)



## LIME

i = 121
x.interest = X[i,]

lime(mod, X,  1000, x.interest=x.interest, predict.args = list(type = 'prob'), class = 1)

lime1 = lime(mod, X,  1000, x.interest=x.interest, predict.args = list(type = 'prob'))
dat = lime1$data()

lime1$x <- X[i+1,]
lime1

## Shapley
shapley(mod, X, x.interest, 100, predict.args = list(type = 'prob'), class = 3)
shapley1 = shapley(mod, X, x.interest, 100, class=NULL, predict.args = list(type = 'prob'))
shapley1$x = X[i+2,]
plot(shapley1)
shapley1

## Permutation feature importance
importance(mod, X,  y=1*(y=='virginica'), predict.args = list(type = 'prob'), class = 3, loss = 'mae')$data()
pimp = importance(mod, X,  y = y, loss = 'ce')
pimp$data()

pimp = importance(mod, X,  y = y, loss = 'ce', method = 'cartesian')
pimp$data()
pimp$plot()

importance(mod, X,  y=y, predict.args = list(type = 'prob'),  loss = 'mae')

library("randomForest")
data("Boston", package  = "MASS")
mod = randomForest(medv ~ ., data = Boston, ntree = 50)
pimportance = importance(mod, Boston[which(names(Boston) != 'medv')], y = Boston$medv, loss = 'mae', method = 'cartesian')

pimp$data()
pimp$plot()


## tree surrogate model, centered
library("randomForest")
data("Boston", package  = "MASS")
mod = randomForest(medv ~ ., data = Boston, ntree = 50)

tree = tree.surrogate(mod, Boston[which(names(Boston) != 'medv')], 100, maxdepth = 4)

mod = randomForest(Species ~ ., data = iris, ntree = 50)

tree = tree.surrogate(mod, iris[which(names(iris) != 'Species')], 100, 
  maxdepth = 2, predict.args = list(type = 'prob'))

plot(tree)

print(tree)
tree$data()

# get.tree.data 
# Returns: data.frame with nodes
dat = X
# alternative: tree$data()$where, but works only for training data
tree.str = tree$data()

