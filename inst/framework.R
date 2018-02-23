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

modCaret <- caret::train(Species ~ ., data = iris, method = "knn",trControl = caret::trainControl(method = "cv"))
mod = Model(modCaret, predict.args = list(type = "prob"))

## PDP
pdp.obj = PartialDependence$new(mod, data=X, feature = c(1, 3), grid.size = 20)  
pdp.obj
pdp.obj$results

plot(pdp.obj) + theme_bw()


plot(pdp.obj) + scale_fill_continuous(low='white', high='red')


pdp.obj$feature = c(3,4)
pdp.obj$plot()

pdp.obj$feature = 1
pdp.obj$plot()


PartialDependence$new(mod, X, feature = 1)$plot()

## ICE
Ice$new(mod, X, feature = 2)$plot()  

ice1 = Ice$new(mod, X, feature = 2)  
plot(ice1)

ice1$results
ice1$feature = 3
ice1$plot()

## ICE centered
ice1 = Ice$new(mod, X, feature = 1, center.at = 4)
ice1$plot()

mod2 = Model$new(modCaret, class = 2, predict.args = list(type = "prob"))
ice1 = Ice$new(mod2, X, feature = 1, center.at = 4)
plot(ice1)
ice1$center.at = 4
plot(ice1)



## LIME

i = 121
x.interest = X[i,]

Lime$new(mod2, X,  1000, x.interest=x.interest)

lime1 = Lime$new(mod, X,  1000, x.interest=x.interest)
dat = lime1$results
plot(lime1)
lime1$x <- X[i+1,]
lime1


lime1 = Lime$new(mod2, X,  1000, x.interest=x.interest)
lime1
plot(lime1)

## Shapley
Shapley$new(mod2, X, x.interest, 100)
shapley1 = Shapley$new(mod, X, x.interest, 100)
shapley1$x = X[i+2,]
plot(shapley1)
shapley1

## Permutation feature importance
FeatureImp$new(mod2, X,  y=1*(y=='virginica'), loss = 'mae')$results
pimp = FeatureImp$new(mod, X,  y = y, loss = 'ce')
pimp$results

pimp = FeatureImp$new(mod, X,  y = y, loss = 'ce', method = 'cartesian')
pimp$results
plot(pimp)

FeatureImp$new(mod, X,  y=y,  loss = 'mae')

library("randomForest")
data("Boston", package  = "MASS")
mod = randomForest(medv ~ ., data = Boston, ntree = 50)
mod = Model$new(mod)
pimportance = FeatureImp$new(mod, Boston[which(names(Boston) != 'medv')], y = Boston$medv, loss = 'mae', method = 'cartesian')

pimp$results
pimp$plot()


## tree surrogate model, centered
library("randomForest")
data("Boston", package  = "MASS")
mod = randomForest(medv ~ ., data = Boston, ntree = 50)
mod = Model$new(mod)
tree = TreeSurrogate$new(mod, Boston[which(names(Boston) != 'medv')], 100, maxdepth = 4)

mod = randomForest(Species ~ ., data = iris, ntree = 50)
mod = Model$new(mod, predict.args = list(type = "prob"))

tree = TreeSurrogate$new(mod, iris[which(names(iris) != 'Species')], 100, 
  maxdepth = 2)

plot(tree)

print(tree)
tree$results

# get.tree.data 
# Returns: data.frame with nodes
dat = X
# alternative: tree$results$where, but works only for training data
tree.str = tree$results

