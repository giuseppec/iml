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
pdp.obj = pdp(object = mod, X=X, feature = c(1, 2))  
pdp.obj
pdp.obj$data()

plot(pdp.obj)


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
shapley1

## Permutation feature importance
perm.imp(mod, X,  y=(y=='setosa'), predict.args = list(type = 'prob'), class = 1)$data()
perm.imp(mod, X,  y=(y=='setosa'), predict.args = list(type = 'prob'))$plot()

perm.imp(mod, X,  y=(y=='setosa'), predict.args = list(type = 'prob'), class = 1)


## Sobol (first order)
#sobol(mod, X, sample.size = 1000, predict.args = list(type = 'prob'), class = 2)


## tree surrogate model, centered
tree = tree.surrogate(mod, X, 10000, predict.args = list(type = 'prob'), class = 1)
print(tree)
tree$data()
