
[![Build Status](https://travis-ci.org/christophM/iml.svg?branch=master)](https://travis-ci.org/christophM/iml)

iml: interpretable machine learning
===================================

`iml` is an R package that interprets the behaviour and explains predictions of machine learning models. It implements model-agnostic interpretability methods - meaning they can be used with any machine learning model.

Currently implemented:

-   Feature importance
-   Partial dependence plots
-   Individual conditional expectation plots (ICE)
-   Tree surrogate
-   LocalModel: Local Interpretable Model-agnostic Explanations
-   Shapley value for explaining single predictions

Read more about the methods in the [Interpretable Machine Learning book](https://christophm.github.io/interpretable-ml-book/agnostic.html)

Installation
============

The package can be installed directly from github with devtools:

``` r
# install.packages("devtools")
devtools::install_github("christophM/iml")
```

Examples
========

First we train a randomForest to predict the Boston median housing value

``` r
library("iml")

library("randomForest")
data("Boston", package  = "MASS")
rf = randomForest(medv ~ ., data = Boston, ntree = 50)
X =  Boston[which(names(Boston) != "medv")]
model = Predictor$new(rf, data = X, y = Boston$medv)
```

#### What were the most important features? (Permutation feature importance / Model reliance)

``` r
imp = FeatureImp$new(model, loss = "mae")
plot(imp)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
imp$results
```

    ##    feature original.error permutationError importance
    ## 1    lstat      0.9876488         4.233008   4.285945
    ## 2       rm      0.9876488         3.712950   3.759383
    ## 3      dis      0.9876488         1.656546   1.677263
    ## 4     crim      0.9876488         1.653230   1.673904
    ## 5      nox      0.9876488         1.637037   1.657509
    ## 6  ptratio      0.9876488         1.580299   1.600061
    ## 7    indus      0.9876488         1.468366   1.486728
    ## 8      age      0.9876488         1.283689   1.299742
    ## 9      tax      0.9876488         1.244533   1.260097
    ## 10   black      0.9876488         1.238134   1.253618
    ## 11     rad      0.9876488         1.109522   1.123398
    ## 12      zn      0.9876488         1.092671   1.106336
    ## 13    chas      0.9876488         1.014975   1.027668

### Let"s build a single tree from the randomForest predictions! (Tree surrogate)

``` r
tree = TreeSurrogate$new(model, maxdepth = 2)
plot(tree)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

### How does lstat influence the prediction on average? (Partial dependence plot)

``` r
pdp.obj = PartialDependence$new(model, feature = "lstat")
plot(pdp.obj)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

### How does lstat influence the individual predictions? (ICE)

``` r
ice.curves = Ice$new(model, feature = "lstat")
plot(ice.curves) 
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Explain a single prediction with a local linear model. (LIME)

``` r
lime.explain = LocalModel$new(model, x.interest = X[1,])
lime.explain$results
```

    ##               beta x.recoded    effect x.original feature feature.value
    ## rm       4.4692948     6.575 29.385614      6.575      rm      rm=6.575
    ## ptratio -0.5477636    15.300 -8.380784       15.3 ptratio  ptratio=15.3
    ## lstat   -0.4313727     4.980 -2.148236       4.98   lstat    lstat=4.98

``` r
plot(lime.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Explain a single prediction with game theory. (Shapley)

``` r
shapley.explain = Shapley$new(model, x.interest = X[1, ])
shapley.explain$results
```

    ##    feature          phi      phi.var feature.value
    ## 1      age -0.173130708  0.253650085  crim=0.00632
    ## 2    black  0.054638212  0.233187939         zn=18
    ## 3     chas -0.003419667  0.005878521    indus=2.31
    ## 4     crim -0.466446658  1.823447917        chas=0
    ## 5      dis -0.256641342  1.762279777     nox=0.538
    ## 6    indus  0.433310952  0.751072303      rm=6.575
    ## 7    lstat  2.987843211 19.500815533      age=65.2
    ## 8      nox -0.270343503  0.759897094      dis=4.09
    ## 9  ptratio  0.581080286  0.615308440         rad=1
    ## 10     rad -0.195727206  0.098238247       tax=296
    ## 11      rm -0.879252628 18.024706110  ptratio=15.3
    ## 12     tax -0.057217190  0.269396266   black=396.9
    ## 13      zn  0.202248909  0.134069091    lstat=4.98

``` r
plot(shapley.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

Python Implementation
=====================

Referring to <https://github.com/datascienceinc/Skater>
