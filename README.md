
[![Build Status](https://travis-ci.org/christophM/iml.svg?branch=master)](https://travis-ci.org/christophM/iml)

iml: interpretable machine learning
===================================

`iml` is an R package that interprets the behaviour and explains predictions of machine learning models. It implements model-agnostic interpretability methods - meaning they can be used with any machine learning model.

Currently implemented:

-   Feature importance
-   Partial dependence plots
-   Individual conditional expectation plots (ICE)
-   Tree surrogate
-   LIME: Local Interpretable Model-agnostic Explanations
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
mod = randomForest(medv ~ ., data = Boston, ntree = 50)
predictor = makePredictor(mod)
```

#### What were the most important features? (Permutation feature importance / Model reliance)

``` r
imp = FeatureImp$new(predictor, Boston, y = Boston$medv, loss = "mae")
plot(imp)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
imp$data()
```

    ## # A tibble: 14 x 3
    ##    ..feature error importance
    ##    <fct>     <dbl>      <dbl>
    ##  1 lstat     4.23        4.29
    ##  2 rm        3.71        3.76
    ##  3 dis       1.66        1.68
    ##  4 crim      1.65        1.67
    ##  5 nox       1.64        1.66
    ##  6 ptratio   1.58        1.60
    ##  7 indus     1.47        1.49
    ##  8 age       1.28        1.30
    ##  9 tax       1.24        1.26
    ## 10 black     1.24        1.25
    ## 11 rad       1.11        1.12
    ## 12 zn        1.09        1.11
    ## 13 chas      1.01        1.03
    ## 14 medv      0.988       1.00

### Let"s build a single tree from the randomForest predictions! (Tree surrogate)

``` r
tree = TreeSurrogate$new(predictor, Boston[which(names(Boston) != "medv")], maxdepth = 2)
plot(tree)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

### How does lstat influence the prediction on average? (Partial dependence plot)

``` r
pdp.obj = PartialDependence$new(predictor, Boston, feature = 13)
plot(pdp.obj)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

### How does lstat influence the individual predictions? (ICE)

``` r
ice.curves = Ice$new(predictor, Boston[1:100,], feature = 13)
plot(ice.curves) 
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Explain a single prediction with a local linear model. (LIME)

``` r
lime.explain = Lime$new(predictor, Boston, x.interest = Boston[1,])
lime.explain$data()
```

    ##              beta x.scaled     effect x.original feature feature.value
    ## rm     0.45640527    6.575  3.0008647      6.575      rm      rm=6.575
    ## lstat -0.04312273    4.980 -0.2147512       4.98   lstat    lstat=4.98
    ## medv   0.78621621   24.000 18.8691890         24    medv       medv=24

``` r
plot(lime.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Explain a single prediction with game theory. (Shapley)

``` r
shapley.explain = Shapley$new(predictor, Boston, x.interest = Boston[1, ])
shapley.explain$data()
```

    ## # A tibble: 14 x 3
    ## # Groups:   feature [?]
    ##    feature     phi phi.var
    ##    <fct>     <dbl>   <dbl>
    ##  1 age     -0.0290  0.235 
    ##  2 black    0.0203  0.124 
    ##  3 chas    -0.0216  0.0158
    ##  4 crim    -0.339   1.37  
    ##  5 dis     -0.462   4.14  
    ##  6 indus    0.590   1.10  
    ##  7 lstat    2.13   12.6   
    ##  8 medv     0       0     
    ##  9 nox     -0.218   0.450 
    ## 10 ptratio  0.427   1.01  
    ## 11 rad     -0.237   0.0904
    ## 12 rm      -1.37   25.7   
    ## 13 tax      0.0216  0.136 
    ## 14 zn       0.286   0.0978

``` r
plot(shapley.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

Python Implementation
=====================

Referring to <https://github.com/datascienceinc/Skater>
