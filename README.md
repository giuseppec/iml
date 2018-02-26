
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
rf = randomForest(medv ~ ., data = Boston, ntree = 50)
model = Model$new(rf)
```

#### What were the most important features? (Permutation feature importance / Model reliance)

``` r
imp = FeatureImp$new(model, Boston, y = Boston$medv, loss = "mae")
plot(imp)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
imp$results
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
tree = TreeSurrogate$new(model, Boston[which(names(Boston) != "medv")], maxdepth = 2)
plot(tree)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

### How does lstat influence the prediction on average? (Partial dependence plot)

``` r
pdp.obj = PartialDependence$new(model, Boston, feature = "lstat")
plot(pdp.obj)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

### How does lstat influence the individual predictions? (ICE)

``` r
ice.curves = Ice$new(model, Boston[1:100,], feature = "lstat")
plot(ice.curves) 
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Explain a single prediction with a local linear model. (LIME)

``` r
lime.explain = Lime$new(model, Boston, x.interest = Boston[1,])
lime.explain$results
```

    ##             beta x.scaled     effect x.original feature feature.value
    ## rm     0.6352285    6.575  4.1766275      6.575      rm      rm=6.575
    ## lstat -0.0516669    4.980 -0.2573012       4.98   lstat    lstat=4.98
    ## medv   0.7919096   24.000 19.0058305         24    medv       medv=24

``` r
plot(lime.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Explain a single prediction with game theory. (Shapley)

``` r
shapley.explain = Shapley$new(model, Boston, x.interest = Boston[1, ])
shapley.explain$results
```

    ## # A tibble: 14 x 3
    ## # Groups:   feature [?]
    ##    feature      phi  phi.var
    ##    <fct>      <dbl>    <dbl>
    ##  1 age     -0.0801   0.145  
    ##  2 black   -0.00383  0.0983 
    ##  3 chas    -0.00635  0.00660
    ##  4 crim    -0.331    1.90   
    ##  5 dis     -0.370    2.73   
    ##  6 indus    0.595    1.30   
    ##  7 lstat    3.87    23.5    
    ##  8 medv     0        0      
    ##  9 nox     -0.0175   1.25   
    ## 10 ptratio  0.491    0.917  
    ## 11 rad     -0.209    0.106  
    ## 12 rm      -0.0712  10.4    
    ## 13 tax      0.0147   0.140  
    ## 14 zn       0.202    0.0937

``` r
plot(shapley.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

Python Implementation
=====================

Referring to <https://github.com/datascienceinc/Skater>
