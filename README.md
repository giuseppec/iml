
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
devtools::install_github('christophM/iml')
```

Examples
========

First we train a randomForest to predict the Boston median housing value

``` r
library('iml')

library('randomForest')
data("Boston", package  = "MASS")
mod = randomForest(medv ~ ., data = Boston, ntree = 50)
```

#### What were the most important features? (Permutation feature importance / Model reliance)

``` r
imp = feature.imp(mod, Boston, y = Boston$medv, loss = 'mae')
plot(imp)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
imp$data()
```

    ## # A tibble: 14 x 3
    ##    ..feature error importance
    ##    <fct>     <dbl>      <dbl>
    ##  1 age       1.28        1.30
    ##  2 black     1.24        1.25
    ##  3 chas      1.01        1.03
    ##  4 crim      1.65        1.67
    ##  5 dis       1.66        1.68
    ##  6 indus     1.47        1.49
    ##  7 lstat     4.23        4.29
    ##  8 medv      0.988       1.00
    ##  9 nox       1.64        1.66
    ## 10 ptratio   1.58        1.60
    ## 11 rad       1.11        1.12
    ## 12 rm        3.71        3.76
    ## 13 tax       1.24        1.26
    ## 14 zn        1.09        1.11

### Let's build a single tree from the randomForest predictions! (Tree surrogate)

``` r
tree = tree.surrogate(mod, Boston[which(names(Boston) != 'medv')], maxdepth = 2)
plot(tree)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

### How does lstat influence the prediction on average? (Partial dependence plot)

``` r
pdp.obj = pdp(mod, Boston, feature = 13)
plot(pdp.obj)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

### How does lstat influence the individual predictions? (ICE)

``` r
ice.curves = ice(mod, Boston[1:100,], feature = 13)
plot(ice.curves) 
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Explain a single prediction with a local linear model. (LIME)

``` r
x = Boston[1,]
lime.explain = lime(mod, Boston, x.interest = x)
lime.explain$data()
```

    ##              beta x.scaled     effect x.original feature feature.value
    ## rm     1.03669473    6.575  6.8162678      6.575      rm      rm=6.575
    ## lstat -0.05867633    4.980 -0.2922081       4.98   lstat    lstat=4.98
    ## medv   0.72759814   24.000 17.4623553         24    medv       medv=24

``` r
plot(lime.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Explain a single prediction with game theory. (Shapley)

``` r
x = Boston[1,]
shapley.explain = shapley(mod, Boston, x.interest = x)
shapley.explain$data()
```

    ## # A tibble: 14 x 3
    ## # Groups:   feature [?]
    ##    feature      phi  phi.var
    ##    <fct>      <dbl>    <dbl>
    ##  1 age     -0.00607  0.0969 
    ##  2 black    0.0974   0.102  
    ##  3 chas    -0.00898  0.00539
    ##  4 crim    -0.298    1.86   
    ##  5 dis     -0.147    1.40   
    ##  6 indus    0.634    0.724  
    ##  7 lstat    3.12    12.7    
    ##  8 medv     0        0      
    ##  9 nox     -0.206    0.467  
    ## 10 ptratio  0.553    0.698  
    ## 11 rad     -0.156    0.0683 
    ## 12 rm       0.706    4.05   
    ## 13 tax     -0.111    0.378  
    ## 14 zn       0.201    0.115

``` r
plot(shapley.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

Python Implementation
=====================

Referring to <https://github.com/datascienceinc/Skater>
