
[![Build Status](https://travis-ci.org/christophM/iml.svg?branch=master)](https://travis-ci.org/christophM/iml) [![CRAN Status Badge](http://www.r-pkg.org/badges/version/iml)](https://CRAN.R-project.org/package=iml) [![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/iml)](https://cran.rstudio.com/web/packages/iml/index.html) [![codecov.io](https://codecov.io/github/christophM/iml/coverage.svg?branch=master)](https://codecov.io/github/christophM/iml?branch=master)

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

News
====

Changes of the packages can be accessed in the [NEWS file](https://github.com/christophM/iml/blob/master/NEWS.md) shipped with the package.

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

    ##    feature original.error permutation.error importance
    ## 1    lstat       1.004678          4.235347   4.215628
    ## 2       rm       1.004678          3.273892   3.258649
    ## 3     crim       1.004678          1.731467   1.723405
    ## 4  ptratio       1.004678          1.729005   1.720955
    ## 5      nox       1.004678          1.661731   1.653994
    ## 6      dis       1.004678          1.653906   1.646205
    ## 7    indus       1.004678          1.608825   1.601335
    ## 8      age       1.004678          1.378753   1.372334
    ## 9      tax       1.004678          1.360375   1.354041
    ## 10   black       1.004678          1.246488   1.240685
    ## 11     rad       1.004678          1.161532   1.156124
    ## 12      zn       1.004678          1.061880   1.056936
    ## 13    chas       1.004678          1.034714   1.029897

### Let"s build a single tree from the randomForest predictions! (Tree surrogate)

``` r
tree = TreeSurrogate$new(model, maxdepth = 2)
plot(tree)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

### How does lstat influence the prediction individually and on average? (Partial dependence plot and ICE)

``` r
pdp.obj = Partial$new(model, feature = "lstat")
pdp.obj$plot()
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Explain a single prediction with a local linear model. (LIME)

``` r
lime.explain = LocalModel$new(model, x.interest = X[1,])
lime.explain$results
```

    ##               beta x.recoded    effect x.original feature feature.value
    ## rm       4.2445268     6.575 27.907764      6.575      rm      rm=6.575
    ## ptratio -0.5224666    15.300 -7.993738       15.3 ptratio  ptratio=15.3
    ## lstat   -0.4287899     4.980 -2.135374       4.98   lstat    lstat=4.98

``` r
plot(lime.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Explain a single prediction with game theory. (Shapley)

``` r
shapley.explain = Shapley$new(model, x.interest = X[1, ])
shapley.explain$results
```

    ##    feature           phi      phi.var feature.value
    ## 1     crim -9.022108e-03  0.964278134  crim=0.00632
    ## 2       zn -9.738367e-02  0.051074401         zn=18
    ## 3    indus  9.395742e-01  1.742122038    indus=2.31
    ## 4     chas -5.958000e-03  0.004576209        chas=0
    ## 5      nox  3.246872e-02  1.625869345     nox=0.538
    ## 6       rm  1.204375e-01  8.147532838      rm=6.575
    ## 7      age -6.728853e-02  0.233897643      age=65.2
    ## 8      dis -8.011222e-05  0.776136361      dis=4.09
    ## 9      rad -3.547343e-01  0.284562657         rad=1
    ## 10     tax -2.085008e-01  0.802706167       tax=296
    ## 11 ptratio  5.989281e-01  1.193181060  ptratio=15.3
    ## 12   black -8.346362e-02  0.189856738   black=396.9
    ## 13   lstat  3.136579e+00 11.712142750    lstat=4.98

``` r
plot(shapley.explain)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

Python Implementation
=====================

Referring to <https://github.com/datascienceinc/Skater>
