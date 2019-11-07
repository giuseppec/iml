
[![Build
Status](https://travis-ci.org/christophM/iml.svg?branch=master)](https://travis-ci.org/christophM/iml)
[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/iml)](https://CRAN.R-project.org/package=iml)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/iml)](https://cran.rstudio.com/web/packages/iml/index.html)
[![codecov.io](https://codecov.io/github/christophM/iml/coverage.svg?branch=master)](https://codecov.io/github/christophM/iml?branch=master)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00786/status.svg)](https://doi.org/10.21105/joss.00786)

# iml: interpretable machine learning

`iml` is an R package that interprets the behaviour and explains
predictions of machine learning models. It implements model-agnostic
interpretability methods - meaning they can be used with any machine
learning model.

\<\<\<\<\<\<\< HEAD ![](iml.png) =======
<img src="inst/iml.png" height="200" />

> > > > > > > master Currently implemented:

  - Feature importance
  - Partial dependence plots
  - Individual conditional expectation plots (ICE)
  - Accumulated local effects
  - Tree surrogate
  - LocalModel: Local Interpretable Model-agnostic Explanations
  - Shapley value for explaining single predictions

Read more about the methods in the [Interpretable Machine Learning
book](https://christophm.github.io/interpretable-ml-book/agnostic.html)

# Tutorial

Start an interactive notebook tutorial by clicking on the badge:
[![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/christophM/iml/master?filepath=./notebooks/tutorial-intro.ipynb)

# Installation

The package can be installed directly from CRAN and the development
version from github:

``` r
# Stable version
install.packages("iml")
# Development version
devtools::install_github("christophM/iml")
```

# News

Changes of the packages can be accessed in the [NEWS
file](https://github.com/christophM/iml/blob/master/NEWS.md) shipped
with the package.

# Example

First we train a randomForest to predict the Boston median housing
value. How does lstat influence the prediction individually and on
average? (Accumulated local effects)

``` r
library("iml")
library("randomForest")
data("Boston", package  = "MASS")
rf = randomForest(medv ~ ., data = Boston, ntree = 50)
X =  Boston[which(names(Boston) != "medv")]
model = Predictor$new(rf, data = X, y = Boston$medv)
effect = FeatureEffects$new(model)
effect$plot(features = c("lstat", "age", "rm"))
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Contribute

Please check the [contribution guidelines](CONTRIBUTING.md)

# License

© 2018 [Christoph Molnar](https://christophm.github.io/)

The contents of this repository are distributed under the MIT license.
See below for details:

    The MIT License (MIT)
    
    Copyright (c) 2018 Christoph Molnar
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

# Python Implementation

Referring to <https://github.com/datascienceinc/Skater>

# Funding

This work is funded by the Bavarian State Ministry of Education, Science
and the Arts in the framework of the Centre Digitisation.Bavaria (ZD.B)
