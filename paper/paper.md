---
title: 'iml: An R package for Interpretable Machine Learning'
tags:
  - R
  - machine learning
  - interpretability
authors:
  - name: Christoph Molnar
    orcid: 0000-0003-2331-868X
    affiliation: 1
affiliations:
 - name: Department of Statistics, LMU Munich
   index: 1
date: 8 May 2018
bibliography: paper.bib
---

# Summary

Machine learning models are good at predicting, but are usually not interpretable.
A bunch of methods exist to make those interpretable. 

``iml`` is an R package [@R] for making machine learning models interpretable. It 
offers model-agnostic methods, that work on any type of supervised model for
tabular data. Implemented methods include:

- Partial dependence plots [@Friedman1999]
- Individual conditional expectation [@Goldstein2013]
- Feature importance [@importance]
- Global surrogate mdoels [@global]
- Local surrogate models [@local]
- Shapley value [@shapley]
- Interaction effects [@interaction]

``iml``  was designed to provide a class-based and user-friendly way to 
use all the listed methods in a similar way. Internally, they all use 
the same parent class and share a common framework for the computation. 
The goal of ``iml`` is to provide a unified interface for all these methods
so that it can all be used in one place. Many of the methods are already 
implemented in other places ([@pdp-pkg], [@ice-pkg], [@lime-pkg], [@imp-pkg]), 
but this is the first place where all are with the same interface and 
in the same computational framework. 

The unified interface will greatly simplify the analysis and interpretation 
of supervised box machine learning learning models. 

# Acknowledgements

This work is funded by the Bavarian State Ministry of Science and the Arts in the framework of the Centre Digitisation.Bavaria (ZD.B)

# References
