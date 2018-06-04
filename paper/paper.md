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
<!-- A clear statement of need that illustrates the purpose of the software-->

Complex, non-parametric models, which are typically used in machine learning, have proven to be very succesful in many prediction tasks. 
But these models usually operate as black boxes: While they are good at predicting, they are often not interpretable.
Many inherently interpretable models have been suggested, which come at the cost of losing predictive power. 
Another option is to apply interpretability methods to a black box model after model training.
Since new machine learning algorithms are invented all the time, it is important to have a toolbox that is model-agnostic, meaning it shouldn't matter if they are applied to a random forest or a neural network. 
Having a toolset of model-agnostic interpretability methods should improve the adoption of machine learning.

<!-- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience-->
``iml`` is an R package [@R] that offers a general toolbox for making machine learning models interpretable. It
implements many model-agnostic methods which work for any type of machine learning model. The package covers following methods:

- Partial dependence plots [@friedman2001greedy]: Visualizing the learned relationship between the features and the predictions. 
- Individual conditional expectation [@ice]: Visualizing the learned relationship between the features and the preditions on an individual level.
- Feature importance [@Fisher2018]: Scoring the features by their contribution to predictive performance.
- Global surrogate tree: Approximating the black box model with an interpretable decision tree.
- Local surrogate models [@ribeiro2016should]: Explaining single predictions by approximating the black box model locally with an interpretable model.
- Shapley value [@strumbelj2014]: Explaining single predictions by fairly distributing the predicted value among the features.
- Interaction effects [@friedman2008predictive]: Measuring how strongly features interact with each other in the black box model.

``iml``  was designed to provide a class-based and user-friendly way to 
make black box machine learning models interpretable. 
Internally, the implemented methods inherit from the same parent class and share a common framework for the computation. 
Many of the methods are already implemented in other packages (e.g. [@pdp1], [@ice], [@lime]), but with the `iml` package, all of the methods are in one place, use the same syntax and the interpretability objects offer consistent methods and outputs.
Similar projects are the R package `DALEX` [@dalex] and the Python package `Skater` [@pramit_choudhary_2018_1198885].
The difference to `iml` is that the other two projects don't implement the methods themselves, but depend on other packages.
`DALEX` focuses more on model comparison, and `Skater` additionally includes interpretable models and has less model-agnostic interpretability methods compared to `iml`.

The unified interface provided by the `iml` package simplifies the analysis and interpretation of black box machine learning learning models.

<!-- Mentions (if applicable) of any ongoing research projects using the software or recent scholarly publications enabled by it -->

# Acknowledgements

This work is funded by the Bavarian State Ministry of Science and the Arts in the framework of the Centre Digitisation.Bavaria (ZD.B)

<!-- A list of key references including a link to the software archive -->
# References
