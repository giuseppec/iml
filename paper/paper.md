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

Complex, non-parametric models as typically used in machine learning have proven to be very succesful in many prediction tasks. 
But machines usually operate as black boxes: While they are good at predicting, they are usually not interpretable.
Many inherently interpretable models have been suggested, which come with the cost of losing predictive power. 
Another option is to apply interpretability methods to a black box model after model training.
Since new machine learning algorithms are invented all the time, it is important to have a toolbox that is model-agnostic, meaning it shouldn't matter if they are applied to a random forest or a neural network. 
This should improve the adoption of machine learning and enable deriving insights from black box models flexibly, even should the underlying model be replaced.

<!-- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience-->
``iml`` is an R package [@R] that offers a general toolbox for making machine learning models interpretable. It
implements many model-agnostic methods which work for any type of supervised machine learning model. The package covers following methods:

- Partial dependence plots [@friedman2001greedy]: Showing the learned relationship between the features and the predictions. 
- Individual conditional expectation [@ice]: Showing the learned relationship between the features and the preditions on an individual level.
- Feature importance [@Fisher2018]: Scoring the features for the importance for the prediction.
- Global surrogate tree: Mimicking the behaviour of the black box model with an interpretable model.
- Local surrogate models [@ribeiro2016should]: Explaining single predictions by mimicking the behavior of the black box model with an interpretable, local model.
- Shapley value [@strumbelj2014]: Explaining single predictions by fairly distributing the predicted value among the features.
- Interaction effects [@friedman2008predictive]: Measuring how strongly features interact with each other in the black box model.

``iml``  was designed to provide a class-based and user-friendly way to 
use all the listed methods in a similar way. Internally, they all use 
the same parent class and share a common framework for the computation. 
The goal of ``iml`` is to provide a unified interface for all these methods
so that it can all be used in one place. Many of the methods are already 
implemented in other packages ([@pdp1], [@ice], [@lime], [@imp-pkg]), 
but this is the first place where all are with the same interface and 
in the same computational framework. Similar R projects: [@dalex]

The unified interface simplifies the analysis and interpretation 
of supervised box machine learning learning models.

<!-- Mentions (if applicable) of any ongoing research projects using the software or recent scholarly publications enabled by it -->

# Acknowledgements

This work is funded by the Bavarian State Ministry of Science and the Arts in the framework of the Centre Digitisation.Bavaria (ZD.B)

<!-- A list of key references including a link to the software archive -->
# References
