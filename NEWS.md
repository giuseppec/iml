# iml 0.3 (In PROGRESS)
* FeatureImp$results column permutationError renamed to permutation.error

# iml 0.2
* The API has been  reworked: 
  * User directly interacts with R6 classes (`pdp()` is now `PartialDependence$new()`).
  * User has to wrap the machine learning model with `Predictor$new()`.
  * New data points in `Shapley` and `LocalModel` can be set with `$explain()`.
  * `Lime` has been renamed to `LocalModel`.
* Plots have been improved.
* Documentation has been improved.

# iml 0.1
Initial release