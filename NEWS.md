
# iml 0.4.0 (IN PROGRESS)
* `object$run()` does not return `self` any longer. This means using `object$set.feature()` for example does not automatically print the object summary any longer.
* Added an introductory vignette.
* Fixed an issue where the Predictor would not store X, when y is given as character.
* The column names of the data.frames with the results of the interpretation methods start with "." instead of "..". This is due to a recent change in the data.table package v1.10.5 [news item 18](https://github.com/Rdatatable/data.table/blob/master/NEWS.md).
* Removed the deprecated classes `PartialDependence` and `Ice`. Use `Partial` instead.

# iml 0.3.0
* FeatureImp$results column permutationError renamed to permutation.error
* Allow setting distance function in LocalModel
* Merge the classes Ice and PartialDependence into Partial
  * The newly introduced Partial class can plot ice and pd curves, also in the same plot
  * It is now possible to center partial dependence plots
  * In obj$results has a new column "type" which contains either "ice" or "pdp". The column ..individual was renamed to "..id" and "y.hat" has been renamed to "..y.hat".
  * Ice and PartialDependence will be deprecated starting from 0.4.x
  * Adds argument and field types in the documentation

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