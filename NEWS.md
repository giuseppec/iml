# iml 0.9.0
* Removes the `run` parameter from all interpretation methods.
* Adds class `FeatureEffects` which wraps `FeatureEffect` and allows to compute feature effects for all features of a model with one call.
* Add column ".type" to `$result` data.frame of `FeatureEffect` when `method="ale"` and the feature is categorical
* Adds parameter `ylim` to `FeatureEffect$plot` to manually set the limits of the y-axis for feature effect plots with one feature.
* Adds `predict` method to FeatureEffect, which predicts the marginal effect for data instances.

# iml 0.8.1
* Fix vignette titles

# iml 0.8.0
* Some bigger changes in the feature importance class `FeatureImp`:
  - The `method` argument was removed, only shuffling is now possible. This means the cartesian product of all data points with all data points is not an option any longer. It was never really practical to use, except for toy examples.
  - The importance plot shows the name of the loss function in the x-axis label.
  - The importance plot shows the quantiles of importance over the different repetitions.
  - Default number of repetitions increased to 5.
* Fixes problems with missing centering of ALE plots when using multiclass
* Automatically extracts data and target from the model when possible (based on  the `prediction::find_data` function). Data extraction doesn't work with mlr, but target extraction does.
* Feature importance (`FeatureImp`) automatically returned the ratio of permuted model error and original model error. With 0.7.2 the user can choose between the ratio (default) and the difference.

# iml 0.7.1
* Fixes problems with wrong computation of feature importance, features effects and so on for xgboost models.

# iml 0.7.0
* The `Partial` class is deprecated and will be removed in future versions. You should use `FeatureEffect` now. Its usage is similar to `Partial` but the `aggregation` and `ice` argument are now combined in the new `method` argument, where you can choose between 'ale', 'pdp', 'ice', 'pdp+ice'.
* Introduced ALE plots into the `FeatureEffect` class  (`method='ale'`). They are now the default instead of PDPs, because they are faster and unbiased.
* Plot for categorical features in PDP changed. Now showing bar plots instead of boxplots when `method='pdp'`


# iml 0.6
* Removed losses: f1, logLoss, rmse, mdae, rae, rmse, rmsle, rse, rrse
f1 because the implementation used didn't make sense anyways
* Interaction: The results return as interaction strength now the H-statistic instead of the H-squared-statistic. 
  This makes it more coherent with the gbm pacakge and the interact.gbm function and with what Friedman uses in the plots in the paper.
  For users of the package this means that an interaction of strength x becomes an interaction of strength sqrt(x).
* `Interaction`, `FeatureImp` and `Partial` are now computed batch-wise in the background. This prevents this methods from overloading the memory. For that, the `Predictor` has a new init argument 'batch.size' which limits the number of rows send to the model for prediction for the methods `Interaction`, `FeatureImp` and `Partial`.
* `Interaction` and `FeatureImp` additionally allow parallel computation on multiple cores. See `vignette("parallel", package = "iml")` for how to use it.

# iml 0.5.2
* The `Predictor` can be initialized with a `type` (e.g. `type = "prob"`), which is more convenient than writing a custom `predict.fun`. For caret classification models, the default is now to return the response, so make sure to initialize the `Predictor` with `type = "prob"` for fine-grained results.
* It's easier to use classifier that output class labels and no probabilities. No warning will be issued anymore. Internally, the class labels are treated as probabilities (one column per class), where the probability for the predicted class is 1, for the others 0.
* `FeatureImp` supports the `n.repetitions` parameter which controls the number of repetitions of the feature shuffling.

# iml 0.5.0/1 
* Implemented Interaction measure
* Removed `feature.index` variable from `Partial` and renamed `.class.name` column in results to `.class`.

# iml 0.4.0 
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