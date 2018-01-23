#' @param object 
#'    The object is the machine learning model. Different types are allowed. 
#'    Recommended are mlr WrappedModel and caret train objects. The \code{object} can also be 
#'    a function that predicts the outcome given features or anything with an S3 predict function,
#'    like an object from class \code{lm}.
#' @param X
#'    data.frame with the data for the prediction model
#' @param class In case of classification, class specifies the class for which to predict the probability. 
#' By default the first class in the prediction (first column) is chosen. 
#' @return 
#' All interpretability methods return an R6 Interpretation object. 
#' The objects can be used in a similar fashion as S3 objects or lists. 
#' 
#' \itemize{
#' \item \code{print(obj)} (alternatively: \code{obj$print()}) prints information about the interpretation
#' \item \code{obj$data()} returns the result data.frame
#' \item \code{plot(obj)} (alternatively \code{obj$plot()}) plots the results.
#' }
#' 
