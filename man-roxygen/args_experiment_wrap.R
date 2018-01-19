#' @param object 
#'    The object is the machine learning model. Different types are allowed. 
#'    Recommended are mlr WrappedModel and caret train object. \code{object} can also be 
#'    a function that predicts the outcome given features or anything with an S3 predict function,
#'    like an object from class lm.
#' @param X
#'    data.frame with the data for the prediction model
#' @param class In case of classification, class specifies the class for which to predict the probability. 
#' By default the first class in the prediction (first column) is chosen. 
#' @return 
#' All interpretability methods return an R6 Interpretation object. 
#' The object can be used in a similar fashion like S3 objects. 
#' 
#' \itemize{
#' \item \code{print(obj)} (alternatively: \code{obj$print()}) prints information about the interpretation
#' \item \code{obj$data()} returns the result data.frame
#' \item \code{plot(obj)} (alternatively \code{obj$plot()}) plots the results. The returned plot is a \code{ggplot}
#' object. This means it can be plotted directly or be extended using ggplots \code{+} operator.   
#' }
#' 
