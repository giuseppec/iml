#' @importFrom stats get_all_vars
find_data <- function(model, ...) {
  UseMethod("find_data")
}

find_data.default <- function(model, ...) {
  if (is.data.frame(model$model)) {
    return(model$model)
  } else {
    data = try(eval(model$call$data), silent = TRUE)
    if (inherits(data, "try-error") | !is.data.frame(data)) {
      stop("Can't extract data from model, please provide via data=")
    } 
    return(data)
  }
} 

find_data.gam <- find_data.default

find_data.glm <- function(model, ...) {
  if (is.environment(model$data))
    return(get_all_vars(model)) else
      model$data
}

find_data.lm <- function(model, ...) {
  get_all_vars(model)
}

find_data.train <- function(model, ...) {
  model$trainingData
}
