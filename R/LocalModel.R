#' LocalModel
#' 
#' \code{LocalModel} fits locally weighted linear regression models (logistic regression for classification) to explain single predictions of a prediction model.
#' 
#' 
#' @format \code{\link{R6Class}} object.
#' @name LocalModel
#' @section Usage:
#' \preformatted{
#' lime = LocalModel$new(predictor, x.interest = NULL, dist.fun = "gower",  
#'                       kernel.width = NULL, k = 3 run = TRUE)
#' 
#' plot(lime)
#' predict(lime, newdata)
#' lime$results
#' lime$explain(x.interest)
#' print(lime)
#' }
#' 
#' @section Arguments:
#' 
#' For LocalModel$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{x.interest: }{(data.frame)\cr Single row with the instance to be explained.}
#' \item{dist.fun: }{(`character(1)`)\cr The name of the distance function for computing proximities (weights in the linear model). Defaults to "gower". Otherwise will be forwarded to [stats::dist].}
#' \item{kernel.width: }{(`numeric(1)`)\cr The width of the kernel for the proximity computation. Only used if dist.fun is not 'gower'.}
#' \item{k: }{(`numeric(1)`)\cr
#' The (maximum) number of features to be used for the surrogate model.}
#' \item{run: }{(`logical(1)`)\cr
#' Should the Interpretation method be run?}
#' }
#' 
#' @section Details: 
#' A weighted glm is fitted with the machine learning model prediction as target. 
#' Data points are weighted by their proximity to the instance to be explained, using the gower proximity measure. 
#' L1-regularisation is used to make the results sparse. 
#' The resulting model can be seen as a surrogate for the machine learning model, which is only valid for that one point.
#' Categorical features are binarized, depending on the category of the instance to be explained: 1 if the category is the same, 0 otherwise.
#' To learn more about local models, read the Interpretable Machine Learning book: https://christophm.github.io/interpretable-ml-book/lime.html
#'
#' The approach is similar to LIME, but has the following differences:
#' \itemize{
#' \item Distance measure: Uses as default the gower proximity (= 1 - gower distance) instead of a kernel based on the Euclidean distance. Has the advantage to have a meaningful neighbourhood and no kernel width to tune.
#' \item Sampling: Uses the original data instead of sampling from normal distributions. 
#' Has the advantage to follow the original data distribution. 
#' \item Visualisation: Plots effects instead of betas. Both are the same for binary features, but ared different for numerical features. 
#' For numerical features, plotting the betas makes no sense, 
#' because a negative beta might still increase the prediction when the feature value is also negative.
#' }
#' 
#' @section Fields:
#' \describe{
#' \item{best.fit.index: }{(`numeric(1)`)\cr The index of the best glmnet fit.}
#' \item{k: }{(`numeric(1)`)\cr The number of features as set by the user.}
#' \item{model: }{(glmnet)\cr The fitted local model.}
#' \item{predictor: }{(Predictor)\cr The prediction model that was analysed.}
#' \item{results: }{(data.frame)\cr Results with the feature names (\code{feature}) and contributions to the prediction}
#' \item{x.interest: }{(data.frame)\cr The instance to be explained. See Examples for usage.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{explain(x.interest)}{method to set a new data point which to explain.}
#' \item{plot()}{method to plot the LocalModel feature effects. See \link{plot.LocalModel}}
#' \item{predict()}{method to predict new data with the local model See also \link{predict.LocalModel}}
#' \item{\code{run()}}{[internal] method to run the interpretability method. Use \code{obj$run(force = TRUE)} to force a rerun.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#' 
#' 
#' @references 
#' Ribeiro, M. T., Singh, S., & Guestrin, C. (2016). "Why Should I Trust You?": Explaining the Predictions of Any Classifier. Retrieved from http://arxiv.org/abs/1602.04938
#'
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623--637.
#' 
#' @seealso 
#' \code{\link{plot.LocalModel}} and \code{\link{predict.LocalModel}}
#' 
#' \code{\link{Shapley}} can also be used to explain single predictions
#' 
#' \code{\link[lime]{lime}}, the original implementation
#' @export
#' @importFrom glmnet glmnet
#' @importFrom stats dist
#' @examples 
#' if (require("randomForest")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' X = Boston[-which(names(Boston) == "medv")]
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = X)
#' 
#' # Explain the first instance of the dataset with the LocalModel method:
#' x.interest = X[1,]
#' lemon = LocalModel$new(mod, x.interest = x.interest, k = 2)
#' lemon
#' 
#' # Look at the results in a table
#' lemon$results
#' # Or as a plot
#' plot(lemon)
#'
#' # Reuse the object with a new instance to explain
#' lemon$x.interest
#' lemon$explain(X[2,])
#' lemon$x.interest
#' plot(lemon)
#'   
#' # LocalModel also works with multiclass classification
#' rf = randomForest(Species ~ ., data= iris, ntree=50)
#' X = iris[-which(names(iris) == 'Species')]
#' predict.fun = function(object, newdata) predict(object, newdata, type = "prob")
#' mod = Predictor$new(rf, data = X, predict.fun = predict.fun, class = "setosa")
#' 
#' # Then we explain the first instance of the dataset with the LocalModel method:
#' lemon = LocalModel$new(mod, x.interest = X[1,], k = 2)
#' lemon$results
#' plot(lemon) 
#' }
NULL


#' @export

LocalModel = R6::R6Class("LocalModel", 
  inherit = InterpretationMethod,
  public = list(
    x.interest = NULL, 
    k = NULL,
    model = NULL,
    best.fit.index = NULL,
    predict = function(newdata = NULL, ...) {
      if (is.null(newdata)) newdata = self$x.interest
      X.recode = recode(newdata, self$x.interest)
      if (private$multiClass) {
        prediction = predict(self$model, newx=as.matrix(X.recode), type = "response")
        prediction = data.frame(prediction[,,self$best.fit.index])
        colnames(prediction) = colnames(private$predictResults)
        prediction
      } else {
        prediction = predict(self$model, newx = as.matrix(X.recode))
        pred = prediction[,self$best.fit.index, drop = FALSE]
        colnames(pred) = NULL
        data.frame(prediction = pred)
      }
    },
    explain = function(x.interest) {
      self$x.interest = x.interest
      private$flush()
      self$run()
    },
    initialize = function(predictor, x.interest = NULL, dist.fun = "gower", kernel.width = NULL, k = 3, run = TRUE) {
      assert_number(k, lower = 1, upper = predictor$data$n.features)
      assert_data_frame(x.interest, null.ok = TRUE)
      assert_choice(dist.fun, c("gower", "euclidean", "maximum", 
        "manhattan", "canberra", "binary", "minkowski"))
      if (!require("glmnet")) {
        stop("Please install glmnet.")
      }
      super$initialize(predictor = predictor)
      self$k = k
      if (!is.null(x.interest)) {
        self$x.interest = x.interest
      }
      private$weight.fun = private$get.weight.fun(dist.fun, kernel.width)
      
      if (run & !is.null(x.interest)) self$run()
    }
  ),
  private = list(
    q = function(pred) probs.to.labels(pred),
    best.index = NULL,
    aggregate = function() {
      X.recode = recode(private$dataDesign, self$x.interest)
      x.recoded = recode(self$x.interest, self$x.interest)
      fam = ifelse(private$multiClass, "multinomial", "gaussian")
      y = unlist(private$qResults[1])
      w = private$weight.fun(X.recode, x.recoded)
      self$model = glmnet(x = as.matrix(X.recode), y = y, family = fam, 
        w = w,  intercept = TRUE, standardize = TRUE, type.multinomial = "grouped")
      res = self$model
      ## It can happen, that no n.vars matching k occurs
      if (any(res$df == self$k)) {
        best.index = max(which(res$df == self$k))
      } else {
        best.index = max(which(res$df < self$k))
        warning("Had to choose a smaller k")
      }
      self$best.fit.index = best.index
      if (private$multiClass) {
        class.results = lapply(res$beta, extract.glmnet.effects, 
          best.index = best.index, x.recoded = x.recoded, x.original = self$x.interest)
        res = data.table::rbindlist(class.results)
        res$.class = rep(names(class.results), each = ncol(X.recode))
      } else {
        res = extract.glmnet.effects(res$beta, best.index, x.recoded, self$x.interest)
      }
      res[res$beta != 0, ]
    },
    intervene = function() private$dataSample, 
    generatePlot = function() {
      p = ggplot(self$results) + 
        geom_col(aes(y = effect, x = feature.value)) + coord_flip()
      if (private$multiClass) p = p + facet_wrap(".class")
      p
    },
    get.weight.fun = function(dist.fun, kernel.width) {
      if (dist.fun == "gower") {
        require("gower")
        function(X, x.interest) {
          1 - gower_dist(X, x.interest)
        }
      } else {
        assert_numeric(kernel.width)
        function(X, x.interest) {
          d = dist(rbind(x.interest, X), method = dist.fun)[1+1:nrow(X)]
          sqrt(exp(-(d^2) / (kernel.width^2)))
        }
      }
    },
    weight.fun = NULL
  )
)

#' Predict LocalModel
#' 
#' Predict the response for newdata with the LocalModel model.
#' 
#' @param object A LocalModel R6 object
#' @param newdata A data.frame for which to predict
#' @param ... Further arguments for the objects predict function
#' @return A data.frame with the predicted outcome. 
#' @seealso 
#' \link{LocalModel}
#' @importFrom stats predict
#' @export
#' @examples 
#' if (require("randomForest")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' X = Boston[-which(names(Boston) == "medv")]
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = X)
#' 
#' # Explain the first instance of the dataset with the LocalModel method:
#' x.interest = X[1,]
#' lemon = LocalModel$new(mod, x.interest = x.interest, k = 2)
#' predict(lemon, newdata = x.interest)
#' }
predict.LocalModel = function(object, newdata = NULL, ...) {
  object$predict(newdata = newdata, ...)
}

#' Plot Local Model
#' 
#' plot.LocalModel() plots the feature effects of a LocalModel object.
#' 
#' @param object  A LocalModel R6 object
#' @return ggplot2 plot object
#' @seealso 
#' \link{LocalModel}
#' @examples 
#' if (require("randomForest")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' X = Boston[-which(names(Boston) == "medv")]
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' mod = Predictor$new(rf, data = X)
#' 
#' # Explain the first instance of the dataset with the LocalModel method:
#' x.interest = X[1,]
#' lemon = LocalModel$new(mod, x.interest = x.interest, k = 2)
#' plot(lemon)
#' }
plot.LocalModel = function(object) {
  object$plot()
}






