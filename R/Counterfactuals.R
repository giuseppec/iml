#' Counterfactuals Explanations
#' 
#' \code{Counterfactuals} Description
#' (Heavily?) based on ecr package and mosmafs package, but (heavily?) modified. 
#' Counterfactuals are calculated with a modified version of NSGA-II. 
#' 
#' @format \code{\link{R6Class}} object.
#' @name Counterfactuals
#' @section Usage:
#' \preformatted{
#' counterfactual = Counterfactuals$new(predictor, x.interest = NULL, target = NULL, 
#' epsilon = NULL, fixed.features = NULL, max.changed = NULL, 
#' mu = 50, nr.iterations = 100, p.mut = 0.2, p.rec = 1, p.mut.gen = 0.2, 
#' p.rec.gen = 0.7, seed = 1000, subset.results = FALSE, 
#' stag = .Machine$integer.max)
#' plotPF(counterfactual)
#' counterfactual$results
#' print(counterfactual)
#' counterfactual$explain(x.interest)
#' }
#' 
#' @section Arguments: 
#' For Counterfactuals$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{x.interest: }{(data.frame)\cr  Single row with the instance to be explained.}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a single numeric or 
#' a vector of two numerics, to define desired interval of outcome}
#' \item{epsilon: }{(numeric(1))\cr Maximal accepted absolute distance from target}
#' \item{fixed.features: }{(character)}\cr Character names of features not allowed to deviate from value of x.interest  } 
#' \item{max.changed: }{integer(1)\cr Maximum number of features that can be changed}
#' \item{mu: }{(integer(1))\cr Size of generation}
#' \item{nr.iterations: }{(integer(1))\cr Number of generations, respectively, iterations}
#' \item{p.mut: }{numeric(1)\cr Probability to apply mutation to a child}
#' \item{p.rec: }{numeric(1)\cr Probability to apply recombination to a child}
#' \item{p.mut.gen:}{numeric(1)\cr Probability to apply mutation to a gene of selected child}
#' \item{p.rec.gen:}{numeric(1)\cr Probability to apply recombination to the same genes of two parents}
#' }
#' 
#' @section Details:
#' For more details on the method see https://christophm.github.io/interpretable-ml-book/counterfactual.html
#' For more details on the algorithm NSGA-II see https://ieeexplore.ieee.org/document/996017
#' 
#' @section Fields:
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{results: }{(data.frame)\cr data.frame with Counterfactuals, Counterfactuals computed difference to x.interest and log for evaluation}
#' \item{x.interest: }{(data.frame)\cr Single row with the instance to be explained.}
#' \item{y.hat.interest: }{(numeric)\cr Predicted value for instance of interest}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{explain(x.interest)}{method to set a new data point which to explain.}
#' \item{plot()}{method to plot the pareto front. See \link{plot.Counterfactuals}}
#' \item{plotStatistics()}{method to plot log of results for evaluation of algorithm}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#'
#' @references 
#' Wachter, NSGA-II basis ecr
#' 
#' 
#' @seealso 
#' \link{Counterfactuals}
#' 
#' @seealso 
#' A different way to explain predictions: \link{LocalModel}, \link{Shapley}
#' 
#' @examples 
#' if (require("randomForest")) {
# data("Boston", package  = "MASS")
# Boston = Boston
# set.seed(1000)
# rf =  randomForest(medv ~ ., data = Boston)
# X = Boston[-which(names(Boston) == "medv")]
# mod = Predictor$new(rf, data = X)
# 
# # Then we calculate Counterfactuals for the first instance
# x.interest = X[1,]
# mod$predict(x.interest)
# target = 30
# cf = Counterfactuals$new(mod, x.interest = x.interest, target = target, 
#   mu = 50, nr.iterations = 100)

# # The results can be accessed and plotted
# cf$results$counterfactuals
# cf$results$counterfactuals.diff
# cf$plotPF()
# cf$results$log
# cf$plotStatistics(range = TRUE)
# 
# # It is also possible to specify a search space epsilon, the maximum 
# # distance from the target as a constraint for the search
# cf = Counterfactuals$new(mod, x.interest = x.interest, target = target, 
#   mu = 50, nr.iterations = 100, epsilon = 1)
# cf$plotPF()
#' }
NULL

#'@export

Counterfactuals = R6::R6Class("Counterfactuals", 
  inherit = InterpretationMethod,
  public = list(
    x.interest = NULL,
    y.hat.interest = NULL,
    target = NULL, 
    input.data = NULL, 
    epsilon = NULL,
    fixed.features   = NULL,
    max.changed  = NULL,
    mu  = NULL,
    nr.iterations  = NULL,
    subset.results = NULL,
    p.mut  = NULL,
    p.rec  = NULL,
    p.mut.gen  = NULL,
    p.rec.gen  = NULL,
    seed = NULL,
    stag = NULL,
    results.subset = NULL,
    ref.point = NULL,
    explain = function(x.interest, target, epsilon = NULL, 
      fixed.features = NULL, max.changed = NULL) {
      private$flush()
      private$set.x.interest(x.interest)
      private$set.target(target)
      private$run()
    },
    initialize = function(predictor, x.interest = NULL, target = NULL, 
      epsilon = NULL, fixed.features = NULL, max.changed = NULL, 
      mu = 50, nr.iterations = 100, p.mut = 0.2, p.rec = 1, p.mut.gen = 0.2, 
      p.rec.gen = 0.7, seed = 1000, subset.results = FALSE, 
      stag = .Machine$integer.max) {
      
      super$initialize(predictor = predictor)
      
      # can be missing
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      checkmate::assert_numeric(epsilon, null.ok = TRUE, len = 1, 
        all.missing = FALSE)
      checkmate::assert_character(fixed.features, null.ok = TRUE)
      checkmate::assert_integerish(max.changed, null.ok = TRUE, len = 1)
      
      # should be here
      checkmate::assert_numeric(mu, lower = 1, finite = TRUE, 
        all.missing = FALSE, len = 1)
      checkmate::assert_numeric(nr.iterations, lower = 1, finite = TRUE, 
        all.missing = FALSE, len = 1)
      checkmate::assert_numeric(p.mut, lower = 0, upper = 1, 
        all.missing = FALSE, len = 1)
      checkmate::assert_numeric(p.rec, lower = 0, upper = 1, 
        all.missing = FALSE, len = 1)
      checkmate::assert_numeric(p.mut.gen, lower = 0, upper = 1, 
        all.missing = FALSE, len = 1)
      checkmate::assert_numeric(p.rec.gen, lower = 0, upper = 1, 
        all.missing = FALSE, len = 1)
      checkmate::assert_numeric(seed, all.missing = FALSE, len = 1)
      
      # assign
      self$input.data = as.data.frame(self$predictor$data$get.x())
      self$epsilon = epsilon
      self$max.changed = max.changed
      self$fixed.features = fixed.features
      self$mu = mu
      self$nr.iterations = nr.iterations
      self$p.mut = p.mut
      self$p.rec = p.rec 
      self$p.mut.gen = p.mut.gen
      self$p.rec.gen = p.rec.gen
      self$seed = seed
      self$subset.results = subset.results
      self$stag = stag
      
      # Define parameterset
      private$ps = ParamHelpers::makeParamSet(
        params = makeParamlist(self$input.data))
      
      # Extract info from input.data
      private$feature.names = names(self$input.data)
      private$range = ParamHelpers::getUpper(private$ps) - 
        ParamHelpers::getLower(private$ps)
      private$range[ParamHelpers::getParamIds(private$ps)
        [ParamHelpers::getParamTypes(private$ps) == "discrete"]]  = NA
      private$range = private$range[names(self$input.data)]
      
      private$sdev = apply(Filter(is.numeric, self$input.data), 2, sd)
      sdev.l = sdev.to.namedlist(private$sdev, private$ps)
      
      # Define operators based on parameterset private$ps
      private$mutator = mosmafs::combine.operators(private$ps,
        numeric = ecr::setup(custom.mutGauss, p = 1, sdev = sdev.l$numeric),
        integer = ecr::setup(custom.mutGaussInt, p = 1, sdev = sdev.l$integer),
        discrete = ecr::setup(mosmafs::mutRandomChoice, p = 1),
        logical = ecr::setup(ecr::mutBitflip, p = 1),
        use.orig = ecr::setup(mutBitflip.use.orig, p = self$p.mut.gen,
          max.changed = self$max.changed),
        .binary.discrete.as.logical = TRUE)
      
      private$recombinator = mosmafs::combine.operators(private$ps,
        numeric = ecr::setup(ecr::recSBX, p = 1),
        integer = ecr::setup(mosmafs::recIntSBX, p = 1),
        discrete = ecr::setup(mosmafs::recPCrossover, p = 1),
        logical = ecr::setup(mosmafs::recPCrossover, p = 1),
        use.orig = ecr::setup(recPCrossover.use.orig, p = self$p.rec.gen,
          max.changed = self$max.changed),
        .binary.discrete.as.logical = TRUE)
      
      private$parent.selector = ecr::selSimple
      
      private$survival.selector = ecr::setup(selNondom, epsilon = self$epsilon, 
        consider.diverse.solutions = TRUE, 
        extract.duplicates = TRUE)
      
      x.interest = x.interest[setdiff(colnames(x.interest), predictor$data$y.names)]
      if (!is.null(x.interest)) {
        private$set.x.interest(x.interest)
        private$set.target(target)
      }
      if (!is.null(x.interest) & !is.null(target)) private$run()
      print("initialize finished")
    }, 
    plotPF = function() {
      result = self$results$counterfactuals
      pf = result[, c("dist.target", "dist.x.interest", "nr.changed")]
      p = ggplot(data = pf, aes(x=dist.target, y=dist.x.interest, 
        color = as.factor(nr.changed))) +
        geom_point() +
        xlab("dist target") +
        ylab("dist x.interest") +
        #ggtitle(title)+
        guides(color=guide_legend(title="nr changed"))
      print(p)
    },
    plotStatistics = function(range = FALSE) {
      
      min.obj = c("gen", "dist.target.min", 
        "dist.x.interest.min", "nr.changed.min")     
      mean.obj = c("gen", "dist.target.mean", 
        "dist.x.interest.mean", "nr.changed.mean")
      eval = c("gen", "fitness.domHV", "fitness.delta", 
        "fitness.spacing", "population.div")
      nameList = list(min.obj, mean.obj, eval)
      if (range) {
        log = mlr::normalizeFeatures(self$results$log, method = "range", 
          cols = names(cf$results$log)[-1])
      } else {
        log = self$results$log
      }
      p = lapply(nameList, function(nam) {
        Sys.sleep(0.5)
        df = melt(log[,nam] , id.vars = 'gen', variable.name = 'legend')
        print(
          ggplot(df, aes(gen, value)) + geom_line(aes(colour = legend)) + 
            ylab("normalized value"))
    })
    }, 
    calculateHV = function() {
      return(private$log$fitness.domHV[self$nr.iterations])
    }
  ), 
  private = list(
    x.interest.orig = NULL,
    range = NULL,
    sdev = NULL,
    ps = NULL,
    mutator = NULL, 
    recombinator = NULL, 
    parent.selector = NULL,
    survival.selector = NULL,
    ecrresults = NULL,
    log = NULL,
    set.x.interest = function(x.interest) {
      assert_data_frame(x.interest, any.missing = FALSE, all.missing = FALSE, 
        min.rows = 1, max.rows = 1, null.ok = FALSE)
      if(any(!(colnames(self$input.data) %in% colnames(x.interest)))) {
        stop("colnames of x.interest must be identical to training data")
      }
      x.interest = x.interest[setdiff(colnames(x.interest), self$input.data)]
      private$x.interest.orig = x.interest
      self$y.hat.interest = self$predictor$predict(x.interest)[1,]
      i <- sapply(x.interest, is.factor)
      x.interest[i] <- lapply(x.interest[i], as.character)
      self$x.interest = x.interest
    }, 
    set.target = function(target) {
      checkmate::assert_numeric(target, null.ok = FALSE, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      self$target = target
    },
    run = function(force = FALSE) {
      self$ref.point = c(max(abs(self$y.hat.interest - self$target)), 1,
        ncol(self$x.interest))
      private$ecrresults = private$search()
      self$results = private$aggregate()
    },
    search = function() {
      
      # Initialize population based on x.interest, ps and sdev
      lower = self$x.interest[names(private$sdev)] - private$sdev
      upper = self$x.interest[names(private$sdev)] + private$sdev
      lower.ps = pmax(ParamHelpers::getLower(private$ps), lower)
      upper.ps = pmin(ParamHelpers::getUpper(private$ps), upper) 
      ps.initialize = ParamHelpers::makeParamSet(params = makeParamlist(self$input.data, 
        lower = lower.ps, 
        upper = upper.ps))
      set.seed(self$seed)
      initial.pop = ParamHelpers::sampleValues(ps.initialize, self$mu, 
        discrete.names = TRUE)
      
      # Compute counterfactuals 
      set.seed(self$seed)
      results = generateCounterfactuals(x.interest = self$x.interest,
        target = self$target, predictor = self$predictor, param.set = private$ps,
        fixed.features = self$fixed.features, 
        mu = self$mu, lambda = self$mu, p.recomb = self$p.rec, p.mut = self$p.mut,
        range.features = private$range, initial.solutions = initial.pop,
        survival.selector = private$survival.selector,
        parent.selector = private$parent.selector,
        mutator = private$mutator, recombinator = private$recombinator,
        number.iterations = self$nr.iterations, ref.point = self$ref.point, 
        stag = self$stag)
      print("run finished")
      return(results)
    },
    
    aggregate = function(subset.results = self$subset.results) {
      
      pareto.front = private$ecrresults$pareto.front
      names(pareto.front) = c("dist.target", "dist.x.interest", "nr.changed")
      
      pareto.set.l = lapply(private$ecrresults$pareto.set, function(x)
        mosmafs::valuesFromNames(private$ps, x))
      pareto.set = list.to.df(pareto.set.l)
      
      if (subset.results) {
        idx = unique(c(which.min(pareto.front$dist.target), 
          which.min(pareto.front$dist.x.interest), 
          getDiverseSolutions(pareto.front, pareto.set, private$range)))
        print("only a subset of solutions is extracted based on diversity measure")
      } else {
        idx = 1:nrow(pareto.set)
      }
      pareto.set = pareto.set %>% slice(idx)
      pareto.front = pareto.front %>% slice(idx) 

      pareto.set.diff = getDiff(pareto.set, self$x.interest)
      pred = private$run.prediction(pareto.set)
      
      pareto.set.pf = cbind(pareto.set, pred, pareto.front) %>% 
        arrange(dist.target)
      pareto.set.diff.pf = cbind(pareto.set.diff, pred, pareto.front) %>% 
        arrange(dist.target)
      
        
      results = list()
      results$counterfactuals = pareto.set.pf
      results$counterfactuals.diff = pareto.set.diff.pf
      results$log = ecr::getStatistics(private$ecrresults$log)
      names(results$log) = c("gen", "dist.target.min", "dist.target.mean", 
        "dist.x.interest.min", "dist.x.interest.mean", "nr.changed.min", 
        "nr.changed.mean", "fitness.domHV", "fitness.delta", 
        "fitness.spacing", "population.div")
      
      print("aggregate finished")
      return(results)
    }
    
  )
)


#' Plot Counterfactuals
#' 
#' plot.Counterfactuals() plots the Counterfactuals values - the contributions of feature values to the prediction. 
#' 
#' @param object  A Counterfactuals R6 object
#' @param sort logical. Should the feature values be sorted by Counterfactuals value? Ignored for multi.class output.
#' @return ggplot2 plot object
#' @seealso 
#' \link{Counterfactuals}
#' @examples 
#' \dontrun{
#' if (require("rpart")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' rf =  rpart(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#' 
#' # Then we explain the first instance of the dataset with the Counterfactuals method:
#' x.interest = X[1,]
#' Counterfactuals = Counterfactuals$new(mod, x.interest = x.interest, target = target)
#' plot(Counterfactuals)
#' }
#' }
plot.Counterfactuals = function(object) {
  object$plotPF()
}

plotStatistics = function(object, range = FALSE) {
  object$plotStatistics(range)
}


