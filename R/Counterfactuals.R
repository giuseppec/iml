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
#' \item{subset.results}
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
    nr.solutions = NULL,
    ref.point = NULL,
    explain = function(x.interest, target, epsilon = NULL, 
      fixed.features = NULL, max.changed = NULL) {
      checkmate::assert_numeric(target, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      self$target = target
      private$set.x.interest(x.interest)
      private$flush()
      if (!is.null(epsilon)) {
        checkmate::assert_number(epsilon)
        self$epsilon = epsilon
      }
      if (!is.null(fixed.features)) {
        checkmate::assert_character(fixed.features)
        self$fixed.features = fixed.features
      }
      if (!is.null(max.changed)) {
        checkmate::assert_integerish(max.changed, len = 1)
        self$max.changed = max.changed
      }
      private$run()
    },
    initialize = function(predictor, x.interest = NULL, target = NULL, 
      epsilon = NULL, fixed.features = NULL, max.changed = NULL, 
      mu = 50, nr.iterations = 100, p.mut = 0.2, p.rec = 1, p.mut.gen = 0.2, 
      p.rec.gen = 0.7, subset.results = FALSE, nr.solutions = NULL,
      stag = .Machine$integer.max,
      seed = NULL) {
      
      # can be missing
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      checkmate::assert_number(epsilon, null.ok = TRUE)
      checkmate::assert_character(fixed.features, null.ok = TRUE)
      checkmate::assert_integerish(max.changed, null.ok = TRUE, len = 1)
      checkmate::assert_number(seed, null.ok = TRUE)
      checkmate::assert_integerish(stag, null.ok = TRUE)
      checkmate::assert_integerish(nr.solutions, null.ok = TRUE)
      
      # should be here
      checkmate::assert_integerish(mu, lower = 1)
      checkmate::assert_integerish(nr.iterations, lower = 1)
      checkmate::assert_number(p.mut, lower = 0, upper = 1)
      checkmate::assert_number(p.rec, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.gen, lower = 0, upper = 1)
      checkmate::assert_logical(subset.results)
      
      if (subset.results && is.null(nr.solutions)) {
        stopf('argument "nr.solutions" is missing, with no default')
      }
      # assign
      super$initialize(predictor = predictor)
      x.interest = x.interest[setdiff(colnames(x.interest), predictor$data$y.names)]
      if (!is.null(x.interest)) {
        private$set.x.interest(x.interest)
      }
      self$target = target
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
      self$nr.solutions = nr.solutions
      self$stag = stag
      
      # Define parameterset
      private$param.set= ParamHelpers::makeParamSet(
        params = makeParamlist(predictor$data$get.x()))
      
      # Extract info from input.data
      private$range = ParamHelpers::getUpper(private$param.set) - 
        ParamHelpers::getLower(private$param.set)
      private$range[ParamHelpers::getParamIds(private$param.set)
        [ParamHelpers::getParamTypes(private$param.set) == "discrete"]]  = NA
      private$range = private$range[predictor$data$feature.names]
      private$sdev = apply(Filter(is.numeric, predictor$data$get.x()), 2, sd)
      if (!is.null(x.interest) & !is.null(target)) private$run()
      cat("initialize finished\n")
    }, 
    plotPF = function(labels = FALSE) {
      result = self$results$counterfactuals
      diffs = self$results$counterfactuals.diff
      diffs = diffs[, !(names(diffs) %in% c("dist.target", "dist.x.interest", "nr.changed", "pred"))]
      pf = result[, c("dist.target", "dist.x.interest", "nr.changed")]
      
      if (labels) {
        pf$label = ""
        for(i in 1:nrow(diffs)) {
          names = names(diffs[i,])[diffs[i,] != 0]
          r = diffs[i, names]
          r = paste(paste(names, round(r, 3)), collapse = " & ")
          pf[i, "label"] = r
        }
      }
      p = ggplot(data = pf, aes(x=dist.target, y=dist.x.interest, 
        color = as.factor(nr.changed))) +
        geom_point() +
        xlab("dist target") +
        ylab("dist x.interest") +
        #ggtitle(title)+
        guides(color=guide_legend(title="nr changed"))
      
      if (labels) {
        p = p + ggrepel::geom_label_repel(aes(label = label),
          box.padding   = 0.35, 
          point.padding = 0.4, 
          show.legend = FALSE) 
      }
      
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
          cols = names(cf$results$log)[!names(cf$results$log) %in% c("gen", "state")])
      } else {
        log = self$results$log
      }
      p = lapply(nameList, function(nam) {
        Sys.sleep(0.5)
        df = melt(log[,nam] , id.vars = 'gen', variable.name = 'legend')
        print(
          ggplot(df, aes(gen, value)) + geom_line(aes(colour = legend)) + 
            ylab("value"))
      })
    }, 
    calculateHV = function() {
      return(self$results$log$fitness.domHV[self$nr.iterations])
    }
  ), 
  private = list(
    feature.names = NULL,
    range = NULL,
    sdev = NULL,
    param.set= NULL,
    ecrresults = NULL,
    results.orig = NULL,
    log = NULL,
    set.x.interest = function(x.interest) {
      assert_data_frame(x.interest, any.missing = FALSE, all.missing = FALSE, 
        nrows = 1, null.ok = FALSE)
      if(any(!(self$predictor$data$feature.names %in% colnames(x.interest)))) {
        stop("colnames of x.interest must be identical to training data")
      }
      x.interest = x.interest[setdiff(colnames(x.interest), self$predictor$data$y.names)]
      self$y.hat.interest = self$predictor$predict(x.interest)[1,]
      self$x.interest = x.interest
    }, 
    flush = function() {
      private$ecrresults = NULL
      self$results = NULL
      private$finished = FALSE
    },
    run = function(force = FALSE) {
      if (force) private$flush()
      if (!private$finished) {
        private$ecrresults = private$search()
        self$results = private$aggregate()
        private$finished = TRUE
      }
    },
    search = function() {
      
      # Define reference point for hypervolumn compuation
      self$ref.point = c(max(abs(self$y.hat.interest - self$target)), 1,
        ncol(self$x.interest))
      
      # Initialize population based on x.interest, param.setand sdev
      lower = self$x.interest[names(private$sdev)] - private$sdev
      upper = self$x.interest[names(private$sdev)] + private$sdev
      lower.ps = pmax(ParamHelpers::getLower(private$param.set), lower)
      upper.ps = pmin(ParamHelpers::getUpper(private$param.set), upper) 
      ps.initialize = ParamHelpers::makeParamSet(params = makeParamlist(
        self$predictor$data$get.x(), 
        lower = lower.ps, 
        upper = upper.ps)
      )
      set.seed(self$seed)
      initial.pop = ParamHelpers::sampleValues(ps.initialize, self$mu, 
        discrete.names = TRUE)
      
      i = sapply(self$x.interest, is.factor)
      x.interest[i] = lapply(self$x.interest[i], as.character)
      
      #%%%%%%%%%%%%%%%%%%%%
      initial.pop = lapply(initial.pop, function(x) {
        x = transformToOrig(x, x.interest, delete.use.orig = FALSE, 
          fixed.features = self$fixed.features, max.changed = self$max.changed)
      })
      
      # Create fitness function with package smoof
      fn = smoof::makeMultiObjectiveFunction(
        has.simple.signature = FALSE, par.set = private$param.set, n.objectives = 3, 
        noisy = TRUE, ref.point = self$ref.point,
        fn = function(x, fidelity = NULL) {
          fitness.fun(x, x.interest = x.interest, target = self$target, 
            predictor = self$predictor,
            range = private$range, param.set = private$param.set)
        })
      n.objectives = smoof::getNumberOfObjectives(fn) 
      
      # Define operators based on parameterset private$param.set
      # Messages can be ignored
      sdev.l = sdevToList(private$sdev, private$param.set)
      mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
        numeric = ecr::setup(mosmafs::mutGaussScaled, p = 1, sdev = sdev.l$numeric),
        integer = ecr::setup(mosmafs::mutGaussIntScaled, p = 1, sdev = sdev.l$numeric),
        #numeric = ecr::setup(custom.mutGauss, p = 1, sdev = sdev.l$numeric),
        #integer = ecr::setup(custom.mutGaussInt, p = 1, sdev = sdev.l$integer),
        discrete = ecr::setup(mosmafs::mutRandomChoice, p = 1),
        logical = ecr::setup(ecr::mutBitflip, p = 1),
        use.orig = ecr::setup(ecr::mutBitflip, p = self$p.mut.gen),
        .binary.discrete.as.logical = TRUE))
      
      recombinator = suppressMessages(mosmafs::combine.operators(private$param.set,
        numeric = ecr::setup(ecr::recSBX, p = 1),
        integer = ecr::setup(mosmafs::recIntSBX, p = 1),
        discrete = ecr::setup(mosmafs::recPCrossover, p = 1),
        logical = ecr::setup(mosmafs::recPCrossover, p = 1),
        use.orig = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
        .binary.discrete.as.logical = TRUE))
      
      overall.mutator = ecr::makeMutator(function(ind) {
        transformToOrig(mutator(ind), x.interest, delete.use.orig = FALSE, 
          fixed.features = self$fixed.features, max.changed = self$max.changed)
      }, supported = "custom")
      
      overall.recombinator <- ecr::makeRecombinator(function(inds, ...) {
        inds <- recombinator(inds)
        do.call(wrapChildren, lapply(inds, function(x) { 
          transformToOrig(x, x.interest, delete.use.orig = FALSE, 
            fixed.features = self$fixed.features, max.changed = self$max.changed) 
        }))
      }, n.parents = 2, n.children = 2)
      
      parent.selector = ecr::selSimple
      
      survival.selector = ecr::setup(selNondom, 
        epsilon = self$epsilon, 
        consider.diverse.solutions = TRUE, 
        extract.duplicates = TRUE)
      
      # Extract algorithm information with a log object
      log.stats = list(fitness = lapply(
        seq_len(n.objectives), 
        function(idx) {
          list(min = function(x) min(x[idx, ]), mean = function(x) mean(x[idx, ]))
        }))
      
      names(log.stats$fitness) <- sprintf("obj.%s", seq_len(n.objectives))
      log.stats$fitness <- unlist(log.stats$fitness, recursive = FALSE)
      log.stats$fitness <- c(log.stats$fitness,
        list(domHV = function(x) ecr::computeHV(x,
          ref.point = self$ref.point), 
          delta = function(x) ecr:::emoaIndDelta(x[c(1,2),]), 
          spacing = function(x) ecr:::emoaIndSP(x, "euclidean")))
      
      # Compute counterfactuals
      set.seed(self$seed)
      results = mosmafs::slickEcr(fn, lambda = self$mu, population = initial.pop,
        mutator = overall.mutator,
        recombinator = overall.recombinator, generations = self$nr.iterations,
        parent.selector = parent.selector,
        survival.strategy = selectDiverse,
        survival.selector = survival.selector,
        p.recomb = self$p.rec,
        p.mut = self$p.mut, log.stats = log.stats)

      # results = mosmafs::slickEcr(fn, self$mu, population = initial.pop, 
      #   mutator = private$mutator, 
      #   recombinator = private$recombinator, generations = self$nr.iterations, 
      #   parent.selector = private$parent.selector, p.recomb = self$p.rec, 
      #   p.mut = self$p.mut, log.stats = log.stats)
      # 
      # results$pareto.set = lapply(results$pareto.set, function(x) {
      #   transformToOrig(x, self$x.interest,delete.use.orig = FALSE,
      #     fixed.features = TRUE)
      # })
      
      #%%%%%%%%%%%%%%%%%%%%
      # Compute counterfactuals 
      # set.seed(self$seed)
      # results = generateCounterfactuals(x.interest = self$x.interest,
      #   target = self$target, predictor = self$predictor, param.set = private$param.set,
      #   fixed.features = self$fixed.features,
      #   mu = self$mu, lambda = self$mu, p.recomb = self$p.rec, p.mut = self$p.mut,
      #   range.features = private$range, initial.solutions = initial.pop,
      #   survival.selector = survival.selector,
      #   parent.selector = parent.selector,
      #   mutator = mutator, recombinator = recombinator,
      #   number.iterations = self$nr.iterations, ref.point = self$ref.point,
      #   stag = self$stag)
      cat("run finished\n")
      return(results)
    },
    
    aggregate = function(subset.results = self$subset.results) {
      
      pareto.front = private$ecrresults$pareto.front
      names(pareto.front) = c("dist.target", "dist.x.interest", "nr.changed")
      
      pareto.set.l = lapply(private$ecrresults$pareto.set, function(x)
        mosmafs::valuesFromNames(private$param.set, x))
      pareto.set = listToDf(pareto.set.l)
      
      if (subset.results) {
        idx = unique(c(which.min(pareto.front$dist.target), 
          which.min(pareto.front$dist.x.interest), 
          getDiverseSolutions(pareto.front, pareto.set, private$range, 
            self$nr.solutions)))
        message("only a subset of solutions is extracted based on diversity measure")
      } else {
        idx = 1:nrow(pareto.set)
      }
      pareto.set = pareto.set[idx, ]
      pareto.front = pareto.front[idx, ]
      
      pareto.set.diff = getDiff(pareto.set, self$x.interest)
      pred = private$run.prediction(pareto.set)
      
      pareto.set.pf = cbind(pareto.set, pred, pareto.front)
      pareto.set.pf = pareto.set.pf[order(pareto.set.pf$dist.target),]
      rownames(pareto.set.pf) = NULL
      pareto.set.diff.pf = cbind(pareto.set.diff, pred, pareto.front) 
      pareto.set.diff.pf = pareto.set.diff.pf[order(pareto.set.diff.pf$dist.target),]
      rownames(pareto.set.diff.pf) = NULL
      
    
      log = mosmafs::getStatistics(private$ecrresults$log)
      names(log)[2:7] = c("dist.target.min", "dist.target.mean", 
        "dist.x.interest.min", "dist.x.interest.mean", "nr.changed.min", 
        "nr.changed.mean")
      pop = mosmafs::getPopulations(private$ecrresults$log)
      div = unlist(lapply(pop, 
        FUN = function(x) {
          pop_gen = x$population
          pop_gen = lapply(pop_gen, function(k) {
            k$use.orig = NULL
            return(k)
          })
          dis = StatMatch::gower.dist(data.x = listToDf(pop_gen), 
            rngs = private$range)
          single.dist = dis[lower.tri(dis)]
          mean.dist = mean(single.dist)
          pop.div = sum(abs(single.dist - mean.dist)/length(pop_gen))
        }))
      log$population.div = div
      
      # private$results.orig$counterfactuals = pareto.set.pf.pf
      # private$results.orig$counterfactuals.diff = pareto.set.diff.pf
      # private$results.orig$log = log
      
      results = list()
      results$counterfactuals = pareto.set.pf
      results$counterfactuals.diff = pareto.set.diff.pf
      results$log = log
      # results$counterfactuals = roundDF(pareto.set.pf, 3)
      # results$counterfactuals.diff = roundDF(pareto.set.diff.pf, 3)
      # results$log = roundDF(log, 3)
      cat("aggregate finished\n")
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
plot.Counterfactuals = function(object, labels = FALSE) {
  object$plotPF(labels)
}

plotStatistics.Counterfactuals = function(object, range = FALSE) {
  object$plotStatistics(range)
}


