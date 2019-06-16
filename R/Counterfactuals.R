#' Counterfactual Explanations
#' 
#' \code{Counterfactuals} are calculated with a modified version of NSGA-II, 
#' available in package mosmafs, which is based on package ecr.
#' 
#' @format \code{\link{R6Class}} object.
#' @name Counterfactuals
#' @section Usage:
#' \preformatted{
#' counterfactual = Counterfactuals$new(predictor, x.interest = NULL, target = NULL, 
#' epsilon = NULL, fixed.features = NULL, max.changed = NULL, 
#' mu = 50, generations = 50, p.mut = 0.2, p.rec = 0.9, p.mut.gen = 0.5,
#' p.mut.use.orig = 0.2, p.rec.gen = 0.7, p.rec.use.orig = 0.7,
#' use.ice.curve.var = FALSE)
#' 
#' plot(counterfactual)
#' counterfactual$results
#' counterfactual$log
#' print(counterfactual)
#' counterfactual$explain(x.interest, target)
#' counterfactual$subset_results(nr.solutions)
#' counterfactuals$continue_search(generations)
#' counterfactual$plot_statistics()
#' counterfactual$calculate_hv()
#' counterfactual$calculate_diversity()
#' }
#' @section Arguments: 
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{x.interest: }{(data.frame)\cr  Single row with the instance to be explained.}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a single numeric or 
#' a vector of two numerics, to define a desired interval of outcome.}
#' \item{epsilon: }{(numeric(1))\cr Soft constraint. If chosen, candidates, whose
#' distance between their prediction and target exceeds epsilon, are penalized.
#' Default `NULL`.}
#' \item{fixed.features: }{(character|numeric)\cr 
#' Feature name or index for which no deviation from values of x.interest are allowed. 
#' Index refers to ordering of feature names of data used to initialize predictor.
#' Default `NULL`.} 
#' \item{max.changed: }{integer(1)\cr Maximum number of features that can be changed.
#' Default `NULL`.}
#' \item{mu: }{(integer(1))\cr Number of individuals in each generation and 
#' number of nearly generated individuals in each generation.
#' Default `50`.}
#' \item{generations: }{(integer(1))\cr Number of generations. 
#' For `Counterfactual$new()` default is `100`.}
#' \item{p.mut: }{numeric(1)\cr Probability to apply mutation to a child. 
#' Default is `0.2`.}
#' \item{p.rec: }{numeric(1)\cr Probability to apply recombination to a child. 
#' Default is `0.9`.}
#' \item{p.mut.gen:}{numeric(1)\cr Probability of mutation for each gene. 
#' Default is `0.5`.}
#' \item{p.mut.use.orig:}{numeric(1)\cr Probability of mutation for each element
#' of the indicator to use feature values of `x.interest`. Default is `0.2`.} 
#' \item{p.rec.gen:}{numeric(1)\cr Probability of recombination for each gene. 
#' Default is `0.7`.}
#' #' \item{p.rec.use.orig:}{numeric(1)\cr Probability of recombination for each 
#' element of the indicator to use feature values of `x.interest`. 
#' Default is `0.7`.} 
#' \item{use.ice.curve.var:}{logical(1)\cr Whether ICE curve variance should be used to 
#' initialize population. Default is `FALSE`.}
#' \item{nr.solutions}{integer(1)\cr Number of solutions to be extracted from results.}
#' }
#' 
#' @section Details:
#' For more details on the method see:  
#' https://christophm.github.io/interpretable-ml-book/counterfactual.html\cr
#' \cr
#' For more details on the algorithm NSGA-II see: 
#' https://ieeexplore.ieee.org/document/996017
#' 
#' @section Fields:
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' Object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{results: }{(list)\cr Object holding searching results: \cr
#' (1) `data.frame` of found counterfactuals \cr
#' (2) `data.frame` of calculated feature differences compared to x.interest}
#' \item{log: }{(data.frame) \cr Object holding information on each generation: 
#' Minimum and mean of objectives, dominated hypervolume, spacing and diversity.}
#' \item{x.interest: }{(data.frame)\cr Single row with the instance to be explained.}
#' \item{y.hat.interest: }{(numeric)\cr Predicted value for instance of interest}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a single numeric or 
#' a vector of two numerics, to define a desired interval of outcome.}
#' \item{epsilon: }{(numeric(1))\cr Maximal accepted absolute distance from target.}
#' \item{fixed.features: }{(character)\cr 
#' Feature names for which no deviation from values of x.interest are allowed.
#' If NULL, all features are allowed to deviate.} 
#' \item{max.changed: }{integer(1)\cr Maximum number of features that can be changed.
#' If NULL, no limit is set.}
#' \item{mu: }{(integer(1))\cr Number of individuals in each generation and 
#' number of nearly generated individuals in each generation.}
#' \item{generations: }{(integer(1))\cr Number of generations.}
#' \item{p.mut: }{numeric(1)\cr Probability to apply mutation to a child. Default is 0.2}
#' \item{p.rec: }{numeric(1)\cr Probability to apply recombination to a child.}
#' \item{p.mut.gen:}{numeric(1)\cr Probability of mutation for each gene.}
#' \item{p.rec.gen:}{numeric(1)\cr Probability of recombination for each gene.}
#' \item{lower:}{numeric\cr Vector of minimal values for numeric features. If NULL
#' lower is extracted from input data specified in field `data` of `predictor`.}
#' \item{upper:}{numeric\cr Vecotor of maximal values for numeric features.}
#' \item{use.ice.curve.var:}{logical(1)\cr Whether ICE curve variance should be used to 
#' initialize population.}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{\code{explain(x.interest, target)}}{Method to set a new data point which to explain.}
#' \item{\code{plot()}}{Method to plot the pareto front with or without labels informing 
#' about which features where changed. See \link{plot.Counterfactuals}}
#' \item{\code{plotStatistics()}}{Method to plot information of `Counterfactuals$log` 
#' for evaluation of algorithm.}
#' \item{\code{continue_search(generations)}}{Method to continue search 
#' after run was also already finished. Results are added automatically to 
#' `Counterfactuals$results`.}
#' \item{\code{calculate_hv()}}{Extract dominated hypervolume of final Pareto front 
#' from `Counterfactuals$log`, equal to `fitness.dominatedHV` of last row.}
#' \item{\code{calculate_diversity()}}{Extract diversity of final Pareto front
#' from `Counterfactuals$log`, equal to `population.div` of last row.}
#' \item{\code{clone()}}{[internal] Method to clone the `R6` object.}
#' \item{\code{initialize()}}{[internal] Method to initialize the R6 object.}
#' \item{\code{subset_results(nr.solutions)}}{Method to subset 
#' `Counterfactuals$results` to a given number of solutions.}
#' }
#'
#' @references 
#' \describe{
#' \item{Bossek, J. (2017). ecr 2.0: A modular framework for evolutionary computation in r,
#' Proceedings of the Genetic and Evolutionary Computation Conference Companion,
#' GECCO '17, pp. 1187-1193.}
#' \item{Deb, K., Pratap, A., Agarwal, S. and Meyarivan, T. (2002). A fast and elitist multiobjective
#' genetic algorithm: Nsga-ii, IEEE Transactions on Evolutionary Computation
#' 6(2): 182-197.}
#' } 
#' 
#' @seealso 
#' \link{Counterfactuals}
#' 
#' @seealso 
#' A different way to explain predictions: \link{LocalModel}, \link{Shapley}
#' 
#' @examples 
#' if (require("randomForest")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' rf =  randomForest(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#'
#' # Then we explain the first instance of the dataset with the Shapley method:
#'x.interest = X[1,]
#'target = 30
#'counterfactual = Counterfactuals$new(mod, x.interest = x.interest, target = target, 
#'  generations = 100)
#'counterfactual
#'
#' # Look at the results in a table
#' counterfactual$results
#' # Or as a plot
#' plot(counterfactual)
#' plot(counterfactual, labels = TRUE)
#'
#' # Explain another instance
#' counterfactual$explain(X[2,], target = target)
#' plot(counterfactual)
#' ## Not run: 
#' # Counterfactuals() can only focus on one class, not multiple classes at a time
#' rf = randomForest(Species ~ ., data = iris)
#' X = iris[-which(names(iris) == "Species")]
#' mod = Predictor$new(rf, data = X, type = "prob", class = "setosa")
#'
#' # Then we explain the first instance of the dataset with the counterfactuals() method:
#' counterfactuals = Counterfactuals$new(mod, x.interest = X[1,], target = 0)
#' counterfactuals$results
#' plot(counterfactuals) 
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
    generations  = NULL,
    p.mut  = NULL,
    p.rec  = NULL,
    p.mut.gen  = NULL,
    p.mut.use.orig = NULL, 
    p.rec.gen  = NULL,
    p.rec.use.orig = NULL,
    use.ice.curve.var = NULL,
    crow.dist.version = NULL, 
    lower = NULL,
    upper = NULL,
    log = NULL,
    initialize = function(predictor, x.interest = NULL, target = NULL, 
      epsilon = NULL, fixed.features = NULL, max.changed = NULL, 
      mu = 50, generations = 50, p.mut = 0.2, p.rec = 0.9, p.mut.gen = 0.5,
      p.mut.use.orig = 0.2, p.rec.gen = 0.7, p.rec.use.orig = 0.7,
      use.ice.curve.var = FALSE, crow.dist.version = 1, 
      lower = NULL, upper = NULL) {
      
      super$initialize(predictor = predictor)
      fixed.features = private$sanitize_feature(fixed.features, predictor$data$feature.names)
      
      # can be missing
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      if (!is.null(target) && all(sapply(target, is.infinite))) {
        stop("One element of target must be finite")
      }
      checkmate::assert_number(epsilon, null.ok = TRUE)
      checkmate::assert_integerish(max.changed, null.ok = TRUE, len = 1)
      
      # should exist
      checkmate::assert_integerish(mu, lower = 1)
      assert(
        checkInt(generations, lower = 0),
        checkList(generations, types = "function")
      )
      checkmate::assert_number(p.mut, lower = 0, upper = 1)
      checkmate::assert_number(p.rec, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.gen, lower = 0, upper = 1)
      checkmate::assert_true(crow.dist.version %in% c(1, 2))
      checkmate::assert_numeric(lower, null.ok = TRUE, any.missing = FALSE)
      checkmate::assert_numeric(upper, null.ok = TRUE, any.missing = FALSE)
      
      # assign
      self$target = target
      self$epsilon = epsilon
      self$max.changed = max.changed
      self$fixed.features = fixed.features
      self$mu = mu
      self$generations = generations
      self$p.mut = p.mut
      self$p.rec = p.rec 
      self$p.mut.gen = p.mut.gen
      self$p.mut.use.orig = p.mut.use.orig
      self$p.rec.gen = p.rec.gen
      self$p.rec.use.orig = p.rec.use.orig
      self$use.ice.curve.var = use.ice.curve.var
      self$crow.dist.version = crow.dist.version
      self$lower = lower
      self$upper = upper
      
      # Define parameterset
      private$param.set= ParamHelpers::makeParamSet(
        params = make_paramlist(predictor$data$get.x(), 
        lower = lower, upper = upper))
      
      # Extract info from input.data
      private$range = ParamHelpers::getUpper(private$param.set) - 
        ParamHelpers::getLower(private$param.set)
      private$range[ParamHelpers::getParamIds(private$param.set)
        [ParamHelpers::getParamTypes(private$param.set) == "discrete"]]  = NA
      private$range = private$range[predictor$data$feature.names]
      private$sdev = apply(Filter(is.numeric, predictor$data$get.x()), 2, sd)
      
      
      # Set x.interest     
      x.interest = x.interest[setdiff(colnames(x.interest), predictor$data$y.names)]
      if (!is.null(x.interest)) {
        private$set_x_interest(x.interest)
      }
      if (!is.null(x.interest) & !is.null(target)) {
        private$run()
      }
      cat("initialize finished\n")
    }, 
    explain = function(x.interest, target) {
      checkmate::assert_numeric(target, min.len = 1, 
        max.len = 2, any.missing = FALSE, null.ok = FALSE)
      if (all(sapply(target, is.infinite))) {
        stop("One element of target must be finite")
      }
      checkmate::assert_true
      self$target = target
      private$set_x_interest(x.interest)
      private$flush()
      private$run()
      return(self)
    },
    subset_results = function(nr.solutions) {
      if (nr.solutions > nrow(self$results$counterfactuals)) {
        warning("nr.solutions out of range, was set to 
          number of solutions in self$results")
        nr.solutions = nrow(self$results$counterfactuals)
      }
      assert_integerish(nr.solutions, lower = 1)
      idx = get_diverse_solutions(self$results$counterfactuals[, private$obj.names],
        self$results$counterfactuals[, self$predictor$data$feature.names], 
        nr.solutions)
      results.subset = self$results
      results.subset$counterfactuals = results.subset$counterfactuals[idx, ]
      rownames(results.subset$counterfactuals) = NULL
      results.subset$counterfactuals.diff = results.subset$counterfactuals.diff[idx,]
      rownames(results.subset$counterfactuals.diff) = NULL
      return(results.subset)
    },
    plot_statistics = function() {
      min.obj = c("generation", "dist.target.min", 
        "dist.x.interest.min", "nr.changed.min")     
      mean.obj = c("generation", "dist.target.mean", 
        "dist.x.interest.mean", "nr.changed.mean")
      eval = c("generation", "fitness.domHV", 
        #"fitness.delta", 
        #"fitness.spacing",
        "population.div")
      nameList = list(min.obj, mean.obj, eval)
      # if (range) {
      #   log = mlr::normalizeFeatures(self$log, method = "range", 
      #     cols = names(self$log)[!names(self$log) %in% c("generation", "state")])
      # } else {
        log = self$log
      #}
      p = lapply(nameList, function(nam) {
        df = melt(log[,nam] , id.vars = "generation", variable.name = "legend")
        singlep = ggplot(df, aes(generation, value)) + geom_line(aes(colour = legend)) + 
            ylab("value")
        return(singlep)
      })
      p
    },
    plot_search = function() {
      pf.over.gen = lapply(seq_len(nrow(self$log)), 
        FUN = function(i) {
          pf.gen = as.data.frame(t(private$ecrresults$log$env$pop[[i]]$fitness))
          names(pf.gen) = paste("y", 1:3, sep = "")
          pf.gen$generation = i-1
          pf.gen
        })
      
      pf.over.gen.df = do.call(rbind, pf.over.gen)
      
      pfPlot = ggplot(data = pf.over.gen.df, aes(x=y1, y=y2, alpha = generation)) +
        geom_point(col = "black")+
        xlab(private$obj.names[1]) +
        ylab(private$obj.names[2])
      
      pfPlot
    },
    continue_search = function(generations) {
      private$ecrresults = continueEcr(ecr.object = private$ecrresults, generations = generations)
      results = mosmafs::listToDf(private$ecrresults$pareto.set, private$param.set)
      results[, grepl("use.orig", names(results))] = NULL
      private$dataDesign = results
      private$qResults = private$run.prediction(results)
      self$results = private$aggregate()    
    },
    calculate_hv = function() {
      return(tail(self$log$fitness.domHV, 1))
    }, 
    calculate_diversity = function() {
      return(tail(self$log$population.div, 1))
    }, 
    calculate_freq = function(plot = FALSE) {
      diff = self$results$counterfactuals.diff
      diff = diff[!(names(diff) %in% c(private$obj.names, "pred"))]
      freq = colSums(diff != 0)/nrow(diff)
      if (plot) {
        barplot(freq, ylim = c(0, 1))
      }
      return(freq)
    }
  ), 
  private = list(
    featurenames = NULL,
    range = NULL,
    ref.point = NULL,
    sdev = NULL,
    param.set= NULL,
    param.set.init = NULL,
    ecrresults = NULL,
    obj.names = c("dist.target", "dist.x.interest", "nr.changed"),
    set_x_interest = function(x.interest) {
      assert_data_frame(x.interest, any.missing = FALSE, all.missing = FALSE, 
        nrows = 1, null.ok = FALSE)
      if(any(!(self$predictor$data$feature.names %in% colnames(x.interest)))) {
        stop("colnames of x.interest must be identical to training data")
      }
      x.interest = x.interest[setdiff(colnames(x.interest), self$predictor$data$y.names)]
      if (any(colnames(x.interest) != self$predictor$data$feature.names)) {
        warning("columns of x.interest were reordered according to predictor$data$feature.names")
        x.interest = x.interest[, self$predictor$data$feature.names]
      }
      x.interest.list = as.list(x.interest)
      x.interest.list$use.orig = rep(TRUE, ncol(x.interest))
      if (!ParamHelpers::isFeasible(private$param.set, x.interest.list)) {
        stop(paste("Feature values of x.interest outside range of training data",
          "of predictor or given arguments lower or upper. Please modify arguments",
          "lower or upper accordingly."))
      }
      self$y.hat.interest = self$predictor$predict(x.interest)[1,]
      self$x.interest = x.interest
    }, 
    flush = function() {
      private$ecrresults = NULL
      self$results = NULL
      private$finished = FALSE
    },
    intervene = function() {
      
      # Define reference point for hypervolumn compuation
      private$ref.point = c(min(abs(self$y.hat.interest - self$target)), 
        1, ncol(self$x.interest))
      if (is.infinite(private$ref.point[1])) {
        pred = self$predictor$predict(self$predictor$data$get.x())
        private$ref.point[1] = diff(c(min(pred), max(pred)))
      }
      # Initialize population based on x.interest, param.setand sdev
      lower = self$x.interest[names(private$sdev)] - private$sdev
      upper = self$x.interest[names(private$sdev)] + private$sdev
      lower.ps = pmax(ParamHelpers::getLower(private$param.set), lower)
      upper.ps = pmin(ParamHelpers::getUpper(private$param.set), upper)
      lower.ps[names(self$lower)] = self$lower
      upper.ps[names(self$upper)] = self$upper
      ps.initialize = ParamHelpers::makeParamSet(params = make_paramlist(
        self$predictor$data$get.x(), 
        lower = lower.ps, 
        upper = upper.ps)
      )
      private$param.set.init = ps.initialize
      initial.pop = ParamHelpers::sampleValues(ps.initialize, self$mu, 
        discrete.names = TRUE)
      if (self$use.ice.curve.var) {
        ice.var = get_ICE_var(self$x.interest, self$predictor, private$param.set)
        prob.use.orig = 1 - mlr::normalizeFeatures(as.data.frame(ice.var), 
          method = "range", range = c(0.01, 0.99))
        # distribution = function() vapply(t(prob.use.orig), FUN.VALUE = numeric(1), 
        # function(x) sample(c(0, length(initial.pop[[1]]$use.orig)), 1, prob = c(1-x, x)))
        ilen = length(initial.pop[[1]]$use.orig)
        distribution = function() rbinom(n = ilen, size = ilen, 
          prob = t(prob.use.orig))
        initial.pop = initSelector(initial.pop, vector.name = "use.orig", 
          distribution = distribution)
      }
      
      i = sapply(self$x.interest, is.factor)
      x.interest = self$x.interest
      x.interest[i] = lapply(self$x.interest[i], as.character)

      initial.pop = lapply(initial.pop, function(x) {
        x = transform_to_orig(x, x.interest, delete.use.orig = FALSE, 
          fixed.features = self$fixed.features, max.changed = self$max.changed)
      })
      
      # Create fitness function with package smoof
      fn = smoof::makeMultiObjectiveFunction(
        has.simple.signature = FALSE, par.set = private$param.set, n.objectives = 3, 
        noisy = TRUE, ref.point = private$ref.point,
        fn = function(x, fidelity = NULL) {
          fitness_fun(x, x.interest = self$x.interest, target = self$target, 
            predictor = self$predictor, range = private$range)
        })
      
      fn = mosmafs::setMosmafsVectorized(fn)
      n.objectives = smoof::getNumberOfObjectives(fn) 
      
      # Define operators based on parameterset private$param.set
      # Messages can be ignored
      sdev.l = sdev_to_list(private$sdev, private$param.set)
      mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
        numeric = ecr::setup(mosmafs::mutGaussScaled, p = self$p.mut.gen, sdev = sdev.l$numeric),
        integer = ecr::setup(mosmafs::mutGaussIntScaled, p = self$p.mut.gen, sdev = sdev.l$integer),
        #numeric = ecr::setup(custom.mutGauss, p = self$p.mut.gen, sdev = sdev.l$numeric),
        #integer = ecr::setup(custom.mutGaussInt, p = self$p.mut.gen, sdev = sdev.l$integer),
        #race = ecr::setup(mosmafs::mutBitflip, p = self$p.mut.gen),
        discrete = ecr::setup(mosmafs::mutRandomChoice, p = self$p.mut.gen),
        logical = ecr::setup(ecr::mutBitflip, p = self$p.mut.gen),
        use.orig = ecr::setup(ecr::mutBitflip, p = self$p.mut.use.orig),
        .binary.discrete.as.logical = TRUE))
      
      recombinator = suppressMessages(mosmafs::combine.operators(private$param.set,
        numeric = ecr::setup(ecr::recSBX, p = self$p.rec.gen),
        integer = ecr::setup(mosmafs::recIntSBX, p = self$p.rec.gen),
        discrete = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
        logical = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
        use.orig = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.use.orig),
        .binary.discrete.as.logical = TRUE))
      
      overall.mutator = ecr::makeMutator(function(ind) {
        transform_to_orig(mutator(ind), x.interest, delete.use.orig = FALSE, 
          fixed.features = self$fixed.features, max.changed = self$max.changed)
      }, supported = "custom")
      
      overall.recombinator <- ecr::makeRecombinator(function(inds, ...) {
        inds <- recombinator(inds)
        do.call(ecr::wrapChildren, lapply(inds, function(x) { 
          transform_to_orig(x, x.interest, delete.use.orig = FALSE, 
            fixed.features = self$fixed.features, max.changed = self$max.changed) 
        }))
      }, n.parents = 2, n.children = 2)

      parent.selector = ecr::selSimple
      
      survival.selector = ecr::setup(select_nondom, 
        epsilon = self$epsilon,  
        extract.duplicates = TRUE, vers = self$crow.dist.version)
      
      # Extract algorithm information with a log object
      log.stats = list(fitness = lapply(
        seq_len(n.objectives), 
        function(idx) {
          list(min = function(x) min(x[idx, ]), mean = function(x) mean(x[idx, ]))
        }))
      
      names(log.stats$fitness) = sprintf("obj.%s", seq_len(n.objectives))
      log.stats$fitness = unlist(log.stats$fitness, recursive = FALSE)
      max.hv = ecr::computeHV(matrix(c(0, 0, 0)), private$ref.point)
      log.stats$fitness = c(log.stats$fitness,
        list(domHV = function(x) ecr::computeHV(x,
          ref.point = private$ref.point)/max.hv
          #delta = function(x) ecr:::emoaIndDelta(x[c(1,2),]), 
          #spacing = function(x) spacing(t(x), "manhattan")
          ))
      
      # Compute counterfactuals
      ecrresults = mosmafs::slickEcr(fn, lambda = self$mu, population = initial.pop,
        mutator = overall.mutator,
        recombinator = overall.recombinator, generations = self$generations,
        parent.selector = parent.selector,
        survival.strategy = select_diverse,
        survival.selector = survival.selector,
        p.recomb = self$p.rec,
        p.mut = self$p.mut, log.stats = log.stats)
      
      private$ecrresults = ecrresults
      # pareto.set.l = lapply(private$ecrresults$pareto.set, function(x)
      #   mosmafs::valuesFromNames(private$param.set, x))
      results = mosmafs::listToDf(ecrresults$pareto.set, private$param.set)
      results[, grepl("use.orig", names(results))] = NULL
      cat("run finished\n")
      return(results)
    },
    aggregate = function() {
      
      # Fill results list
      pareto.front = private$ecrresults$pareto.front
      names(pareto.front) = private$obj.names
      
      pareto.set = private$dataDesign
      
      pareto.set.diff = get_diff(pareto.set, self$x.interest)
      pred = private$qResults
      names(pred) = "pred"
      
      pareto.set.pf = cbind(pareto.set, pred, pareto.front)
      pareto.set.pf = pareto.set.pf[order(pareto.set.pf$dist.target),]
      rownames(pareto.set.pf) = NULL
      pareto.set.diff.pf = cbind(pareto.set.diff, pred, pareto.front) 
      pareto.set.diff.pf = pareto.set.diff.pf[order(pareto.set.diff.pf$dist.target),]
      rownames(pareto.set.diff.pf) = NULL
      
      results = list()
      results$counterfactuals = pareto.set.pf
      results$counterfactuals.diff = pareto.set.diff.pf

      # Add diversity to log data frame
      pop = mosmafs::getPopulations(private$ecrresults$log)
      div = unlist(lapply(pop, 
        FUN = function(x) {
          pop_gen = mosmafs::listToDf(x$population, private$param.set)
          pop_gen[, grepl("use.orig", names(pop_gen))] = NULL
          dis = StatMatch::gower.dist(data.x = pop_gen, 
            rngs = private$range)
          single.dist = dis[lower.tri(dis)]
          mean.dist = mean(single.dist)
          pop.div = sum(abs(single.dist - mean.dist)/nrow(pop_gen))
        }))
      log = mosmafs::getStatistics(private$ecrresults$log)
      log$population.div = div
      nam = c("generation", "dist.target.min", "dist.target.mean", 
        "dist.x.interest.min", "dist.x.interest.mean", "nr.changed.min", 
        "nr.changed.mean")
      names(log)[1:7] = nam
      log = log[c("generation", "state", nam[2:7], "fitness.domHV", 
        #"fitness.delta", 
        #"fitness.spacing", 
        "population.div")]
      self$log = log

      cat("aggregate finished\n")
      return(results)
    },
    generatePlot = function(labels = FALSE, decimal.points = 3, nr.solutions = NULL, 
      nr.changed = NULL) {
      assert_logical(labels)
      assert_integerish(decimal.points, null.ok = !labels)
      assert_int(nr.solutions, null.ok = TRUE)
      assert_integerish(nr.changed, null.ok = TRUE)
      results_diff = self$results$counterfactuals.diff
      if (!is.null(nr.solutions)) {
        results_diff = self$subset_results(nr.solutions)$counterfactuals.diff
      }
      if (!is.null(nr.changed)) {
        results_diff = results_diff[results_diff$nr.changed %in% nr.changed, ]
      }
      pf = results_diff[, private$obj.names]
      
      p = ggplot(data = pf, aes(x=dist.target, y=dist.x.interest, 
        color = as.factor(nr.changed))) +
        geom_point() +
        #scale_colour_gradient2(low = "black", mid = "orange", high = "green") +
        xlab("dist target") +
        ylab("dist x.interest") +
        #ggtitle(title)+
        guides(color=guide_legend(title="nr changed"))
      
      if (labels) {
        diffs = results_diff[, !(names(results_diff) %in% c("dist.target", 
          "dist.x.interest", "nr.changed", "pred", "X1"))]
        labels = c()
        for(i in 1:nrow(diffs)) {
          names = names(diffs[i,])[diffs[i,] != 0]
          lab = as.data.frame(diffs[i, names])
          lab = paste(paste(names, round_df(lab, decimal.points)), collapse = " & ")
          labels = c(labels, lab)
        }
        p = p + ggrepel::geom_label_repel(aes(label = labels),
          box.padding   = 0.35, 
          point.padding = 0.4, 
          show.legend = FALSE) 
      }
      p
    },
    sanitize_feature = function(fixed.features, feature.names) {
      if (is.numeric(fixed.features)) {
        assert_integerish(fixed.features, lower = 1, upper = length(feature.names), 
          null.ok = TRUE)
        fixed.features = feature.names[fixed.features]
      }
      assert_character(fixed.features, null.ok = TRUE, unique = TRUE)
      stopifnot(all(fixed.features %in% feature.names))
      fixed.features
    }
))


#' Plot Counterfactuals
#'
#' \code{plot.Counterfactuals()} plots the Pareto front, the found Counterfactuals.
#' @format \code{\link{R6Class}} object.
#' @section Arguments: 
#' \describe{
#' \item{labels:}{logical(1)\cr Whether labels with difference to feature values of 
#' x.interest should be plotted. Default is `FALSE`.}
#' \item{decimal.points:}{integer(1)\cr Number of decimal places used. Default is `3`.}
#' \item{nr.solutions:}{integer(1)\cr Number of solutions showed. Default `NULL` means, 
#' all solutions are showed.}
#' \item{nr.changed:}{integer}\cr Plot only counterfactuals with certain number of 
#' features changed.}
#' }
#' @return ggplot2 plot object
#' @seealso 
#' \link{Counterfactuals}
#' @examples 
#' \dontrun{
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' Boston = Boston
#' set.seed(1000)
#' rf =  randomForest(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#' 
#' # Then we calculate Counterfactuals for the first instance
#' x.interest = X[1,]
#' mod$predict(x.interest)
#' target = 30
#' cf = Counterfactuals$new(mod, x.interest = x.interest, target = target, 
#'   mu = 50, generations = 60)
#'
#' # The results can be plotted
#' plot(cf)
#' plot(cf, labels = TRUE, nr.solutions = 10)
#' }
#' }
#' @export
plot.Counterfactuals = function(object, labels = FALSE, decimal.points = 3, nr.solutions = NULL, nr.changed = NULL) {
  object$plot(labels = labels, decimal.points = decimal.points, nr.solutions = nr.solutions, 
    nr.changed = nr.changed)
}

#' @title Calculate frequency of one feature altered
#' 
#' @description Identify leverages that alter prediction to desired target
#'  over multiple datapoints. Leverages are identified by calculating
#'  the frequency a feature was altered within the set of the 
#'  final calculated counterfactuals.  
#'  
#' @section Arguments: 
#' \describe{
#' \item{counterfactual: }{(Counterfactuals)\cr Instance of class 
#' `Counterfactual` to extract
#' dataset and target, if not needed, as well as all the parameters}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a 
#' single numeric or 
#' a vector of two numerics, to define a desired interval of outcome.}
#' \item{obs: }{(data.frame) data.frame to use to identify leverages 
#' by calculating counterfactuals for them}
#' \item{row.ids }{(integer) Rows with the specific row.ids are extracted
#' from the input data defined in the predictor field of the 
#' `Counterfactuals` class. The subset is used to identify leverages.}
#' \item{plot: }{(logical(1)) whether to plot the frequency over all 
#' observations}
#' }
#' 
#' @export
calculate_freq_wrapper = function(counterfactual, target = NULL, obs = NULL, 
  row.ids = NULL, plot = FALSE) {
  assert_data_frame(obs, null.ok = TRUE)
  assert_integerish(row.ids, null.ok = TRUE)
  assert_numeric(target, min.len = 1, max.len = 2, null.ok = TRUE)
  if (is.null(counterfactual$target) & is.null(target)) {
    stop("target not specified")
  }
  
  df = counterfactual$predictor$data$get.x()
  if (!is.null(obs)) {
    df = obs
  }
  if (!is.null(row.ids)) {
    df = df[row.ids,]
  }
  if(is.null(target)) {
    target = counterfactual$target 
  }
  
  df = as.data.frame(df)
  freq = by(df, 1:nrow(df), function(row) {
    counterfactual = counterfactual$explain(row, target)
    counterfactual$calculate_freq()
  })
  freq = do.call(rbind, freq)
  
  average_freq = colMeans(freq)
  
  if(plot) {
    barplot(average_freq, ylim = c(0, 1))
  }
  return(average_freq)
  
}


