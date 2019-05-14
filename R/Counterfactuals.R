#' Counterfactual Explanations
#' 
#' \code{Counterfactuals} Description
#' Counterfactuals are calculated with a modified version of NSGA-II, 
#' available in package mosmafs, which is based on package ecr.
#' 
#' @format \code{\link{R6Class}} object.
#' @name Counterfactuals
#' @section Usage:
#' \preformatted{
#' counterfactual = Counterfactuals$new(predictor, x.interest = NULL, target = NULL, 
#' epsilon = NULL, fixed.features = NULL, max.changed = NULL, mu = 50, 
#' generations = 100, p.mut = 0.2, p.rec = 1, p.mut.gen = 0.2, p.rec.gen = 0.7)
#' 
#' plot(counterfactual)
#' counterfactual$results
#' print(counterfactual)
#' counterfactual$explain(x.interest, target)
#' counterfactuals$continue_search(generations)
#' }
#' 
#' @section Arguments: 
#' For Counterfactuals$new():
#' \describe{
#' \item{predictor: }{(Predictor)\cr 
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{x.interest: }{(data.frame)\cr  Single row with the instance to be explained.}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a single numeric or 
#' a vector of two numerics, to define a desired interval of outcome.}
#' \item{epsilon: }{(numeric(1))\cr Soft constraint. If chosen, candidates, whose
#' distance between their prediction and target exceeds epsilon, are penalized.}
#' \item{fixed.features: }{(character)\cr 
#' Feature names for which no deviation from values of x.interest are allowed.} 
#' \item{max.changed: }{integer(1)\cr Maximum number of features that can be changed.}
#' \item{mu: }{(integer(1))\cr Number of individuals in each generation and 
#' number of nearly generated individuals in each generation.}
#' \item{generations: }{(integer(1))\cr Number of generations.}
#' \item{p.mut: }{numeric(1)\cr Probability to apply mutation to a child}
#' \item{p.rec: }{numeric(1)\cr Probability to apply recombination to a child}
#' \item{p.mut.gen:}{numeric(1)\cr Probability of mutation for each gene}
#' \item{p.rec.gen:}{numeric(1)\cr Probability of recombination for each gene}
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
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{results: }{(data.frame)\cr data.frame with elements Counterfactuals, 
#' Counterfactuals computed difference to x.interest and log for evaluation}
#' \item{x.interest: }{(data.frame)\cr Single row with the instance to be explained.}
#' \item{y.hat.interest: }{(numeric)\cr Predicted value for instance of interest}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a single numeric or 
#' a vector of two numerics, to define a desired interval of outcome.}
#' \item{epsilon: }{(numeric(1))\cr Maximal accepted absolute distance from target.}
#' \item{fixed.features: }{(character)\cr 
#' Feature names for which no deviation from values of x.interest are allowed.} 
#' \item{max.changed: }{integer(1)\cr Maximum number of features that can be changed.}
#' \item{mu: }{(integer(1))\cr Number of individuals in each generation and 
#' number of nearly generated individuals in each generation.}
#' \item{generations: }{(integer(1))\cr Number of generations.}
#' \item{p.mut: }{numeric(1)\cr Probability to apply mutation to a child}
#' \item{p.rec: }{numeric(1)\cr Probability to apply recombination to a child}
#' \item{p.mut.gen:}{numeric(1)\cr Probability of mutation for each gene}
#' \item{p.rec.gen:}{numeric(1)\cr Probability of recombination for each gene}
#' }
#' 
#' @section Methods:
#' \describe{
#' \item{\code{explain(x.interest, target)}}{method to set a new data point which to explain.}
#' \item{\code{plot(labels)}}{method to plot the pareto front with or without labels informing 
#' about which features where changed. See \link{plot.Counterfactuals}}
#' \item{\code{plotStatistics(range)}}{method to plot log information of results for evaluation of algorithm, 
#' can be normalized to range [0, 1]. See \link {plotStatistics.Counterfactuals}}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
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
    log = NULL,
    initialize = function(predictor, x.interest = NULL, target = NULL, 
      epsilon = NULL, fixed.features = NULL, max.changed = NULL, 
      mu = 50, generations = 100, p.mut = 0.2, p.rec = 1, p.mut.gen = 0.7,
      p.mut.use.orig = 0.2, p.rec.gen = 0.7, p.rec.use.orig = 0.7,
      use.ice.curve.var = FALSE) {
      
      super$initialize(predictor = predictor)
      fixed.features = private$sanitize_feature(fixed.features, predictor$data$feature.names)
      
      # can be missing
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      checkmate::assert_number(epsilon, null.ok = TRUE)
      checkmate::assert_integerish(max.changed, null.ok = TRUE, len = 1)
      if (!is.null(fixed.features) & class(fixed.features) == "numeric") {
        fixed.features = self$predictor$data$feature.names[fixed.features]
      }
      checkmate::assert_character(fixed.features, null.ok = TRUE)
      
      # should be here
      checkmate::assert_integerish(mu, lower = 1)
      assert(
        checkInt(generations, lower = 0),
        checkList(generations, types = "function")
      )
      checkmate::assert_number(p.mut, lower = 0, upper = 1)
      checkmate::assert_number(p.rec, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.gen, lower = 0, upper = 1)
      
      # assign
      x.interest = x.interest[setdiff(colnames(x.interest), predictor$data$y.names)]
      if (!is.null(x.interest)) {
        private$set_x_interest(x.interest)
      }
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
      
      # Define parameterset
      private$param.set= ParamHelpers::makeParamSet(
        params = make_paramlist(predictor$data$get.x()))
      
      # Extract info from input.data
      private$range = ParamHelpers::getUpper(private$param.set) - 
        ParamHelpers::getLower(private$param.set)
      private$range[ParamHelpers::getParamIds(private$param.set)
        [ParamHelpers::getParamTypes(private$param.set) == "discrete"]]  = NA
      private$range = private$range[predictor$data$feature.names]
      private$sdev = apply(Filter(is.numeric, predictor$data$get.x()), 2, sd)
      if (!is.null(x.interest) & !is.null(target)) {
        private$run()
      }
      cat("initialize finished\n")
    }, 
    explain = function(x.interest, target) {
      checkmate::assert_numeric(target, min.len = 1, 
        max.len = 2, any.missing = FALSE)
      self$target = target
      private$set_x_interest(x.interest)
      private$flush()
      private$run()
    },
    subset_results = function(nr.solutions) {
      if (nr.solutions > nrow(self$results$counterfactuals)) {
        warning("nr.solutions > number of non-dominated solutions, was set to 
          number of non-dominated solutions")
        nr.solutions = nrow(self$results$counterfactuals)
      }
      assert_integerish(nr.solutions, lower = 1)
      idx = get_diverse_solutions(self$results$counterfactuals[, private$obj.names],
        self$results$counterfactuals[, self$predictor$data$feature.names], 
        private$range, nr.solutions)
      results.subset = self$results
      results.subset$counterfactuals = results.subset$counterfactuals[idx, ]
      rownames(results.subset$counterfactuals) = NULL
      results.subset$counterfactuals.diff = results.subset$counterfactuals.diff[idx,]
      rownames(results.subset$counterfactuals.diff) = NULL
      return(results.subset)
    },
    plot_statistics = function(range = FALSE) {
      min.obj = c("generation", "dist.target.min", 
        "dist.x.interest.min", "nr.changed.min")     
      mean.obj = c("generation", "dist.target.mean", 
        "dist.x.interest.mean", "nr.changed.mean")
      eval = c("generation", "fitness.domHV", "fitness.delta", 
        "fitness.spacing", "population.div")
      nameList = list(min.obj, mean.obj, eval)
      if (range) {
        log = mlr::normalizeFeatures(self$log, method = "range", 
          cols = names(cf$results$log)[!names(cf$results$log) %in% c("generation", "state")])
      } else {
        log = self$log
      }
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
        geom_point(col = "blue")+
        xlab(private$obj.names[1]) +
        ylab(private$obj.names[2])
      
      pfPlot
    },
    continue_search = function(generations) {
      private$ecrresults = continueEcr(ecr.object = private$ecrresults, generations = generations)
      self$results = private$aggregate()    
    },
    calculate_hv = function() {
      return(tail(self$log$fitness.domHV, 1))
    }, 
    calculate_diversity = function() {
      return(tail(self$log$population.div, 1))
    }
  ), 
  private = list(
    featurenames = NULL,
    range = NULL,
    ref.point = NULL,
    sdev = NULL,
    param.set= NULL,
    ecrresults = NULL,
    obj.names = c("dist.target", "dist.x.interest", "nr.changed"),
    set_x_interest = function(x.interest) {
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
    intervene = function() {
      
      # Define reference point for hypervolumn compuation
      private$ref.point = c(max(abs(self$y.hat.interest - self$target)), 1,
        ncol(self$x.interest))
      
      # Initialize population based on x.interest, param.setand sdev
      lower = self$x.interest[names(private$sdev)] - private$sdev
      upper = self$x.interest[names(private$sdev)] + private$sdev
      lower.ps = pmax(ParamHelpers::getLower(private$param.set), lower)
      upper.ps = pmin(ParamHelpers::getUpper(private$param.set), upper) 
      ps.initialize = ParamHelpers::makeParamSet(params = make_paramlist(
        self$predictor$data$get.x(), 
        lower = lower.ps, 
        upper = upper.ps)
      )
      initial.pop = ParamHelpers::sampleValues(ps.initialize, self$mu, 
        discrete.names = TRUE)
      if (self$use.ice.curve.var) {
        ice.var = get_ICE_var(self$x.interest, self$predictor, private$param.set)
        prob.use.orig = 1 - mlr::normalizeFeatures(as.data.frame(ice.var), 
          method = "range", range = c(0.01, 0.99))
        # distribution = function() vapply(t(prob.use.orig), FUN.VALUE = numeric(1), 
        #   function(x) sample(c(0, length(initial.pop[[1]]$use.orig)), 1, prob = c(1-x, x)))
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
            predictor = self$predictor,
            range = private$range, param.set = private$param.set)
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
          ref.point = private$ref.point), 
          delta = function(x) ecr:::emoaIndDelta(x[c(1,2),]), 
          spacing = function(x) ecr:::emoaIndSP(x, "euclidean")))
      
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
          pop_gen = x$population
          pop_gen = lapply(pop_gen, function(k) {
            k$use.orig = NULL
            return(k)
          })
          dis = StatMatch::gower.dist(data.x = list_to_df(pop_gen), 
            rngs = private$range)
          single.dist = dis[lower.tri(dis)]
          mean.dist = mean(single.dist)
          pop.div = sum(abs(single.dist - mean.dist)/length(pop_gen))
        }))
      log = mosmafs::getStatistics(private$ecrresults$log)
      log$population.div = div
      nam = c("generation", "dist.target.min", "dist.target.mean", 
        "dist.x.interest.min", "dist.x.interest.mean", "nr.changed.min", 
        "nr.changed.mean")
      names(log)[1:7] = nam
      log = log[c("generation", "state", nam[2:7], "fitness.domHV", "fitness.delta", "fitness.spacing", 
        "population.div")]
      self$log = log

      cat("aggregate finished\n")
      return(results)
    },
    generatePlot = function(labels = FALSE, decimal.points = 3, nr.solutions = NULL) {
      assert_logical(labels)
      assert_integerish(decimal.points, null.ok = !labels)
      assert_integerish(nr.solutions, null.ok = TRUE)
      results_diff = self$results$counterfactuals.diff
      if (!is.null(nr.solutions)) {
        results_diff = self$subset_results(nr.solutions)$counterfactuals.diff
      }
      pf = results_diff[, private$obj.names]
      
      p = ggplot(data = pf, aes(x=dist.target, y=dist.x.interest, 
        color = as.factor(nr.changed))) +
        geom_point() +
        xlab("dist target") +
        ylab("dist x.interest") +
        #ggtitle(title)+
        guides(color=guide_legend(title="nr changed"))
      
      if (labels) {
        diffs = results_diff[, !(names(results_diff) %in% c("dist.target", 
          "dist.x.interest", "nr.changed", "pred"))]
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
        assert_numeric(fixed.features, lower = 1, upper = length(feature.names), 
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
#' plot.Counterfactuals() plots the Pareto front, the found Counterfactuals.
#' 
#' @param object  A Counterfactuals R6 object
#' @param labels (logical(1)) Should the labels about the differences 
#' from x.interest be assigned as labels to the points?
#' @param decimal.points (integer(1))
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
#'   mu = 50, generations = 100)
#'
#' # The results can be plotted
#' plot(cf)
#' plot(cf, labels = TRUE)
#' }
#' }
#' @export
plot.Counterfactuals = function(object, labels = FALSE, decimal.points = 3, nr.solutions = NULL) {
  object$plot(labels = labels, decimal.points = decimal.points, nr.solutions = nr.solutions)
}



