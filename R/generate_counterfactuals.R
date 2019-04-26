# slickEcr = mosmafs::slickEcr
# initEcr = mosmafs::initEcr
# continueEcr = mosmafs::continueEcr


generateCounterfactuals = function(x.interest, target, predictor, param.set,
  fixed.features = NULL, mu, lambda, p.recomb, p.mut, initial.solutions, 
  range.features, survival.selector, parent.selector, mutator, recombinator, 
  number.iterations, stag = .Machine$integer.max, ref.point) {
  
  n.objectives = 3 
  log.pop = FALSE
  
  checkmate::assert_data_frame(x.interest, any.missing = FALSE)
  checkmate::assert_numeric(target, any.missing = FALSE, min.len = 1, 
    max.len = 2)
  checkmate::assert_class(predictor, c("Predictor", "R6"))
  checkmate::assert(all(names(x.interest) %in% names(predictor$data$get.x())))
  checkmate::assert_number(mu, lower = 1, finite = TRUE)
  checkmate::assert_number(lambda, lower = 1, finite = TRUE)
  checkmate::assert(mu == lambda)
  mu = asInt(mu, lower = 1L)
  lambda.lower = 1L
  lambda = asInt(lambda, lower = lambda.lower)
  checkmate::assert_number(p.recomb, lower = 0, upper = 1)
  checkmate::assert_number(p.mut, lower = 0, upper = 1)
  checkmate::assert_list(initial.solutions, len = mu)
  checkmate::assert_numeric(range.features, finite = TRUE, null.ok = TRUE)
  checkmate::assert_number(number.iterations, finite = TRUE)
  checkmate::assert_character(fixed.features, null.ok = TRUE, min.len = 1)
  
  
  terminators = list(ecr::stopOnIters(number.iterations), 
    stopOnStag(stag))
  
  control = ecr::initECRControl(fitness.fun = function(){}, n.objectives = n.objectives, 
    minimize = TRUE)
  control$type = "custom"
  control = ecr::registerECROperator(control, "mutate", 
    BBmisc::coalesce(mutator, 
      ecr:::getDefaultEvolutionaryOperators(representation, "mutator", 
      n.objectives, control)))
  control = ecr::registerECROperator(control, "recombine", 
    BBmisc::coalesce(recombinator, 
    ecr:::getDefaultEvolutionaryOperators(representation, "recombinator", 
      n.objectives, control)))
  control = ecr::registerECROperator(control = control, slot = "selectForSurvival", 
    fun = BBmisc::coalesce(survival.selector, getDefaultEvolutionaryOperators("custom", 
      "survival.selector", n.objectives, control)))
  control = ecr::registerECROperator(control, "selectForMating", 
    BBmisc::coalesce(parent.selector, getDefaultEvolutionaryOperators("custom", 
      "parent.selector", n.objectives, control)))
  
  log.stats = list(fitness = lapply(seq_len(n.objectives), function(idx) {
    list(min = function(x) min(x[idx, ]), mean = function(x) mean(x[idx, ]))
  }))
  names(log.stats$fitness) <- sprintf("obj.%s", seq_len(n.objectives))
  log.stats$fitness <- unlist(log.stats$fitness, recursive = FALSE)
  log.stats$fitness <- c(log.stats$fitness,
    list(domHV = function(x) ecr::computeHV(x,
      ref.point = ref.point), 
      delta = function(x) ecr:::emoaIndDelta(x[c(1,2),]), 
      spacing = function(x) ecr:::emoaIndSP(x, "euclidean")))
  
  
  log = ecr::initLogger(control, log.stats = log.stats, log.pop = log.pop,
    log.extras = c(population.div = "numeric"), init.size = 1000)
  
  population = initial.solutions
  population = lapply(population, function(x) {
    x = transform.to.orig(x, x.interest, delete.use.orig = FALSE, 
      fixed.features = fixed.features)
  })
  # TEST:
  # if (!all(lapply(population, function(x){class(x)}) == class(x.interest))) {
  #   stop("population and x_orig different feature types")
  # }

  fitness = evaluateFitness(control, population, target, x.interest,
    range.features, param.set = param.set, predictor)
  for (i in seq_along(population)) {
    attr(population[[i]], "fitness") = fitness[, i]
  }

  repeat {
    offspring = ecr:::generateOffspring(control, population, fitness, 
      lambda = lambda, p.recomb = p.recomb, p.mut = p.mut)
    offspring = lapply(offspring, function(x) {
      x = transform.to.orig(x, x.interest, delete.use.orig = FALSE, 
        fixed.features = fixed.features)
    })
    fitness.offspring = evaluateFitness(control, offspring, target,
      x.interest, range.features, param.set, predictor)
    for (i in seq_along(offspring)) {
      attr(offspring[[i]], "fitness") = fitness.offspring[, i]
    }
    
    sel = selectDiverse(control, population, offspring, 
      fitness, fitness.offspring)

    fitness = sel$fitness
    ##ranks = doNondominatedSorting(fitness)$ranks
    ##browser(expr = {2 %in% ranks & iteration > 30})
    
    population = sel$population
    dis = StatMatch::gower.dist(data.x = list.to.df(population), 
      rngs = range.features)
    single.dist = dis[lower.tri(dis)]
    mean.dist = mean(single.dist)
    pop.div = sum(abs(single.dist - mean.dist)/length(population))
    
    ecr::updateLogger(log, population, fitness, n.evals = length(population), 
      extras = list(population.div = pop.div))
    stop.object = ecr:::doTerminate(log, terminators)
    
    if (length(stop.object) > 0L) { 
      print(stop.object$message)
      break
    }
  }
  return(ecr:::makeECRResult(control, log, population, fitness, stop.object))
}



evaluateFitness = function (control, inds, target, x.interest, range,
  param.set, predictor){
  assertList(inds)
  assertClass(control, "ecr_control")

  inds.trans = lapply(inds, function(x) {
    x$use.orig  = NULL
    as.data.frame(x, stringsAsFactors = FALSE)
  })
  inds.char = do.call(rbind, inds.trans)
  
  if ("discrete" %in% ParamHelpers::getParamTypes(param.set)) {
    inds.factor = lapply(inds.trans, function(x) mosmafs::valuesFromNames(param.set, x))
    inds.factor = do.call(rbind, inds.factor)
  }
  else {
    inds.factor = inds.char
  }
  
  # Objective Functions
  pred = predictor$predict(newdata = inds.factor)[[1]]
  q1 = ifelse(length(target) == 2 & (pred > target[1]) & (pred < target[2]), 
    0, abs(pred - target))
  q2 = StatMatch::gower.dist(data.x = x.interest, data.y = inds.char, rngs = range)[1,]
  q3 = rowSums(inds.char != x.interest[rep(row.names(x.interest), nrow(inds.char)),])
  
  fitness = mapply(function(a,b,c) {
    c(a,b,c)
  }, q1, q2, q3)
  
  fitness = ecr:::makeFitnessMatrix(fitness, control)
  return(fitness)
}



selectDiverse = function (control, population, offspring, fitness, 
  fitness.offspring) {
  assertClass(control, "ecr_control")
  assertClass(control$selectForSurvival, "ecr_selector")
  assertList(population)
  assertList(offspring)
  n.select = length(population)
  assertMatrix(fitness, ncols = length(population), any.missing = FALSE, 
    all.missing = FALSE, min.rows = 1L)
  assertMatrix(fitness.offspring, ncols = length(offspring), any.missing = FALSE, 
    all.missing = FALSE, min.rows = 1L)
  merged.pop = c(population, offspring)
  merged.fit = cbind(fitness, fitness.offspring)
  
  fitness = ecr:::transformFitness(merged.fit, control$task, control$selectForSurvival)
  surv.idx = control$selectForSurvival(fitness, n.select = n.select, merged.pop) 
  fitness = merged.fit[, surv.idx, drop = FALSE]
  fitness = ecr:::makeFitnessMatrix(fitness, control)
  return(list(population = merged.pop[surv.idx], fitness = fitness))
}
environment(replaceMuPlusLambda) = asNamespace("mosmafs")

stopOnStag = function (stag) {
  assertInt(stag)
  stag.access = "gen"
  stag.name = "generations"
  condition.fun = function(log) {
    current.gen = log$env$n.gens
    if (current.gen < stag) {
      return(FALSE)
    }
    obj.colnames = grep("(fitness.obj.[123].min)|(fitness.domHV)", 
      colnames(log$env$stats), value = TRUE)
    stags = apply(log$env$stats[obj.colnames], 2, function(col) {
      length(unique(col[(current.gen - stag) : current.gen])) == 1
    })
    return(all(stags))
  }
  ecr::makeTerminator(condition.fun, name = "StagLimit", 
    message = sprintf("Minimum objective values and HV did not change for %s iterations", 
      stag))
}

#assignInNamespace("replaceMuPlusLambda", replaceMuPlusLambda, environment = "ecr")


