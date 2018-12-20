
#' @importFrom stats formula terms
find_y = function(mod) {
  if(is.null(mod)) return(NULL)
  UseMethod("find_y")
}

find_y.WrappedModel = function(mod) {
  mod$task.desc$target
}

find_y.NULL = function(mod) {
  NULL
}

find_y.default = function(mod) {
  fml =  tryCatch(Formula::as.Formula(formula(mod)), error = function(e) NULL)
  if(is.null(fml)) return(NULL)
  target = terms(fml, lhs = 1, rhs = 0)
  ix = attr(target, "variables")[-1L]
  y = sapply(ix, deparse, width.cutoff = 500L)
  y
}
