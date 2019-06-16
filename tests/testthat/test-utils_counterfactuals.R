context("utils_counterfactuals") 

discval = c("a", "b", "c")
test.df = data.frame(num = runif(10, 0, 1), 
  char = sample(discval, size = 10, replace = TRUE), 
  fact = factor(sample(discval, size = 10, replace = TRUE), levels = c(discval)), 
  int = sample(1:20, size = 10), stringsAsFactors = FALSE)
ps = ParamHelpers::makeParamSet(params = make_paramlist(test.df))

test_that("make_paramlist", {
  expect_true(all(c("num", "char", "fact", "int", "use.orig") %in% getParamIds(ps)))
  invisible(lapply(getValues(ps)[[1]], function(x) expect_class(x, "character")))
  invisible(lapply(getValues(ps)[[2]], function(x) expect_class(x, "factor")))
  expect_true(all(getParamTypes(ps) == c("numeric", "discrete", "discrete", 
    "integer", "logicalvector")))
  
  # set lower
  lower = c(-1, 0)
  names(lower) = c("num", "int")
  ps.low = ParamHelpers::makeParamSet(params = make_paramlist(test.df, 
    lower = lower))
  assert_true(all(getLower(ps.low) == lower))
  
  upper = c(1, 11)
  names(upper) = c("num", "int")
  ps.upper = ParamHelpers::makeParamSet(params = make_paramlist(test.df, 
    upper = upper))
  assert_true(all(getUpper(ps.upper) == upper))
  
  # lower = upper --> error
  lower = 1
  names(lower) = "num"
  expect_error(make_paramlist(test.df, lower = lower), 
    "some component of 'upper' is smaller than the corresponding one in 'lower'")
  
  # wrong names in upper and lower
  upper = c(1, 11)
  names(upper) = c("num3", "int")
  expect_error(make_paramlist(test.df, upper = upper))
  expect_error(make_paramlist(test.df, lower = upper))
  
  # one parameter
  

})


test_that("sdev_to_list", {
  sdev = c(0.1, 0.3, 0.5, 0.7)
  names(sdev) = names(test.df)
  sdevlist = sdev_to_list(sdev, ps)
  expect_list(sdevlist)
  
  # different ordering in sdev
  sdev = c(0.7, 0.1)
  names(sdev) = c("int", "num")
  sdevlist = sdev_to_list(sdev, ps)
  expect_list(sdevlist)
  expect_true(sdevlist$integer == 0.7)
  expect_true(sdevlist$numeric == 0.1)
  
  names(sdev) = c("woeh", "num")
  expect_error(sdev_to_list(sdev, ps))
})

test_that("get_diff", {
  ps.simple = pSS(one: numeric [1, 2], 
    two: discrete [discval], 
    three: discrete [char_to_factor(discval)], 
    four: integer [1, 5])
  
  test.df = ParamHelpers::generateRandomDesign(n = 6, par.set = ps.simple)
  test.df$two = as.character(test.df$two)
  x.interest = test.df[1, ]
  diff = get_diff(x = test.df, x.interest = x.interest)
  
  expect_true(all(diff[1, ] == 0))
  expect_class(diff$two, "character")
  expect_class(diff$three, "character")
  expect_class(diff$four, "integer")
  expect_identical(test.df$one[2]-test.df$one[1], diff$one[2])
  equal.id = which(diff$two != "0")
  expect_equal(test.df$two[equal.id], diff$two[equal.id])
  equal.id = which(diff$three != "0")
  expect_equal(as.character(test.df$three[equal.id]), diff$three[equal.id])
})


test_that("round_df", {
  decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
  rdf = round_df(test.df, digits = 5)
  expect_true(all(nchar(rdf$num) <= 5 + 2)) # 2 for 0 and ,
  expect_equal(rdf[,-1], test.df[,-1])
  expect_class(rdf$fact, "factor")
  expect_class(rdf$char, "character")
})

test_that("transform_to_orig", {
  x.interest = test.df[1, ]
  x = list(num = x.interest$num + 0.05, char = "b", 
    fact = factor("c", levels = discval), int = 2L, 
    use.orig = c(TRUE, TRUE, FALSE, FALSE))
  x.trans = transform_to_orig(x = x, x.interest = x.interest)
  expect_identical(x.trans$num, x.interest$num)
  expect_identical(x.trans$char, x.interest$char)
  expect_true(x.trans$fact != x.interest$fact)
  expect_true(x.trans$int != x.interest$int)
  
  x$use.orig = c(FALSE, FALSE, TRUE, TRUE)
  x.trans = transform_to_orig(x = x, x.interest = x.interest, delete.use.orig = TRUE)
  expect_identical(x.trans$fact, x.interest$fact)
  expect_identical(x.trans$int, x.interest$int)
  expect_true(x.trans$num != x.interest$num)
  expect_true(x.trans$char != x.interest$char)
  expect_null(x.trans$use.orig)
  
  x$use.orig = rep(FALSE, 4)
  expect_true(sum(transform_to_orig(x = x, 
      x.interest = x.interest, max.changed = 1)$use.orig) == 3)
  x.trans = transform_to_orig(x = x, x.interest = x.interest, 
    fixed.features = "num")
  expect_true(x.trans$use.orig[1])
  expect_identical(x.trans$num, x.interest$num)
  x.trans = transform_to_orig(x = x, x.interest = x.interest, 
    fixed.features = c("num", "fact"))
  expect_true(all(x.trans$use.orig[c(1, 3)]))
  expect_identical(x.trans$fact, x.interest$fact)
  expect_identical(x.trans$num, x.interest$num)  
  
  x = list(num = x.interest$num + 0.05, char = factor("b", levels = discval), 
    fact = "c", int = 2L, 
    use.orig = c(TRUE, TRUE, FALSE, FALSE))
  expect_error(transform_to_orig(x = x, x.interest = x.interest), 
    "type shift")
  
  
  
  
})

test_that("get_ICE_var", {
  data("Boston", package = "MASS")
  test.df = Boston[, c("medv", "rm")]
  n = nrow(test.df)
  set.seed(1234)
  test.df$random1 = runif(n, 0, 1) 
  test.df$random2 = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  test.df$random3 = rnorm(n, 10, 1)
  lm =  lm(medv ~ ., data = test.df)
  test.df = test.df[,-1]
  x.interest = test.df[1,]
  mod = Predictor$new(lm, data = test.df)
  param.set = ParamHelpers::makeParamSet(params = make_paramlist(test.df))
  
  sdev = get_ICE_var(x.interest, mod, param.set)
  expect_true(which.max(sdev) == 1)
  expect_true(all(sdev[-1] <= 1))
  expect_true(sdev[1] > 14)
})


test_that("get diverse solutions", {
  fitness = data.frame(
    o1 = c(1.5, 2.5, 3, 5.5, 4.5, 6),
    o2 = c(8.5, 7, 6, 3.5, 5, 2),
    o3 = c(1, 1, 1, 2, 1, 2))
  df = data.frame(one = c(1.5), two = c("a"), three = c(1L))
  df = df[rep(seq_len(nrow(df)), each=6),]
  rownames(df) = NULL
  
  set.seed(1234)
  test = vapply(1:10, FUN.VALUE = numeric(3), 
    function(x) get_diverse_solutions(fitness = fitness, 
      pareto.set = df, nr.solutions = 3))
  expect_true(all(test %in% c(1, 4, 5, 6)))
  expect_true(length(unique(test[1,])) > 1)
  expect_error(get_diverse_solutions(fitness = matrix(fitness), 
    pareto.set = df, nr.solutions = 3), 
    "'fitness' failed: Must be of type 'data.frame', not 'list'")
  expect_error(get_diverse_solutions(fitness = fitness, 
    pareto.set = df[-1,], nr.solutions = 3), 
    "'fitness' failed: Must have exactly 5 rows, but has 6 rows")
  
  fitness = data.frame(
    o1 = rep(1, 5), 
    o2 = rep(2, 5), 
    o3 = rep(1, 5))
  
  df = data.frame(
    a = rep(1, 5), 
    b = c(1, 1, 5, 1, 1), 
    c = rep(1, 5))
  
  expect_true(all(get_diverse_solutions(fitness, df, nr.solution = 3) %in%
    c(1, 5, 3)))
  
})


