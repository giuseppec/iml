context("utils_counterfactuals") 

discval = c("a", "b", "c")
test.df = data.frame(num = runif(10, 0, 1), 
  char = sample(discval, size = 10, replace = TRUE), 
  fact = factor(sample(discval, size = 10, replace = TRUE), levels = c(discval)), 
  int = sample(1:20, size = 10), stringsAsFactors = FALSE)


test_that("make_paramlist", {
  ps = ParamHelpers::makeParamSet(params = make_paramlist(test.df))
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

