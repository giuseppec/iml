# Test
# Equality of $effects with individual ALE and so on
# different methods, ALE, PDP, ...
# subselection of features works
# - for numerical
# - for character
# plot works

test_that("FeatureEffect (pdp only) works for single output and single feature", {
  grid.size <- 10
  pdp.objs <- FeatureEffects$new(predictor1,
    method = "pdp",
    grid.size = grid.size
  )
  suppressWarnings(expect_s3_class(pdp.objs$plot(), "patchwork"))

  pdp.obj.a <- FeatureEffect$new(predictor1,
    feature = c("a"),
    method = "pdp", grid.size = grid.size
  )
  pdp.obj.b <- FeatureEffect$new(predictor1,
    feature = c("b"),
    method = "pdp", grid.size = grid.size
  )
  pdp.obj.c <- FeatureEffect$new(predictor1,
    feature = c("c"),
    method = "pdp", grid.size = grid.size
  )
  pdp.obj.d <- FeatureEffect$new(predictor1,
    feature = c("d"),
    method = "pdp", grid.size = grid.size
  )

  expect_equal(pdp.obj.a$results, pdp.objs$effects$a$results)
  expect_equal(pdp.obj.b$results, pdp.objs$effects$b$results)
  expect_equal(pdp.obj.c$results, pdp.objs$effects$c$results)
  expect_equal(pdp.obj.d$results, pdp.objs$effects$d$results)

  pdp.objs <- FeatureEffects$new(predictor1,
    features = c("a", "c"),
    method = "pdp", grid.size = grid.size
  )
  suppressWarnings(expect_s3_class(pdp.objs$plot(), "patchwork"))

  expect_equal(pdp.obj.a$results, pdp.objs$effects$a$results)
  expect_equal(pdp.obj.c$results, pdp.objs$effects$c$results)
  expect_equal(names(pdp.objs$effects), c("a", "c"))


  pdp.objs <- FeatureEffects$new(predictor1, method = "ale", grid.size = 10)
  suppressWarnings(expect_s3_class(pdp.objs$plot(), "patchwork"))
  pdp.obj.a <- FeatureEffect$new(predictor1,
    feature = c("a"), method = "ale",
    grid.size = 10
  )
  expect_equal(pdp.obj.a$results, pdp.objs$effects$a$results)

  pdp.objs <- FeatureEffects$new(predictor1, method = "pdp+ice", grid.size = 2)
  expect_s3_class(pdp.objs$plot(), "patchwork")
  pdp.obj.a <- FeatureEffect$new(predictor1,
    feature = c("a"),
    method = "pdp+ice", grid.size = 2
  )
  expect_equal(pdp.obj.a$results, pdp.objs$effects$a$results)
})
