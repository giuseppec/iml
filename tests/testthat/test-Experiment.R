context("Experiment")


test_that("Experiments work",{
  
  
  f = function(x) {
    unlist(x[1] + x[2])
  }
  X = data.frame(a = c(1,2,3), b = c(2,3,4))
  ds = Data$new(X)
  pred = makePredictor(f)
  e = Experiment$new(pred, ds)
  set.seed(1)
  dat = e$run()$data()
  
  set.seed(2)
  dat3 = e$run(force=TRUE)$data()
  
})
  
  