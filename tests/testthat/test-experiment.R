context('Test experiment super class')


test_that('Experiments work',{
  
  
  f = function(x){
    unlist(x[1] + x[2])
  }
  X = data.frame(a = c(1,2,3), b = c(2,3,4))
  e = Experiment$new(f=f, X=X)
  e$run()
  
  e2 = e$clone()$run()
  
  # Rerunning experiment does not change the experiment
  expect_equal(e, e2)
  expect_false(isTRUE(all.equal(e, e$clone()$run(force=TRUE))))
}
)