 custom.mutGauss = ecr::makeMutator(
  mutator = function (ind, p = 1L, sdev = 0.05, lower, upper) 
  {
    checkmate::assert_number(p, lower = 0, finite = TRUE, na.ok = FALSE)
    checkmate::assert_numeric(lower, any.missing = FALSE, all.missing = FALSE)
    checkmate::assert_numeric(lower, any.missing = FALSE, all.missing = FALSE)
    checkmate::assert_numeric(sdev, len = length(ind))
    if (length(lower) != length(upper)) {
      stopf("Gauss mutator: length of lower and upper bounds need to be equal!")
    }
    n = length(ind)
    mut.idx = runif(n) < p
    
    if (length(sdev)>0) {
      mut.noise = c()
      for (sd in sdev[mut.idx]) {
        mut = rnorm(1, mean = 0, sd = sd)
        mut.noise = c(mut.noise, mut)
      }
    }
    
    else {
      mut.noise = rnorm(sum(mut.idx), mean = 0, sd = sdev)
    }
    
    ind[mut.idx] = ind[mut.idx] + mut.noise
    ind = pmin(pmax(lower, ind), upper)
    return(ind)
  },
  supported = "float")


custom.mutGaussInt = mosmafs::intifyMutator(custom.mutGauss)

