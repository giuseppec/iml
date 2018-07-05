

# 
devtools::load_all()
library(testthat)
library(checkmate)
library(data.table)
data("airquality")

airq = data.table(airquality)

set.seed(42)
mg = MarginalGenerator$new(grid.dat = airq, dist.dat = airq, features = c("Ozone", "Wind"))



## Check that n matches for different n and different datasets
mg$next.batch(154)

mg$all()



## Test different grid.dat 
mg = MarginalGenerator$new(grid.dat = airq[1,], dist.dat = airq, features = c("Ozone", "Wind"))
mg$all()
