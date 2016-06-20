library(catSurv)
library(catR)
context("obsInf")

test_that("obsInf calculates correctly", {
  Ii(1, tcals[,1:4])$Ii[1]
  testCats[[1]]@discrimination <- tcals[,1]
  testCats[[1]]@difficulty <- tcals[,2]
  testCats[[1]]@guessing <- tcals[,3]
  testCats[[1]]@answers <- c(1, rep(NA, 84))
  obsInf(testCats[[1]], 1, 3)
  
  OIi(1, tcals[,1:4], x=0)$OIi[1]
  
})
