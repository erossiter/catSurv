library(catSurv)
library(testthat)
context("Prior")

test_that("trapezoidal integration calculates correctly", {
  
  trapIntegration_test <- function(x, fx){
    add_these <- rep(0, length(x))
    for(i in 1:(length(x)-1)){
      add_these[i] <- (x[i+1] - x[i]) * ((fx[i] + fx[i+1])/2)
      }
    return(sum(add_these))
    }
  
  expect_equal(xxxxxxx, trapIntegration_test(cat@X, cat@X^2))
})

## Where is the cpp code for this, if there is any??
## We might end up just deleting this, but I wrote it for now...


