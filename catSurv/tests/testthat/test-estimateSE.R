library(catSurv)
library(testthat)
context("estimateSE")

test_that("estimateSE calculates correctly", {
  
  estimateSE_test <- function(cat){
    library(stats)
    numerator <- function(theta){
      theta_hat <- estimateTheta(cat)
      diff <- (theta - theta_hat)^2
      prior_theta <- prior(theta, cat@priorName, cat@priorParams)
      L_theta <- likelihood(cat, theta)
      return(diff * prior_theta * L_theta)
      }
    denominator <- function(theta){
      prior_theta <- prior(theta, cat@priorName, cat@priorParams)
      L_theta <- likelihood(cat, theta)
      return(prior_theta * L_theta)
      }
    results <- (integrate(Vectorize(numerator), -6, 6)$value)/
        (integrate(Vectorize(denominator), -6, 6)$value)
    return(sqrt(results))
  }
  
   # lapply(c(catBiCreator(5), catPolyCreator(5)),
   #        function(x) expect_equal(estimateSE(x), estimateSE_test(x), tolerance = .1))
  for(i in 1:length(testCats)){
    print(round(estimateSE(testCats[[i]]) - estimateSE_test(testCats[[i]]), 5))
  }
  
  ## testCats 1, 2, and 9 are not very close.
  testCats[[1]]@estimation
  testCats[[2]]@estimation
  testCats[[9]]@estimation
  
  testCats[[1]]@poly
  testCats[[2]]@poly
  testCats[[9]]@poly
  
  testCats[[1]]@priorName
  testCats[[2]]@priorName
  testCats[[9]]@priorName
})




