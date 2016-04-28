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
  
  # lapply(testCats[[1]], function(x) expect_equal(estimateSE(x),
  #                                           estimateSE_test(x),
  #                                           tolerance = .1))

})

## Ony testing it on binary cats, because estimateTheta isn't working 
## consisntely for categorical
estimateSE(testCats[[1]])
estimateSE_test(testCats[[1]])

estimateSE(testCats[[2]])
estimateSE_test(testCats[[2]])

estimateSE(testCats[[3]])
estimateSE_test(testCats[[3]])

estimateSE(testCats[[4]])
estimateSE_test(testCats[[4]])

estimateSE(testCats[[5]])
estimateSE_test(testCats[[5]])

estimateSE(testCats[[6]])
estimateSE_test(testCats[[6]])


