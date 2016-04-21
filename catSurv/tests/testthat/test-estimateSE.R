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
  expect_equal(estimateSE(test_cat1), estimateSE_test(test_cat1))
})

