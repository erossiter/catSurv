library(catSurv)
library(testthat)
context("Prior")

test_that("estimateSE calculates correctly", {
  
  test_cat1 <- new("Cat")
  test_cat1@discrimination <- c(2,4,6,8)
  test_cat1@difficulty <- c(1,2,3,4)
  test_cat1@priorName <- "NORMAL"
  test_cat1@priorParams <- c(0,1.5)
  test_cat1@poly <- FALSE
  
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

