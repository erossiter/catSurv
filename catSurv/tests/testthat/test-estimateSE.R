library(catSurv)
library(testthat)
context("Prior")

test_that("estimateSE calculates correctly", {
  
  estimateSE_test <- function(cat){
    #answered_questions <- cat@applicable_rows
    #theta_hat <- estimateTheta(cat)
    #x <- cat@X
    #prior_values <- prior(cat)
  
    #fx <- fx_theta <- rep(NA, length(x))
    #for(i in 1:length(x)){
    #  fx[i] <- likelihood(cat, x[i], answered_questions)*prior_values[i]
    #  fx_theta[i] <- ((x[i] - theta_hat)^2) * fx[i]
    #}
    numerator <- function(cat){
      answered_questions <- cat@applicable_rows
      theta_hat <- estimateTheta(cat)
      x <- cat@X
      prior_values <- prior(cat)
      return((theta - theta_hat)^2 * prior_values * likelihood(cat, x, answered_questions))
    }
    denominator <- function(cat){
      answered_questions <- cat@applicable_rows
      theta_hat <- estimateTheta(cat)
      x <- cat@X
      prior_values <- prior(cat)
      return(prior_values * likelihood(cat, x, answered_questions))
    }
  
    results <- sqrt(integrate(numerator)/integrate(denominator))
    return(results)
  }
  
  expect_equal()
})

