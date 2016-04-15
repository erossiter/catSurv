library(catSurv)
library(testthat)
context("Prior")

test_that("estimateTheta calculates correctly", {
  
  test_cat <- new("Cat")
  cat@answers <- c(1,2,3,4,5)
  
  
  estimateTheta_test <- function(cat){
    answered_questions <- which(!is.na(cat@answers))
    prior_values <- prior_test(cat)
    x <- cat@X
    if(cat@estimation == "EAP"){
      fx <- fx_x <- rep(NA, length(x))
      for(i in 1:length(x)){
        fx[i] <- likelihood(cat, x[i], answered_questions)*prior_values[i]
        fx_x[i] <- fx[i] * x[i] 
        }
      results <- trapIntegration_test(x, fx_x) / trapIntegration_test(x, fx)
    }
      
      ##come back and change this, obviousyl
    if(cat@estimation == "XXXXXXXXXX"){ 
      numerator <- function(cat){
        answered_questions <- which(!is.na(cat@answers))
        prior_theta <- prior(cat)
        x <- cat@X
        L_theta <- likelihood(cat, x, answered_questions) 
        return(theta * L_theta * prior_theta)
      }
      denominator <- function(cat){
        answered_questions <- which(!is.na(cat@answers))
        prior_theta <- prior(cat)
        x <- cat@X
        L_theta <- likelihood(cat, x, answered_questions) 
        return(L_theta * prior_theta)
      }
      results <- integrate(numerator(cat), -5, 5)/integrate(denominator(cat), -5, 5)
    }
  
  if(cat@estimation == "MAP"){
    theta_hat_old <- 0
    theta_hat_new <- 1
    tolerance <- .0000001
    difference <- abs(theta_hat_new - theta_hat_old)
    while(difference > tolerance){
      ## still need to write dLL_test() and d2LL_test() functions
      theta_hat_new <- theta_hat_old - dLL(cat, theta_hat_old, TRUE)/d2LL(cat, theta_hat_old, TRUE)
      difference <- abs(theta_hat_new - theta_hat_old)
      theta_hat_new <- theta_hat_old
    }
    results <- theta_hat_new
  }
    

  return(results)
}
  
  expect_equal(estimateTheta(xxxxxxxxx), estimateTheta_test(test_cat) )
})

## I think the cpp code is wrong.

## We need to:
# add documentation specifying it takes a scalar and returns a scalar
# add code in that will throw an error if it is fed a vector or list
# add a test to make sure it throws an error when fed something other than scalar
# make sure we are consistent about character strings for "normal" and "studentT"

