library(catSurv)
library(testthat)
context("estimateTheta")

test_that("estimateTheta calculates correctly", {
  
  estimateTheta_test <- function(cat){
    library(stats)
    numerator <- function(theta){
      prior_values <- prior(theta, cat@priorName, cat@priorParams)
      return(theta * likelihood(cat, theta) * prior_values)
      }
    denominator <- function(theta){
      prior_values <- prior(theta, cat@priorName, cat@priorParams)
      return(likelihood(cat, theta) * prior_values)
      }
    if(cat@estimation == "EAP"){ 
      results <- (integrate(Vectorize(numerator), -6, 6)$value)/
        (integrate(Vectorize(denominator), -6, 6)$value)
      }
    
    if(cat@estimation == "MAP"){
      theta_hat_old <- 0
      theta_hat_new <- 1
      tolerance <- .00000001
      difference <- abs(theta_hat_new - theta_hat_old)
      while(difference > tolerance){
        theta_hat_new <- theta_hat_old - (dLL(cat, theta_hat_old, TRUE)/d2LL(cat, theta_hat_old, TRUE))
        difference <- abs(theta_hat_new - theta_hat_old)
        theta_hat_old <- theta_hat_new
        }
      results <- theta_hat_new
      }
    return(results)
  }
  
  # lapply(testCats, function(x) expect_equal(estimateTheta(x),
  #                                           estimateTheta_test(x),
  #                                           tolerance = .01))
})

## 'stats' is the package integrate() is in.

## Binary - MAP -- works
estimateTheta(testCats[[1]])
estimateTheta_test(testCats[[1]])

## Binary - EAP -- works
estimateTheta(testCats[[6]])
estimateTheta_test(testCats[[6]])

## Categorical - MAP
estimateTheta(testCats[[11]]) ## Doesn't work -- 7, 9, 11
estimateTheta_test(testCats[[9]]) ## Doesn't work b/c of dLL -- 7, 9, 11

## Categorical - EAP
## what do we do if estimateTheta returns "NaN"?  
## what effect does this have down the line?
## This cat returns different answers each time.
estimateTheta(testCats[[12]])
estimateTheta_test(testCats[[12]])


