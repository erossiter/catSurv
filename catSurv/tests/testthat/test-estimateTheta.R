library(catSurv)
library(testthat)
context("Prior")

test_that("estimateTheta calculates correctly", {
  
  cat_dnorm <- new("Cat")
  cat_dnorm@discrimination <- c(2,4,6,8)
  cat_dnorm@difficulty <- c(1,2,3,4)
  cat_dnorm@priorName <- "NORMAL"
  cat_dnorm@priorParams <- c(0,1.5)
  cat_dnorm@poly <- FALSE
  
  estimateTheta_test <- function(cat, theta){
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
      results <- integrate(Vectorize(numerator), lower = -5, upper = 5) #/integrate(Vectorize(denominator), -5, 5)
      }
  # if(cat@estimation == "MAP"){
  #   theta_hat_old <- 0
  #   theta_hat_new <- 1
  #   tolerance <- .0000001
  #   difference <- abs(theta_hat_new - theta_hat_old)
  #   while(difference > tolerance){
  #     ## still need to write dLL_test() and d2LL_test() functions
  #     theta_hat_new <- theta_hat_old - dLL(cat, theta_hat_old, TRUE)/d2LL(cat, theta_hat_old, TRUE)
  #     difference <- abs(theta_hat_new - theta_hat_old)
  #     theta_hat_new <- theta_hat_old
  #   }
  #   results <- theta_hat_new
  # }
  return(results)
}
  
  expect_equal(estimateTheta(cat_dnorm), estimateTheta_test(cat_dnorm, 1) )
})

## 'stats' is the package integrate() is in.
