library(catSurv)
library(testthat)
context("Prior")

test_that("estimateTheta calculates correctly", {
  
  test_cat1 <- new("Cat")
  test_cat1@discrimination <- c(2,4,6,8)
  test_cat1@difficulty <- c(1,2,3,4)
  test_cat1@priorName <- "NORMAL"
  test_cat1@priorParams <- c(0,1.5)
  test_cat1@poly <- FALSE
  
  test_cat2 <- new("Cat")
  test_cat2@discrimination <- c(1,10,12,13)
  test_cat2@difficulty <- c(1,2,3,4)
  test_cat2@priorName <- "NORMAL"
  test_cat2@priorParams <- c(0,10)
  test_cat2@poly <- FALSE
  
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
      results <- (integrate(Vectorize(numerator), -Inf, Inf)$value)/
        (integrate(Vectorize(denominator), -Inf, Inf)$value)
      }
    
    # Need dLL() and d2LL() to do further work on "MAP"
    # if(cat@estimation == "MAP"){
    #   theta_hat_old <- 0
    #   theta_hat_new <- 1
    #   tolerance <- .001
    #   difference <- abs(theta_hat_new - theta_hat_old)
    #   while(difference > tolerance){
    #   ## still need to write dLL_test() and d2LL_test() functions
    #     theta_hat_new <- theta_hat_old - dLL(cat, theta_hat_old, TRUE)/d2LL(cat, theta_hat_old, TRUE)
    #     difference <- abs(theta_hat_new - theta_hat_old)
    #     theta_hat_new <- theta_hat_old
    #     }
    #   results <- theta_hat_new
    #   }
    return(results)
    }
  
  expect_equal(estimateTheta(test_cat1), estimateTheta_test(test_cat1))
  expect_equal(estimateTheta(test_cat2), estimateTheta_test(test_cat2))
})

## 'stats' is the package integrate() is in.
## Is likelihood giving back correct answers??
## range from -inf to inf for integrating???
