library(catSurv)
library(testthat)
context("Prior")

test_that("estimateTheta calculates correctly", {
  
    test_cat1 <- test_cat2 <- new("Cat")
    test_cat1@estimation <- "EAP"
    test_cat1@priorName <- "NORMAL"
    test_cat1@priorName <- "STUDENT_T"
    test_cat1@priorParams <- c(0,1)
    
    
    test_cat2@estimation <- "MAP"
    test_cat2@priorName <- "NORMAL"
    test_cat2@priorName <- "STUDENT_T"
    test_cat2@priorParams <- c(0,1)
    
  
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
      tolerance <- .0000001
      difference <- abs(theta_hat_new - theta_hat_old)
      while(difference > tolerance){
        theta_hat_new <- theta_hat_old - (dLL(cat, theta_hat_old, TRUE)/d2LL(cat, theta_hat_old, TRUE))
        difference <- abs(theta_hat_new - theta_hat_old)
        theta_hat_new <- theta_hat_old
        }
      results <- theta_hat_new
      }
    return(results)
    }
  
  expect_equal(estimateTheta(test_cats[[1]]), estimateTheta_test(test_cats[[1]]))
  expect_equal(estimateTheta(test_cats[[2]]), estimateTheta_test(test_cats[[2]]))
})

## 'stats' is the package integrate() is in.
## Is likelihood giving back correct answers??
