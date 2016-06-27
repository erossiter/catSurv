library(catSurv)
library(testthat)
library(ltm)
library(stats)
context("estimateSE")

test_that("estimateSE calculates correctly", {
  
  estimateSE_test <- function(cat){
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
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  binary_cat <- ltmCat(binary_data)
  poly_cat <- grmCat(poly_data)
  
  differences <- numeric(nrow(binary_data))
  for(i in 1:nrow(binary_data)){
    binary_cat@answers <- as.numeric(binary_data[i, ])
    differences[i] <- estimateSE(binary_cat) - estimateSE_test(binary_cat)
  }
  
  differences <- numeric(nrow(poly_data))
  for(i in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[i, ])
    differences[i] <- estimateSE(poly_cat) - estimateSE_test(poly_cat)
  }
  
})




