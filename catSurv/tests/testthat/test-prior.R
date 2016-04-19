library(catSurv)
library(testthat)
context("Prior")

test_that("prior calculates correctly", {
  
  cat_dnorm <- new("Cat")
  cat_dnorm@discrimination <- c(2,4,6,8)
  cat_dnorm@difficulty <- c(1,2,3,4)
  cat_dnorm@priorName <- "NORMAL"
  cat_dnorm@priorParams <- c(0,1.5)
  
  cat_studT <- new("Cat")
  cat_studT@discrimination <- c(2,4,6,8)
  cat_studT@difficulty <- c(1,2,3,4)
  cat_studT@priorName <- "STUDENT_T"
  cat_studT@priorParams <- c(2,3,4)
  
  ## R test function
  prior_test <- function(x, cat){
    distribution <- cat@priorName
    parameters <- cat@priorParams
    
    if(distribution == "NORMAL"){
      prior_values <- dnorm(x, parameters[1], parameters[2])
    }
    if(distribution == "STUDENT_T"){
      #prior_values <- (1/parameters[2]) * dt( (x - parameters[1]) / parameters[2], parameters[3])
      prior_values <- dt(x, df = parameters[2], ncp = parameters[1])
    }
    return(prior_values)
  }
  
  expect_equal(prior(1, cat_dnorm@priorName, cat_dnorm@priorParams), prior_test(1, cat_dnorm))
  expect_equal(prior(1, cat_studT@priorName, cat_studT@priorParams), prior_test(1, cat_studT))
})


