library(catSurv)
library(testthat)
context("Prior")

test_that("prior calculates correctly", {
  
  prior_test <- function(x, cat){
    distribution <- cat@priorName
    parameters <- cat@priorParams
    
    if(distribution == "NORMAL"){
      prior_values <- dnorm(x, parameters[1], parameters[2])
      }
    if(distribution == "STUDENT_T"){
      prior_values <- dt(x, df = parameters[2], ncp = parameters[1])
    }
    return(prior_values)
  }
  

  equal_test <- function(cat){
    x <- sample(1:10, 1)
    expect_equal(prior(x, cat@priorName, cat@priorParams), prior_test(x, cat),
                 tolerance = .00001)
    }
  lapply(testCats, equal_test)
})


