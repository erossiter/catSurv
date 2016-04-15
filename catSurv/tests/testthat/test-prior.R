library(catSurv)
library(testthat)
context("Prior")

test_that("prior calculates correctly", {
  
  test_cat <- new("Cat", discrimination = c(2,4), difficulty = c(1,2),
                  priorName = "NORMAL", priorParams = c(0,1),
                  guessing = c(.5, .5), answers = c(1,2), poly = c(TRUE, TRUE))

  
  ## R test function
  prior_test <- function(x, cat){
    distribution <- cat@priorName
    parameters <- cat@priorParams

    if(distribution == "NORMAL"){
      prior_values <- dnorm(x, parameters[1], parameters[2])
    }
    if(distribution == "STUDENT_T"){
      prior_values <- (1/parameters[2]) * dt( (x - parameters[1]) / parameters[2], parameters[3])
    }
    return(prior_values)
  }
  
  expect_equal(prior(5, test_cat@priorName, test_cat@priorParams), prior_test(5, test_cat))
})

## I think the cpp code is wrong.

## We need to:
# add documentation specifying it takes a scalar and returns a scalar
# add code in that will throw an error if it is fed a vector or list
# add a test to make sure it throws an error when fed something other than scalar
# make sure we are consistent about character strings for "normal" and "studentT"

## UNLESS, prior() is used to calculate "prior_values" which is a slot in the
## S4 cat object according to the documentation, but there isn't a slot
## in the R code.  
## If prior() is used to calculate "prior_values", then we need to allow for
## it to take and return a vector

## I see that nextItemEPV fills in the "prior_values" slot...

## --------------------------------------------------------------------------
## An overall question/concern:  for cpp and R functions, why would we have
## as an arguement something that is a slot in the Cat object?  Wouldn't
## we then only feed it a Cat object and grab what we needed from there?
## This is mainly a concern in the cpp code with prior() and moving foward

