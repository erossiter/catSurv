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
# 
# my_dt <- function(x, mu, df){
#   Z <- dnorm(x, 0, 1)
#   V <- 6
#   results <- (Z+mu) / sqrt(V/df)
#   results <- 1 - pt(results, df=df)
#   return(results)
# }
# 
# my_dt(1, mu=2, df=3)
# 
# dt(1, df=3, ncp=2)

## I figured out how the cpp code and my R function differ for normal distribution.
## When I set the 'log' arguement equal to TRUE they get the same answer... 
## do we want the log of probabilities or not?

## Pretty sure the documentation is wrong for dt()... it's just giving you
## the equation for the student t-distributed random variable.  But even
## that is wrongly implemented in c++ code... "V" is being calculated as
## the pdf when we need the noncentral Chisquared-distributed random variable






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

