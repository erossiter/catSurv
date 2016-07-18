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
    if(distribution == "UNIFORM"){
      prior_values <- dunif(x, cat@lowerBound, cat@upperBound)
      }
    return(prior_values)
  }
  
  x.val <- sample(1:10, 1)

  data("npi")
  data("nfc")
  data("AMTknowledge")
  ltm_data <- npi[1:100, ]
  tpm_data <- AMTknowledge[1:100, ]
  poly_data <- nfc[1:100, ]
  
  binary_cat.ltm <- ltmCat(ltm_data, 100)
  binary_cat.tpm <- tpmCat(tpm_data, 100)
  poly_cat <- grmCat(poly_data, 100)
  
  ## ltmCat
  
  # NORMAL
  binary_cat.ltm@priorName <- "NORMAL"
  expect_equal(prior(x = x.val, binary_cat.ltm@priorName, binary_cat.ltm@priorParams), 
               prior_test(x = x.val, binary_cat.ltm),
               tolerance = .00001)
  
  # STUDENT_T
  binary_cat.ltm@priorName <- "STUDENT_T"
  expect_equal(prior(x = x.val, binary_cat.ltm@priorName, binary_cat.ltm@priorParams), 
               prior_test(x = x.val, binary_cat.ltm),
               tolerance = .00001)
  
  # UNIFORM
  binary_cat.ltm@priorName <- "UNIFORM"
  binary_cat.ltm@priorParams <- c(binary_cat.ltm@lowerBound, binary_cat.ltm@upperBound)
  expect_equal(prior(x = x.val, binary_cat.ltm@priorName, binary_cat.ltm@priorParams), 
               prior_test(x = x.val, binary_cat.ltm),
               tolerance = .00001)  
  
  ## tpmCat
  
  # NORMAL
  binary_cat.tpm@priorName <- "NORMAL"
  expect_equal(prior(x = x.val, binary_cat.tpm@priorName, binary_cat.tpm@priorParams), 
               prior_test(x = x.val, binary_cat.tpm),
               tolerance = .00001)
  
  # STUDENT_T
  binary_cat.tpm@priorName <- "STUDENT_T"
  expect_equal(prior(x = x.val, binary_cat.tpm@priorName, binary_cat.tpm@priorParams), 
               prior_test(x = x.val, binary_cat.tpm),
               tolerance = .00001)
  
  # UNIFORM
  binary_cat.tpm@priorName <- "UNIFORM"
  binary_cat.tpm@priorParams <- c(binary_cat.tpm@lowerBound, binary_cat.tpm@upperBound)
  expect_equal(prior(x = x.val, binary_cat.tpm@priorName, binary_cat.tpm@priorParams), 
               prior_test(x = x.val, binary_cat.tpm),
               tolerance = .00001)  
  
  ## grmCat
  
  # NORMAL
  poly_cat@priorName <- "NORMAL"
  expect_equal(prior(x = x.val, poly_cat@priorName, poly_cat@priorParams), 
               prior_test(x = x.val, poly_cat),
               tolerance = .00001)
  
  # STUDENT_T
  poly_cat@priorName <- "STUDENT_T"
  expect_equal(prior(x = x.val, poly_cat@priorName, poly_cat@priorParams), 
               prior_test(x = x.val, poly_cat),
               tolerance = .00001)
  
  # UNIFORM
  poly_cat@priorName <- "UNIFORM"
  poly_cat@priorParams <- c(poly_cat@lowerBound, poly_cat@upperBound)
  expect_equal(prior(x = x.val, poly_cat@priorName, poly_cat@priorParams), 
               prior_test(x = x.val, poly_cat),
               tolerance = .00001)  
  
  
})





