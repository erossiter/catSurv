library(catSurv)
library(testthat)
context("expectedPV")

test_that("expectedPV calculates correctly", {
  
  expectedPV_test <- function(cat, item){
    test_cat1 <- new("Cat")
    test_cat1@discrimination <- c(2,4,6,8)
    test_cat1@difficulty <- c(1,2,3,4)
    test_cat1@priorName <- "NORMAL"
    test_cat1@priorParams <- c(0,1.5)
    test_cat1@poly <- FALSE
  
    if(cat@poly == FALSE){
      ## Probability they get it right
      ## and variance *if* they get it right
      pr_correct <- probability(cat, estimateTheta(cat), item)
      cat@answers[item] <- 1 
      var_correct <- estimateSE(cat)^2
    
      ## Probability they get it wrong,
      ## and variance *if* they get it wrong
      pr_incorrect <- 1 - pr_correct
      cat@answers[item] <- 0 
      var_incorrect <- estimateSE(cat)^2
    
      cat@answers[item] <- NA
    
      item_EPV <- (pr_correct * var_correct) + (pr_incorrect * var_incorrect)
    }
  
  
    if(cat@poly == TRUE){
      item_probabilities <- function(cat){
      ## temporarily changing cat to just have this question's info
      cat@difficulty <- cat@difficulty[[item]]
      cat@discrimination <- cat@discrimination[item]
      cat@guessing <- cat@guessing[item]
      result <- probability(cat, estimateTheta(cat))
      return(result)
      }
    
      item_thetas <- rep(NA, length(cat@difficulty[[item]]))
      item_vars <- rep(NA, length(cat@difficulty[[item]]))
      for(i in 1:length(item_thetas)){
        cat@answers <- i
        item_vars[i] <- estimateSE(cat)^2
      }
      cat@answers[item] <- NA
    
      itemEPV <- sum(item_probabilities * item_vars)
    }
    return(item_EPV)
  }
  
  expect_equal(expectedPV(test_cat1, 2), expectedPV_test(test_cat1, 1))
})
