library(catSurv)
library(testthat)
context("expectedPV")

test_that("expectedPV calculates correctly", {
  
  expectedPV_test <- function(cat, item){
  
    if(cat@poly == FALSE){
      ## Probability they get it right
      ## and variance *if* they get it right
      pr_correct <- probability(cat, estimateTheta(cat), item)$all.probabilities$probabilities
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
      pr_item_correct <- function(cat){
      ## temporarily changing cat to just have this question's info
        cat@difficulty <- cat@difficulty[[item]]
        cat@discrimination <- cat@discrimination[item]
        cat@guessing <- cat@guessing[item]
        result <- probability(cat, estimateTheta(cat), item)$all.probabilities$probabilities
        return(result)
      }
      pr_item_incorrect <- 1 - pr_item_correct(testCats[[7]]@difficulty)
    
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
  
     equal_test <- function(cat){
     item <- min(which(is.na(cat@answers)))
     expect_equal(expectedPV(cat, item),
                  expectedPV_test(cat, item),
                  tolerance = .01)
     }
   lapply(testCats, equal_test)
})


## Again, only testing binary b/c categorical code isn't working b/c of liklihood
expectedPV_test(cat = testCats[[3]],  item = min(which(is.na(testCats[[3]]@answers))) )

## c++ codes isn't returning anything
expectedPV(testCats[[3]], min(which(is.na(testCats[[3]]@answers))))


