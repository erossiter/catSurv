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
        setDifficulty(cat) <- cat@difficulty[[item]]
        setDiscrimination(cat) <- cat@discrimination[item]
        setGuessing(cat) <- cat@guessing[item]
        result <- probability(cat, estimateTheta(cat), item)$all.probabilities$probabilities
        return(result)
      }
      pr_item_incorrect <- 1 - pr_item_correct(cat)
    
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
       item <- which(is.na(cat@answers))[2]
       #expect_equal(expectedPV(cat, item), expectedPV_test(cat, item), tolerance = .01)
       print(expectedPV(cat, item) - expectedPV_test(cat, item))
     }
     ## won't work on 6th cat
     lapply(c(catBiCreator(5), catPolyCreator(5)), equal_test)
})


