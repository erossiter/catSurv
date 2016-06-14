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
      # pr_item <- function(cat){
      # ## temporarily changing cat to just have this question's info
      #   setDifficulty(cat) <- cat@difficulty[[item]]
      #   setDiscrimination(cat) <- cat@discrimination[item]
      #   setGuessing(cat) <- cat@guessing[item]
      #   result <- probability(cat, estimateTheta(cat), item)$all.probabilities$probabilities
      #   return(result)
      # }
      
      item_probabilities <- probability(cat, estimateTheta(cat), item)$all.probabilities$probabilities
      #item_thetas <- rep(NA, length(item_probabilities))
      item_vars <- rep(NA, length(item_probabilities))
      for(i in 1:length(item_probabilities)){
        cat@answers[item] <- i
        item_vars[i] <- estimateSE(cat)^2
      }
      cat@answers[item] <- NA
      #print(item_vars)
      
      item_EPV <- sum(item_probabilities * item_vars)
    }
    return(item_EPV)
  }
  

    for(i in 1:4){
      ##picking the first item that is NA
      item <- min(which(is.na(testCats[[i]]@answers)))
      print(expectedPV(testCats[[i]], item) - expectedPV_test(testCats[[i]], item))
      #print(expectedPV(testCats[[i]], item))
      #print(expectedPV_test(testCats[[i]], item))
      print("")
    }
  expectedPV(testCats[[1]], 10)
  expectedPV_test(testCats[[1]], 8)
})


