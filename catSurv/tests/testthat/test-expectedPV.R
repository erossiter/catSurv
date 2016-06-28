library(catSurv)
library(testthat)
library(ltm)
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
  

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  binary_cat <- ltmCat(binary_data)
  poly_cat <- grmCat(poly_data)
  
  binary_cat@answers <- as.numeric(binary_data[1,])
  binary_cat@answers[35:40] <- NA
  for(i in which(is.na(binary_cat@answers))){
    expectedPV(binary_cat, 34)
  }
  
  
  differences <- numeric(nrow(poly_data))
  for(i in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[i, ])
    differences[i] <- estimateSE(poly_cat) - estimateSE_test(poly_cat)
  }
  
  
})


