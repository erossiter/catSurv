library(catSurv)
library(testthat)
context("expectedObsInf")

test_that("expectedObsInf calculates correctly", {
  
  expectedObsInf_test <- function(cat, item){
  if(cat@poly == FALSE){
      ## Probability they get it right
      ## and obsInf if they get it right
      pr_correct <- probability(cat, estimateTheta(cat), item)$all.probabilities$probabilities
      cat@answers[item] <- 1 
      inf_correct <- obsInf(cat, estimateTheta(cat), cat@answers[item])
    
      ## Probability they get it wrong,
      ## and obsInf *if* they get it wrong
      pr_incorrect <- 1 - pr_correct
      cat@answers[item] <- 0 
      inf_incorrect <- obsInf(cat, estimateTheta(cat),  cat@answers[item])
    
      cat@answers[item] <- NA
      item_EI <- (pr_correct * inf_correct) + (pr_incorrect * inf_incorrect)
  }
  
    if(cat@poly == TRUE){
      item_probabilities <- function(cat){
      ## temporarily changing cat to just have this question's info
      cat@difficulty <- cat@difficulty[[item]]
      cat@discrimination <- cat@discrimination[item]
      cat@guessing <- cat@guessing[item]
      result <- probability(cat, estimateTheta(cat))$all.probabilities$probabilities
      return(result)
      }
      item_thetas <- rep(NA, length(cat@difficulty[[item]]))
      item_inf <- rep(NA, length(cat@difficulty[[item]]))
      for(i in 1:length(item_thetas)){
        cat@answers <- i
        item_inf[i] <- obsInf(cat, estimateTheta(cat),cat@answers[item])
      }
      cat@answers[item] <- NA
      item_EI <- sum(item_probabilities * item_inf)
    }
    return(item_EI)
  }
  #expect_equal(expectedObsInf(), expectedObsInf_test())
})


for(i in 1:length(testCats)){
   ##picking the first item that is NA
   item <- min(which(is.na(testCats[[i]]@answers)))
   print(expectedObsInf(testCats[[i]], item) - expectedObsInf_test(testCats[[i]], item))
}

item <- min(which(is.na(testCats[[2]]@answers)))
expectedObsInf(testCats[[2]], item) 
expectedObsInf_test(testCats[[7]], item)


