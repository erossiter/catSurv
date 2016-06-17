library(catSurv)
library(testthat)
context("dLL")

test_that("dLL calculates correctly", {
  
  dLL_test <- function(cat, theta, usePrior) {
    answered_questions <- which(!is.na(cat@answers))
    prior_shift <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
    
    if(length(answered_questions) == 0) {
      L_theta <- prior_shift
    }
    
    sum_this <- rep(0, length(answered_questions))

    if(cat@poly == FALSE){
      for(i in 1:length(answered_questions)){
        item <- answered_questions[i]
        P <- probability(cat, theta, item)$all.probabilities$probabilities
        Q <- 1-P
        sum_this[i] <- cat@discrimination[i] * (cat@answers[i]-P) *
          ( (P - cat@guessing[i]) / (P * (1 - cat@guessing[i])) )
      }
      L_theta <- sum(sum_this)
    }

    if(cat@poly == TRUE){
      for(i in 1:length(answered_questions)){
        item <- answered_questions[i]
        answer_k <- cat@answers[item]
        probs <- probability(cat, theta, item)$all.probabilities$probabilities
        Pstar1 <- probs[answer_k]
        Qstar1 <- 1-Pstar1
        Pstar2 <- probs[answer_k-1]
        ## didn't want to add 0 as first element of probs vector
        ## so this takes care of indexing one before the first prob
        if(length(Pstar2) == 0) Pstar2 <- 0
        Qstar2 <- 1 - Pstar2
        P <- Pstar1 - Pstar2
        W2 <- Pstar2 * Qstar2
        W1 <- Pstar1 * Qstar1
        sum_this[i] <- -1*cat@discrimination[i] * ((W1 - W2)/P)
      }
      L_theta <- sum(sum_this)
    }
    ## usePrior for both
    if(usePrior == TRUE){
      L_theta <- L_theta - prior_shift
    }
    return(L_theta)
  }
  
  
  
dLL(testCats[[7]], 2, T)
dLL_test(testCats[[7]], 2, T)
  
})


