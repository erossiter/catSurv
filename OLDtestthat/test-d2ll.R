library(catSurv)
library(testthat)
context("d2LL")


test_that("d2LL calculates correctly",{
  
  d2LL_test <- function(cat, theta, usePrior) {
    answered_questions <- which(!is.na(cat@answers))
    prior_shift <- 1 / cat@priorParams[2]^2
    
    if(length(answered_questions) == 0) {
      L_theta <- prior_shift
    }
    
    sum_this <- rep(0, length(answered_questions))
    
    if(cat@poly == FALSE){
      for(i in 1:length(answered_questions)){
        item <- answered_questions[i]
        P <- probability(cat, theta, item)$all.probabilities$probabilities
        Q <- 1-P
        sum_this[i] <- cat@discrimination[i]^2 * ((P-cat@guessing[i]) / (1-cat@guessing[i]))^2 * (Q/P)
      }
      L_theta <- - sum(sum_this)
    }
    if(cat@poly == TRUE){
      for(i in 1:length(answered_questions)){
        item <- answered_questions[i]
        answer_k <- cat@answers[item]
        probs <- probability(cat, theta, item)$all.probabilities$probabilities[c(2:5)]
        Pstar1 <- probs[answer_k]
        Qstar1 <- 1-Pstar1
        Pstar2 <- probs[answer_k -1]
        ## didn't want to add 0 as first element of probs vector
        ## so this takes care of indexing one before the first prob
        if(length(Pstar2) == 0) Pstar2 <- 0
        Qstar2 <- 1 - Pstar2
        P <- Pstar2 - Pstar1
        W2 <- Pstar2 * Qstar2
        W1 <- Pstar1 * Qstar1
        sum_this[i] <- cat@discrimination[i]^2 * ((-W2*(Qstar2-Pstar2)+W1*(Qstar1-Pstar1)) / 
                                                      P - ((W1 - W2)^2/P^2))
      }
      L_theta <- sum(sum_this)
    }
    if(usePrior == TRUE){
      L_theta <- L_theta - prior_shift
    }
  return(L_theta)
  }
  
 d2LL_test(testCats[[3]], theta = 1, usePrior = F) 
 d2LL(testCats[[3]], 1, F)
 
  d2LL_test(testCats[[5]], theta = 1, usePrior = F) 
 d2LL(testCats[[5]], 1, F)
  
})

cat <-testCats[[5]]
