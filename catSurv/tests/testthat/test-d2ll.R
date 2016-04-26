library(catSurv)
library(testthat)
context("d2LL")


test_that("d2LL calculates correctly",{
  
  dLL_test <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
    answered_questions <- cat@applicable_rows
    L_theta <- 0
    if(length(answered_questions) == 0) {
      return_this <- ( 1/ cat@priorParams[1]^2)
    }
    if(cat@poly == FALSE) {
      for(i in 1:length(answered_questions)) {
        P <- probability(cat, theta, answered_questions[i])
        Q <- 1-P
        L_theta <- L_theta + cat@discrimination * (P-cat@guessing / P(1-cat@guessing)) * (cat@answers[i]-P)
      }
    }
    if(cat@poly == TRUE) {
      for(i in 1:length(answered_questions)){
        answer_k <- cat@answers[i]s
        probs <- probability(cat, theta, answered_questions[i])
        Pstar1 <- probs[answer_k]
        Qstar1 <- 1-Pstar1
        Pstar2 <- probs[answer_k -1]
        Qstar2 <- 1 - Pstar2
        P <- Pstar2 - Pstar1
        W2 <- Pstar2 * Qstar2
        W1 <- Pstar1 * Qstar1
        L_theta <- L_theta + (cat@discrimintation[item]^2 * (-W2*(Qstar1-Pstar1) +W2*(Qstar2-Pstar2)/P -(W2-W1)^2/p^2)
      }
    }
    if(usePrior == TRUE){
      L_theta <- L_theta - (1/cat@priorParams[1]^2)
    }
    return(L_theta)
  }
  expect_equal(d2LL(cat, t=1, usePrior=TRUE), d2LL_test(test_cat, 1, usePrior=TRUE))
  expect_equal(d2LL(cat, t=1, usePrior=FALSE), d2LL_test(test_cat, 1, usePrior=FALSE))
})
