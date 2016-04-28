library(catSurv)
library(testthat)
context("d2LL")


test_that("dichotomous case of d2LL calculates correctly",{
  
  d2LL_test_bi <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
    unanswered_questions <- length(is.na(cat@answers))
    Lambda_theta <- 0
    sum_this <- rep(0, length(unanswered_questions))
    if(length(unanswered_questions) == 0) {
      Lambda_theta <- -(1 / cat@priorParams[2]^2)
    }
    if(usePrior == FALSE) {
      for(i in 1:length(unanswered_questions)) {
        P <- probability(cat, theta, question)$all.probabilities$probabilities
        Q <- 1-P
        sum_this[i] <- cat@discrimination[i]^2 * ((P-cat@guessing[i]) / (1-cat@guessing[i]))^2 * (Q/P)
      }
      Lambda_theta <- -sum(sum_this)
    }
    if(usePrior == TRUE){
      for(i in 1:length(unanswered_questions)) {
        P <- probability(cat, theta, question)$all.probabilities$probabilities
        Q <- 1-P
        sum_this[i] <- cat@discrimination[i]^2 * ((P-cat@guessing[i]) / (1-cat@guessing[i]))^2 * (Q/P)
      }
      Lambda_theta <- -sum(sum_this) - (1/cat@priorParams[2]^2)
    }
    return(Lambda_theta)
  }
  expect_equal(d2LL(test_cat, 1, TRUE), d2LL_test_bi(test_cat, 1, TRUE))
  expect_equal(d2LL(test_cat, 1, FALSE), d2LL_test_bi(test_cat, 1, FALSE))
})




test_that("Graded response case d2LL calculates correctly",{
  
  d2LL_test_poly <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
    
    unanswered_questions <- length(is.na(cat@answers))
    Lambda_theta <- 0
    sum_this <- rep(0, length(unanswered_questions))
    if(length(unanswered_questions) == 0) {
      Lambda_theta <- -(1 / cat@priorParams[2]^2)
    }
    if(usePrior == FALSE) {
      for(i in 1:length(unanswered_questions)){
        answer_k <- cat@answers[i]
        probs <- probability(cat, theta, question)$all.probabilities$probabilities
        Pstar1 <- probs[answer_k]
        Qstar1 <- 1-Pstar1
        Pstar2 <- probs[answer_k -1]
        Qstar2 <- 1 - Pstar2
        P <- Pstar2 - Pstar1
        W2 <- Pstar2 * Qstar2
        W1 <- Pstar1 * Qstar1
        sum_this[i] <- cat@discrimintation[i]^2 * ((-W1*(Qstar1-Pstar1)+W2*(Qstar2-Pstar2))/p - ((W2 - W1)^2/P^2))
      }
      Lambda_theta <- sum(sum_this)
    }
    if(usePrior == TRUE){
      for(i in 1:length(unanswered_questions)){
        answer_k <- cat@answers[i]
        probs <- probability(cat, theta, question)$all.probabilities$probabilities
        Pstar1 <- probs[answer_k]
        Qstar1 <- 1-Pstar1
        Pstar2 <- probs[answer_k -1]
        Qstar2 <- 1 - Pstar2
        P <- Pstar2 - Pstar1
        W2 <- Pstar2 * Qstar2
        W1 <- Pstar1 * Qstar1
        sum_this[i] <- sum(cat@discrimintation[i]^2 * ((-W1*(Qstar1-Pstar1)+W2*(Qstar2-Pstar2))/p - ((W2 - W1)^2/P^2)))
      }
      Lambda_theta <- sum(sum_this) - (1/cat@priorParams[2]^2)
    }
    return(Lambda_theta)
  }
  expect_equal(d2LL(test_cat, 1, TRUE), d2LL_test_poly(test_cat, 1, TRUE))
  expect_equal(d2LL(test_cat, 1, FALSE), d2LL_test_poly(test_cat, 1, FALSE))
}
)

