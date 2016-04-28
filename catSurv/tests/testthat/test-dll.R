library(catSurv)
library(testthat)
context("dLL")


test_that("dichotomous case of dLL calculates correctly",{
  
  dLL_test_bi <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
    unanswered_questions <- length(is.na(cat@answers))
    L_theta <- 0
    sum_this <- rep(0, length(unanswered_questions))
    if(length(unanswered_questions) == 0) {
      L_theta <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
    }
    if(usePrior == FALSE) {
      for(i in 1:length(unanswered_questions)) {
        P <- probability(cat, theta, question)$all.probabilities$probabilities
        Q <- 1-P
        sum_this[i] <- cat@discrimination[i] * ((P-cat@guessing[i]) / P(1-cat@guessing[i])) * (cat@answers[i]-P)
      }
      L_theta <- sum(sum_this)
    }
    if(usePrior == TRUE){
      for(i in 1:length(unanswered_questions)) {
        P <- probability(cat, theta, question)$all.probabilities$probabilities
        Q <- 1-P
        sum_this[i] <- cat@discrimination[i] * ((P-cat@guessing[i]) / P(1-cat@guessing[i])) * (cat@answers[i]-P)
      }
      L_theta <- sum(sum_this) - ((theta - cat@priorParams[1])/cat@priorParams[2]^2)
    }
    return(L_theta)
  }
  expect_equal(dLL(cat_df=testCats, 1, use_prior=TRUE), dLL_test_bi(test_cat, 1, TRUE))
  expect_equal(dLL(test_cat, 1, FALSE), dLL_test_bi(test_cat, 1, FALSE))
})




test_that("Graded response case dLL calculates correctly",{
  
dLL_test_poly <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
  
  unanswered_questions <- length(is.na(cat@answers))
  L_theta <- 0
  sum_this <- rep(0, length(unanswered_questions))
  if(length(unanswered_questions) == 0) {
    L_theta <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
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
      sum_this[i] <- sum(cat@discrimintation[i] * ((W2 - W1)/P))
    }
    L_theta <- sum(sum_this)
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
      sum_this[i] <- sum(cat@discrimintation[i] * ((W2 - W1)/P))
    }
    L_theta <- sum(sum_this) - ((theta - cat@priorParams[1])/cat@priorParams[2]^2)
  }
  return(L_theta)
}
expect_equal(dLL(test_cat, 1, TRUE), dLL_test_poly(testCats, 1, TRUE))
expect_equal(dLL(test_cat, 1, FALSE), dLL_test_poly(test_cat, 1, FALSE))
}
)

