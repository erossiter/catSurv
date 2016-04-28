library(catSurv)
library(testthat)
context("dLL")


test_that("dichotomous case of dLL calculates correctly",{
  
  dLL_test_bi <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
    unanswered_questions <- length(is.na(cat@answers))
    L_theta <- 0
    if(length(unanswered_questions) == 0) {
      return_this <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
      return(return_this)
    }
    if(usePrior == FALSE) {
      for(i in 1:length(unanswered_questions)) {
        P <- probability(cat, theta, question)
        Q <- 1-P
        sum_this <- cat@discrimination[i] * ((P-cat@guessing[i]) / P(1-cat@guessing[i])) * (cat@answers[i]-P)
        L_theta <- sum_this
      }
      return(L_theta)
    }
    if(usePrior == TRUE){
      L_theta <- sum_this - ((theta - cat@priorParams[1])/cat@priorParams[2]^2)
      return(L_theta)
    }
  }
  expect_equal(dLL(test_cat, 1, TRUE), dLL_test_bi(test_cat, 1, TRUE))
  expect_equal(dLL(test_cat, 1, FALSE), dLL_test_bi(test_cat, 1, FALSE))
})




test_that("Graded response case dLL calculates correctly",{
  
dLL_test_poly <- function(cat="Cat", theta="numeric", usePrior=TRUE) {
  
  unanswered_questions <- length(is.na(cat@answers))
  L_theta <- 0
  if(length(unanswered_questions) == 0) {
    return_this <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
    return(return_this)
  }
  if(usePrior == FALSE) {
    for(i in 1:length(unanswered_questions)){
      answer_k <- cat@answers[i]
      probs <- probability(cat, theta, question)
      Pstar1 <- probs[answer_k]
      Qstar1 <- 1-Pstar1
      Pstar2 <- probs[answer_k -1]
      Qstar2 <- 1 - Pstar2
      P <- Pstar2 - Pstar1
      W2 <- Pstar2 * Qstar2
      W1 <- Pstar1 * Qstar1
      sum_this <- cat@discrimintation[i] * ((W2 - W1)/P)
    }
    L_theta <- sum_this
    return(L_theta)
  }
  if(usePrior == TRUE){
    L_theta <- sum_this - ((theta - cat@priorParams[1])/cat@priorParams[2]^2)
    return(L_theta)
  }
  expect_equal(dLL(test_cat, 1, TRUE), dLL_test_poly(test_cat, 1, TRUE))
  expect_equal(dLL(test_cat, 1, FALSE), dLL_test_poly(test_cat, 1, FALSE))
}
}
)

