library(catSurv)
library(testthat)
context("nextItemMEI")

test_that("nextItemMEI calculates correctly", {
  nextItemMEI_test <- function(cat){
    unanswered_questions <- which(is.na(cat@answers))
    questions_EI <- rep(NA, length(unanswered_questions))
    for(i in 1:length(unanswered_questions)){
      questions_EI[i] <- expectedObsInf(cat, unanswered_questions[i])
      }
    next_item <- which(max(questions_EI))
    return(next_item)
  }
  #expect_equal(nextItemMEI(), nextItemMEI())
})