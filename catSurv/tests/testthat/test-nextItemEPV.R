library(catSurv)
library(testthat)
context("nextItemEPV")

test_that("nextItemEPV calculates correctly", {
  
  nextItemEPV_test <- function(cat){
    unanswered_questions <- which(is.na(cat@answers))
    questions_EPV <- rep(NA, length(unanswered_questions))
    for(i in 1:length(unanswered_questions)){
      questions_EPV[i] <- expectedPV(cat, unanswered_questions[i])
      }
    next_item <- which(min(questions_EPV))
    return(next_item)
  }
  expect_equal(nextItemEPV(test_cat1), nextItemEPV(test_cat1))
})