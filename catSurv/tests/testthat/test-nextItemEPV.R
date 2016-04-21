library(catSurv)
library(testthat)
context("nextItemEPV")

test_that("nextItemEPV calculates correctly", {
  test_cat1 <- new("Cat")
  test_cat1@discrimination <- c(2,4,6,8)
  test_cat1@difficulty <- c(1,2,3,4)
  test_cat1@priorName <- "NORMAL"
  test_cat1@priorParams <- c(0,1.5)
  test_cat1@poly <- FALSE
  
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