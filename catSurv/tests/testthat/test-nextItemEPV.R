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
  
  #lapply(c(catBiCreator(5), catPolyCreator(5)),
  #       function(x) expect_equal(nextItem(x), nextItemEPV_test(x), tolerance = .1))
  
  # for(i in 1:length(testCats)){
  #   ##picking the first item that is NA
  #   item <- min(which(is.na(testCats[[i]]@answers)))
  #   print(nextItem(testCats[[i]]) - expectedPV_test(testCats[[i]], item))
  # }
})