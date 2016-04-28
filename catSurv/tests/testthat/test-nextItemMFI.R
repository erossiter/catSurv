library(catSurv)
library(testthat)
context("nextItemMFI")

test_that("nextItemMFI calculates correctly", {

  nextItemMFI_test <- function(cat = "Cat", theta = "numeric", items = "numeric"){
    unanswered_questions <- which(is.na(cat@answers))
    questions_fisherInf <- rep(0, length(unanswered_questions))
    for(i in 1:length(unanswered_questions)){
      questions_fisherInf[i] <- fisherInf(cat, unanswered_questions[i])
    }
    max_fisher <- max(questions_fisherInf)
    next_item <- which(max(questions_fisherInf))
    return_this <- list(max_fisher, next_item)
    return(return_this)
  }
  expect_equal(nextItemMFI_test(test_cat), nextItemMFI(test_cat))
})
