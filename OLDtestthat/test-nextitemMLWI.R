library(catSurv)
library(testthat)
context("nextItemMLWI")

test_that("nextItemMLWI calculates correctly", {

  nextItemMLWI_test <- function(cat = "Cat"){
    library(stats)
    unanswered_questions <- which(is.na(cat@answers))
    LWI <- rep(0, length(unanswered_questions))
    for(i in 1:length(unanswered_questions)){
      LWI[i] <- fisherInf(cat, unanswered_questions[i], theta)*likelihood(cat, theta)
    }
    maxLWI <- max(integrate(LWI, -1, 1)$value)
    next_item <- which(max(integrate(LWI, -1, 1)$value))
    return_list <- list(maxLWI, next_item)
    return(return_list)
  }
  expect_equal(nextItemMLWI_test(testCats[[1]]), nextItemMLWI(testCats[[1]]))
})
