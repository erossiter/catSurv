library(catSurv)
context("Probability")

test_that("binary probability calculates correctly", {
  
  ## Creating a cat object and filling in needed slots
  
  catBi <- new("Cat")
  catBi@discrimination <- c(2,4,6,8)
  catBi@difficulty <- c(3,5,-12.27,9)
  catBi@guessing <- c(.5, .1, .32, .999)
  
  ## R test function
  probability_test <- function(cat, theta, question){
    D = cat@D
	  discrimination = cat@discrimination[question]
	  difficulty = cat@difficulty[question]
	  guessing = cat@guessing[question]
	  exp_prob = exp(D * discrimination * (theta - difficulty))
	  probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
	  return(probability)
	  }

  expect_equal(probability(catBi, t=1, q=1), probability_test(catBi, 1, 1))
  expect_equal(probability(catBi, t=1872, q=2), probability_test(catBi, 1872, 2))
  expect_equal(probability(catBi, t=.001, q=3), probability_test(catBi, .001, 3))
  expect_equal(probability(catBi, t=-90, q=4), probability_test(catBi, -90, 4))
  
})

test_that("polytomous probability calculates correctly", {
  
  ## Creating a cat object and filling in needed slots
  
  catBi <- new("Cat")
  catBi@discrimination <- c(2,4,6,8)
  catBi@difficulty <- c(3,5,-12.27,9)
  catBi@guessing <- c(.5, .1, .32, .999)
  
  ## R test function
  probability_test <- function(cat, theta, question){
    D = cat@D
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[question]
    guessing = cat@guessing[question]
    exp_prob = exp(D * discrimination * (theta - difficulty))
    probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
    return(probability)
  }
  
  expect_equal(probability(catBi, t=1, q=1), probability_test(catBi, 1, 1))
  expect_equal(probability(catBi, t=1872, q=2), probability_test(catBi, 1872, 2))
  expect_equal(probability(catBi, t=.001, q=3), probability_test(catBi, .001, 3))
  expect_equal(probability(catBi, t=-90, q=4), probability_test(catBi, -90, 4))
  
})

