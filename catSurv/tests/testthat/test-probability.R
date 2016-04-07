library(catSurv)
context("Probability")

test_that("probability calculates correctly", {
  
  ## Creating a cat object and filling in needed slots
  cat <- new("Cat")
  cat@discrimination <- c(2)
  cat@difficulty <- c(2)
  cat@guessing <- c(.5)
  
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

  expect_equal(probability(cat, t=1, q=1), probability_test(cat, 1, 1))
})

