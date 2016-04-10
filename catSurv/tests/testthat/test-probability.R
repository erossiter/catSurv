library(catSurv)
context("Probability")

## is it better to set this up as a single test, with an if(poly){...expect_equal(...)} else {...expect_equal(...)} ?

test_that("binary probability calculates correctly", {
  
  ## Creating a cat object and filling in needed slots
  
  catBi <- new("Cat")
  catBi@discrimination <- c(2,4,6,8)
  catBi@difficulty <- c(3,5,-12.27,9)
  catBi@guessing <- c(.5, .1, .32, .999)
  
  ## R test function
  probability_test <- function(cat = "Cat", theta = "numeric", question = "numeric"){
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
  
  catPoly <- new("Cat")
  catPoly@discrimination <- c(2,4,6,8)
  catPoly@difficulty <- list(q1=c(1,2,3,4), q2=c(-90.2, -87, -.003), q3=c(seq(-10, 10, .1)), q4=2)
  ## I can't tell from the cpp code how a cpp Cat object creates its "difficulty" slot from a polytomous S4 Cat object
  ## ...it looks like it's a vector of vectors, which I guess would be an array, except that the length of the diffuculty
  ##      parameter vector for each quesiton could be different... 
  ## So I used a list here
  ##
  ##...also, should there be a test to make sure the difficulty parameters for a given question are non-decreasing?
  

  ## R test function
  probability_test <- function(cat = "Cat", theta = "numeric", question = "numeric"){
    D = cat@D
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[[question]]
    probVec <- c()
    for(k in 1:length(difficulty)){
      exp_prob = exp(D * discrimination * (theta - difficulty[k]))
      probK<-(exp_prob/(1+exp_prob))
      probVec<-c(probVec, probK)
    }
    return(probVec)
  }
  
  expect_equal(probability(catPoly, t=1, q=1), probability_test(catPoly, 1, 1))
  expect_equal(probability(catPoly, t=1872, q=2), probability_test(catPoly, 1872, 2))
  expect_equal(probability(catPoly, t=.001, q=3), probability_test(catPoly, .001, 3))
  expect_equal(probability(catPoly, t=-90, q=4), probability_test(catPoly, -90, 4))
  ## I don't know if it's a problem that the cpp function returns a List (according to main.cpp)
  ##  because according to Cat.cpp it actually returns a vector of doubles...
})

