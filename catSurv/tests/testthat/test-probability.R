library(catSurv)
context("Probability")

###### BINARY PROBABILITY TEST ########

test_that("binary probability calculates correctly", {
  
  ## Creating a lot of cat objects and filling in needed slots
  
  catBiCreator<-function(numCats=10){
    set.seed(999)
    allTheCats<-as.list(rep(NA, numCats))
    for(i in 1:numCats){
      newCat<-new("Cat")
      numQuestions<-floor(abs(50*(rnorm(1))))
      newCat@discrimination<-sort(100*rnorm(numQuestions))
      newCat@difficulty<-sort(100*rnorm(numQuestions))
      newCat@difficulty<-runif(numQuestions)
      allTheCats[[i]]<-newCat
    }
  }
  
  
  ## R test function
  probability_test_bi <- function(cat = "Cat", theta = "numeric", question = "numeric"){
	  discrimination = cat@discrimination[question]
	  difficulty = cat@difficulty[question]
	  guessing = cat@guessing[question]
	  exp_prob = exp(discrimination * (theta - difficulty))
	  probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
	  return(probability)
	  }

  thetaVec<-
  funValues<-lapply(allTheCats, function(x){
    
    probability_test_bi(x, )})
  testValues<-
  expect_equal(probability(catBi, t=1, q=1), probability_test_bi(catBi, 1, 1))
  expect_equal(probability(catBi, t=1872, q=2), probability_test_bi(catBi, 1872, 2))
  expect_equal(probability(catBi, t=.001, q=3), probability_test_bi(catBi, .001, 3))
  expect_equal(probability(catBi, t=-90, q=4), probability_test_bi(catBi, -90, 4))
  
})

###### POLYTOMOUS PROBABILITY TEST ########

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
  probability_test_poly <- function(cat = "Cat", theta = "numeric", question = "numeric"){
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[[question]]
    probVec <- c()
    for(k in 1:length(difficulty)){
      exp_prob = exp(discrimination * (theta - difficulty[k]))
      probK<-(exp_prob/(1+exp_prob))
      probVec<-c(probVec, probK)
    }
    return(as.list(probVec))
  }
  
  expect_equal(probability(catPoly, t=1, q=1), probability_test_poly(catPoly, 1, 1))
  expect_equal(probability(catPoly, t=1872, q=2), probability_test_poly(catPoly, 1872, 2))
  expect_equal(probability(catPoly, t=.001, q=3), probability_test_poly(catPoly, .001, 3))
  expect_equal(probability(catPoly, t=-90, q=4), probability_test_poly(catPoly, -90, 4))
  ## I don't know if it's a problem that the cpp function returns a List (according to main.cpp)
  ##  because according to Cat.cpp it actually returns a vector of doubles...
})

