library(catSurv)
context("Probability")

###### BINARY PROBABILITY TEST ########

test_that("binary probability calculates correctly", {
  
  ## Creating a lot of cat objects and filling in needed slots
  
  catBiCreator<-function(numCats="numeric"){
    set.seed(999)
    allTheCats<-as.list(rep(NA, numCats))
    for(i in 1:numCats){
      newCat<-new("Cat")
      ## randomly assigning number of questions for each Cat
      numQuestions<-floor(abs(50*(rnorm(1))))
      newCat@discrimination<-sort(100*rnorm(numQuestions))
      newCat@difficulty<-sort(100*rnorm(numQuestions))
      newCat@difficulty<-runif(numQuestions)
      allTheCats[[i]]<-newCat
    }
  }
  
  #### NOTE: there should be NA values somewhere in here....
  
  ##running the function, creating 10 cats
  ## ADJUST THIS INPUT IF YOU WANT A SHORTER OR LONGER TEST
  catBiCreator(10)
  
  ## R test function
  probability_test_bi <- function(cat = "Cat", theta = "numeric", question = "numeric"){
	  discrimination = cat@discrimination[question]
	  difficulty = cat@difficulty[question]
	  guessing = cat@guessing[question]
	  exp_prob = exp(discrimination * (theta - difficulty))
	  probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
	  return(probability)
	  }

  ## setting the question and theta values for the test...
  thetaVec<-c()
  questionVec<-c()
  setThetaQuestion<-function(seed="numeric"){
    set.seed(seed)
    thetaVec<-c(1000*rnorm(length(allTheCats)))
    questionVec<-c(floor(1000*runif(length(allTheCats))))
  }
  setThetaQuestion(1000)
  
  ##### NEED A CHECK ON WHETHER QUESTIONS ARE IN BOUNDS 
  #####    (ie is each value in questionVec smaller than the number of questions in its corresponding Cat)
  
  
  ##calculating values from real probability function
  realFunValues<-lapply(length(allTheCats), function(x){
    probability(allTheCats[[x]], thetaVec(x), questionVec(x))})
  
  ##calculating values from the test probability function (created above)
  testFunValues<-lapply(length(allTheCats), function(x){
    probability_test_bi(allTheCats[[x]], thetaVec(x), questionVec(x))})
  
  ##expect the values to be equal
  expect_equal(realFunValues, testFunValues)
  
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

