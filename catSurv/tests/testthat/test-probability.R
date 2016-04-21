library(catSurv)
context("Probability")

###### BINARY PROBABILITY TEST ########

test_that("binary probability calculates correctly", {
  
  ## Creating a lot of cat objects and filling in needed slots

    catBiCreator<-function(numCats="numeric"){
    set.seed(999)
    allTheCats<-c()
    for(i in 1:numCats){
      numQuestions<-floor(abs(50*(rnorm(1))))
      newCat<-new("Cat",
                  discrimination=(10*rnorm(numQuestions)),
                  difficulty=sort(100*rnorm(numQuestions)),
                  guessing=runif(numQuestions),
                  poly=F,
                  answers=rep(NA, numQuestions))
      
      allTheCats<-c(allTheCats, newCat)
      
    }
    return(as.list(allTheCats))
  }
  
  ##running the function, creating 10 cats
  ## ADJUST THIS INPUT IF YOU WANT A SHORTER OR LONGER TEST
  allTheCats<-catBiCreator(10)

  ## setting the question and theta values for each Cat, to be used in the probability function...
  
  thetaVec<-c()
  setThetas<-function(seed="numeric"){
    set.seed(seed)
    thetaVec<-c(10*rnorm(length(allTheCats))) # drawing one theta value for each Cat (number of draws = length(allTheCats))... 
    ## ...and multiplying by 100 so the values cover a wide range
  }
  thetaVec<-setThetas(1000)
  
  questionList<-c()
  set.seed(2222)
  questionList<-lapply(allTheCats, function(x){
    #drawing a question randomly from the number of questions stored in each Cat:
    ##  (number of quesitons corresponds to the length of a Cat's discrimination vector)
    return(sample(length(x@discrimination), 1))
  })
  questionVec<-unlist(questionList)
  

  ## R test function
  probability_test_bi <- function(cat = "Cat", theta = "numeric", question = "numeric"){
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[question]
    guessing = cat@guessing[question]
    exp_prob = exp(discrimination * (theta - difficulty))
    probability <- guessing + ((1-guessing) * (exp_prob / (1 + exp_prob)))
    return(probability)
  }

  ##calculating values from real probability function 
  realFunLists<-lapply(1:length(allTheCats), function(x){
    probability(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  realFunValues<-lapply(1:length(realFunLists), function(x){
    return(as.numeric(realFunLists[[x]]$all.probabilities))
  })

### PROBABILITY NOT WORKING

  ##calculating values from the test probability function (created above)
  testFunValues<-lapply(1:length(allTheCats), function(x){
    probability_test_bi(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  
  probability(allTheCats[[5]],.9,1)
  probability_test_bi(allTheCats[[5]],.9,1)
  
  class(realFunValues[[1]])
  class(testFunValues[[1]])
  
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
  
  #expect_equal(probability(catPoly, t=1, q=1), probability_test_poly(catPoly, 1, 1))
  #expect_equal(probability(catPoly, t=1872, q=2), probability_test_poly(catPoly, 1872, 2))
  #expect_equal(probability(catPoly, t=.001, q=3), probability_test_poly(catPoly, .001, 3))
  #expect_equal(probability(catPoly, t=-90, q=4), probability_test_poly(catPoly, -90, 4))
  ## I don't know if it's a problem that the cpp function returns a List (according to main.cpp)
  ##  because according to Cat.cpp it actually returns a vector of doubles...
})

