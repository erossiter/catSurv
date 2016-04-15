library(catSurv)
context("Probability")

###### BINARY PROBABILITY TEST ########

test_that("binary probability calculates correctly", {
  
  ## Creating a lot of cat objects and filling in needed slots

    catBiCreator<-function(numCats="numeric"){
    set.seed(999)
    allTheCats<-c()
    for(i in 1:numCats){
      numQuestions<-floor(abs(50*(rnorm(1)))) ## 50 is arbitrary... goal is that most Cats have between 0-50 questions, with some higher outliers
      newCat<-new("Cat",
                  discrimination=(100*rnorm(numQuestions)),
                  difficulty=sort(100*rnorm(numQuestions)),
                  guessing=runif(numQuestions),
                  poly=rep(F, numQuestions),
                  answers=rep(F, numQuestions))
      
      allTheCats<-c(allTheCats, newCat)
      
    }
    return(as.list(allTheCats))
  }
  
  #### NOTE: there should be NA values somewhere in here....
  
  ##running the function, creating 10 cats
  ## ADJUST THIS INPUT IF YOU WANT A SHORTER OR LONGER TEST
  allTheCats<-catBiCreator(10)
  length(allTheCats)
  ## setting the question and theta values for each Cat, to be used in the probability function...
  
  thetaVec<-c()
  setThetas<-function(seed="numeric"){
    set.seed(seed)
    thetaVec<-c(1000*rnorm(length(allTheCats))) # drawing one theta value for each Cat (number of draws = length(allTheCats))... 
    ## ...and multiplying by 1000 so the values cover a wide range
  }
  thetaVec<-setThetas(1000)
  
  questionList<-c()
  set.seed(2222)
  questionList<-lapply(allTheCats, function(x){
    #drawing a question randomly from the number of questions stored in each Cat:
    ##  (number of quesitons corresponds to the length of a Cat's discrimination vector)
    #return(length(x@discrimination))
    return(sample(1:length(x@discrimination), 1))
    #return(ceiling(runif(1)*length(x@discrimination))) #rounding up: question indices are integer values, and there is no question zero
  })
  questionVec<-unlist(questionList)
  

  ## R test function
  probability_test_bi <- function(cat = "Cat", theta = "numeric", question = "numeric"){
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[question]
    guessing = cat@guessing[question]
    exp_prob = exp(discrimination * (theta - difficulty))
    probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
    return(probability)
  }

  ##calculating values from real probability function 
  
  #realFunValues<-lapply(1:length(allTheCats), function(x){
  #  probability(allTheCats[[x]], thetaVec[x], questionVec[x])
  #})  
  
  #lapply won't work. using a for loop :D
  
  realFunValues<-as.list(c())
  for(i in 1:length(allTheCats)){
    thisProb<-probability(allTheCats[[i]], thetaVec[i], questionVec[i])
    realFunValues[[i]]<-thisProb
    }

  ##calculating values from the test probability function (created above)
  
  #testFunValues<-lapply(allTheCats, function(x){
  #  probability_test_bi(x, thetaVec(which(allTheCats, x)), questionVec(which(allTheCats, x)))
  #})
  testFunValues<-as.list(c())
  for(i in 1:length(allTheCats)){
    thisProb<-probability_test_bi(allTheCats[[i]], thetaVec[i], questionVec[i])
    testFunValues[[i]]<-thisProb
  }
  
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

