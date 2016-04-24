library(catSurv)
context("Probability")

###### BINARY PROBABILITY TEST ########

test_that("binary probability calculates correctly", {
  
  ## Creating a lot of cat objects and filling in needed slots
  ## ADJUST THIS INPUT IF YOU WANT A SHORTER OR LONGER TEST
  allTheCats<-catBiCreator(10)

  ## setting the question and theta values for each Cat, to be used in the probability function...
  
  setThetas<-function(seed="numeric"){
    set.seed(seed)
    thetas<-c(2*rnorm(length(allTheCats))) # drawing one theta value for each Cat (number of draws = length(allTheCats))... 
    ## ...and multiplying by 2 so the values cover a range of ~(-4,4)
  }
  thetaVec<-setThetas(1000)
  
  set.seed(2222)
  questionList<-lapply(allTheCats, function(x){
    #drawing a question/item number randomly from the number of questions stored in each Cat:
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
    probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
    return(probability)
  }

  ##calculating values from real probability function (found in Estimator.cpp)
  realFunValues<-lapply(1:length(allTheCats), function(x){
    return(as.numeric((probability(allTheCats[[x]], thetaVec[x], questionVec[x]))$all.probabilities))
  })

  ##calculating values from the test probability function (created above)
  testFunValues<-lapply(1:length(allTheCats), function(x){
    probability_test_bi(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  

  
  ##expect the values to be equal
  expect_equal(realFunValues, testFunValues)
  
})

###### POLYTOMOUS PROBABILITY TEST ########

test_that("polytomous probability calculates correctly", {
  
  ## Polytoumous Cat creator function
  catPolyCreator<-function(numCats="numeric", spread=2, seed=272){
    set.seed(seed)
    allTheCats<-c()
    for(i in 1:numCats){
      numQuestions<-2+floor(abs(50*(rnorm(1))))
      newCat<-new("Cat",
                  discrimination=(spread*rnorm(numQuestions)),
                  difficulty=lapply(1:numQuestions, function(x){
                    sort(spread*rnorm(sample(10, 1, replace=T)))}) ,
                  guessing=.1*runif(numQuestions),
                  poly=T,
                  answers=rep(NA, numQuestions))
      
      allTheCats<-c(allTheCats, newCat)
      
    }
    return(as.list(allTheCats))
  }
 
  allPolyCats<-catPolyCreator(10)
  

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
  
  setThetas<-function(spread=2, seed=754){
    set.seed(seed)
    return(spread*rnorm(length(allPolyCats))) # drawing one theta value for each Cat (number of draws = length(allPolyCats))... 
    ## ...and multiplying by a spread factor so the values cover a certain range
  }
  thetaVec<-setThetas()
  

  
  questionList<-lapply(allTheCats, function(x, seed=2534){
    set.seed(seed)
    #drawing a question randomly from the number of questions stored in each Cat:
    ##  (number of quesitons corresponds to the length of a Cat's discrimination vector)
    return(sample(length(x@discrimination), 1))
  })
  questionVec<-unlist(questionList)
  
  probability(allPolyCats[[1]], 2.3, 2)
  probability_test_poly(allPolyCats[[1]], 2.3, 2)
  
  ##calculating values from real probability function 
  realFunLists<-lapply(1:length(allTheCats), function(x){
    probability(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  realFunValues<-lapply(1:length(realFunLists), function(x){
    return(as.numeric(realFunLists[[x]]$all.probabilities))
  })
  
  
  ##calculating values from the test probability function (created above)
  testFunValues<-lapply(1:length(allTheCats), function(x){
    probability_test_bi(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  
  expect_equal(realFunValues, testFunValues)
  
})

