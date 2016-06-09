library(catSurv)
context("Probability")

test_that("probability calculates correctly", {
  
  # R test function
  probability_test <- function(cat = "Cat", theta = "numeric", question = "numeric"){
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[[question]]
    guessing = cat@guessing[question]
    
    # For binary probability
    if (cat@poly == F) {
    exp_prob = exp(difficulty + (theta * discrimination))
    probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
    
    # For polytomous probability
    } else {
      probability <- rep(NA,length(difficulty))
      for(k in 1:length(difficulty)){
        exp_prob = exp((difficulty[k]) - (theta * discrimination))
        probK <- (exp_prob/(1+exp_prob))
        probability[k] <- probK
      }
    }
    return(probability)
  }

  ##### Note that the following functions require data inputs before test_that can operate
  
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

