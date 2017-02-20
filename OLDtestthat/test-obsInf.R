library(catSurv)
context("obsInf")

rm(list=ls())

######## BINARY OBSINF TEST ###########

test_that("binary obsInf calculates correctly", {
  ## creating cats
  
  allTheCats<-catBiCreator(10, seed=343434)
  
  ## setting the question and theta values for each Cat, to be used in the obsInf function...
  
  setThetas<-function(seed=1000){
    set.seed(seed)
    thetas<-c(2*rnorm(length(allTheCats))) # drawing one theta value for each Cat (number of draws = length(allTheCats))... 
    ## ...and multiplying by 2 so the values cover a range of ~(-4,4)
  }
  thetaVec<-setThetas()
  
  set.seed(2222)
  questionList<-lapply(allTheCats, function(x){
    #drawing a question/item number randomly from the number of questions stored in each Cat:
    ##  (number of quesitons corresponds to the length of a Cat's discrimination vector)
    ## ...more specifically, drawing question numbers from the questions which have been answered
    return(sample(length(x@discrimination[!is.na(x@answers)]), 1))
  })
  questionVec<-unlist(questionList)
  
  test_obsInf_bi<-function(cat="Cat", theta="numeric", item="numeric"){
    discrim = cat@discrimination[item]
    guess<-cat@guessing[item]
    probs<-probability(cat, theta, item)$all.probabilities$probabilities
    return((discrim^2)* (( (probs-guess)/(1-guess) )^2)* (1-probs)/probs )
  }
  
  ## creating list of values as calculated by the real function
  realObsValues<-lapply(1:length(allTheCats), function(x){
    obsInf(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
    
  ## creating list of values as calculated by the test function
  testObsValues<-lapply(1:length(allTheCats), function(x){
    test_obsInf_bi(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  
  expect_equal(testObsValues, realObsValues)
  
})


rm(list=ls())

######## POLYTOMOUS OBSINF TEST ###########


test_that("polytomous obsInf calculates correctly", {
  ## creating cats
  
  allTheCats<-catPolyCreator(10)
  
  ## setting the question and theta values for each Cat, to be used in the obsInf function...
  
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
    ## ...and only selecting from question indices for which an answer has been given
    return(sample(length(x@discrimination[!is.na(x@answers)]), 1))
  })
  questionVec<-unlist(questionList)

  
  test_obsInf_poly<-function(catPoly="Cat", theta="numeric", item="numeric"){
    
    ## check if this question is within the bounds of this Cat
    if(item>length(catPoly@answers)){
      return("Question item is out of bounds: this Cat does not have that many questions")
    }
    
    ## check if this question has been answered:
    if(is.na(catPoly@answers[item])){
      return("Question item has not been answered")
    }
    
    ##if both these checks pass, move on...
    
    ## creating pkExact: vector of each 
    
    ##indices of questions that have been answered:
    answered_indices<-which(!is.na(catPoly@answers), arr.ind=T)
    ##storing answers for question items that have been answered
    ansVec<-catPoly@answers[answered_indices]
    
    if(length(answered_indices)>0){
      p_ikList<-lapply(answered_indices, function(i){
        probability(catPoly, theta, i)$all.probabilities$probabilities
      })
      
      ## now, need to convert each value in each vector...
      ##  ...from "the probability of a response in a category strictly higher than k"...
      ##  ...to, "the probability of a response in exactly category k"
      
      p_ikListExact<-p_ikList ##copy list, for dimensions
      
      for (i in 1:length(p_ikList)){ ##iterating over items...
        for(k in 1:length(p_ikList[[i]])){ ##iterating over response categories
          if(k==1){ ## p_ikListExact[[i]][k] = p_ikList[[i]][k-1] - p_ikList[[i]][k]...
            ## ...but p_ikList[[i]][0] = 1, as no responses are in category k=0 (so all responses are above k=0)
            ##  (see note in 3.1.2, between equations (4) and (5))
            p_ikListExact[[i]][k]<-1-p_ikList[[i]][k]
          }
          else {  ## for all answers in response category higher than 1...
            p_ikListExact[[i]][k]<-p_ikList[[i]][k-1]-p_ikList[[i]][k]
          }
        }
      }
      
      

      
    
    
    discrim = cat@discrimination[item]
    guess<-cat@guessing[item]
    probs<-probability(cat, theta, item)
    return((discrim^2)* (( (probs-guess)/(1-guess) )^2)* (1-probs)/probs )
  }
  }
    
  ## creating list of values as calculated by the real function
  realObsValues<-lapply(1:length(allTheCats), function(x){
    obsInf(allTheCats[[x]], thetaVec[x], questionVec[x])
  })

  ## creating list of values as calculated by the test function
  testObsValues<-lapply(1:length(allTheCats), function(x){
    test_obsInf_bi(allTheCats[[x]], thetaVec[x], questionVec[x])
  })
  
  expect_equal(testObsValues, realObsValues)
})
