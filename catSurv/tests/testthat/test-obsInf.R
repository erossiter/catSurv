library(catSurv)
context("obsInf")


######## BINARY OBSINF TEST ###########

test_that("binary obsInf calculates correctly", {
  ## creating cats
  
  allTheCats<-catBiCreator(10)
  
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
    return(sample(length(x@discrimination), 1))
  })
  questionVec<-unlist(questionList)
  
  test_obsInf_bi<-function(cat="Cat", theta="numeric", item="numeric"){
    discrim = cat@discrimination[item]
    guess<-cat@guessing[item]
    probs<-probability(cat, theta, item)
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
    return(sample(length(x@discrimination), 1))
  })
  questionVec<-unlist(questionList)
  
  test_obsInf_bi<-function(cat="Cat", theta="numeric", item="numeric"){
    discrim = cat@discrimination[item]
    guess<-cat@guessing[item]
    probs<-probability(cat, theta, item)
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