library(catSurv)
library(testthat)
context("CheckRules")

obj<-new("Cat")
obj
obj@answers[3]<-1
names(obj@discrimination)<-rep("Q", length(obj@discrimination))
(isTRUE(all(fishAll==0)))


expect

test_that("CheckRules functions correctly", {
  ### TEST FUNCTION
  checkRules_test_fun = function(obj = "Cat"){
    
    
    ## check that there are threshold values to enable stopping rules to be satisfied
    if(isTRUE(all(is.na(obj@lengthThreshold), is.na(obj@seThreshold), 
                  is.na(obj@infoThreshold), is.na(obj@gainThreshold)))){
      
      ### IF NO THRESHOLD VALUES PROVIDED -- error, or return false?
      
      #stop("No threshold values provided. Survey will never end.")
      return(FALSE)
    }
    ## set stopNow boolean
    stopNow = FALSE
    
    ## calculate values to compare to threshold values
    numAnswered = length(obj@answers[!is.na(obj@answers)])
    seHat = estimateSE(obj)
    thetaHat = estimateTheta(obj)
    unanswered = which(is.na(obj@answers), arr.ind = T)
    fishAll = sapply(unanswered,function(i){
      return(fisherInf(obj,thetaHat,i))
    })
    gainAll = sapply(unanswered, function(i){
      return(abs(seHat - (expectedPV(obj,i))^.5))
    })
    
    
    ## check stop rules
    
    ## lengthThreshold
    if(!is.na(obj@lengthThreshold)){
      if (numAnswered>=obj@lengthThreshold){
        stopNow = TRUE
      }
    }
    ## seThreshold
    if(!is.na(obj@seThreshold)){
      if(seHat>obj@seThreshold){
        stopNow = TRUE
      }
    }
    ## infoThreshold
    if(!is.na(obj@infoThreshold)){
      if(isTRUE(all(fishAll<obj@infoThreshold))){
        stopNow = TRUE
      }
    }
    ## gainThreshold
    if(!is.na(obj@gainThreshold)){
      if(isTRUE(all(gainAll<obj@gainThreshold))){
        stopNow = TRUE
      }
    }
    
    
    ## check override rules
    
    ## lengthOverride
    if(!is.na(obj@lengthOverride)){
      if(numAnswered<obj@lengthOverride){
        stopNow = FALSE
      }
    }
    
    ## gainOverride
    if(!is.na(obj@gainOverride)){
      if(isTRUE(all(gainAll>obj@gainThreshold))){
        stopNow = FALSE
      }
    }
    

    return (stopNow)
      
  }
  
  ### MAKE SOME RANDOM CATS
  biCats = catBiCreator(100, seed = 555)
  polyCats = catPolyCreator(100, seed = 888)
  allCats = c(biCats, polyCats)
 
  ## assign threshold/override values
  for (i in 1:length(allCats)){
    allCats[[i]]@lengthThreshold <- sample(c(round(100*runif(1)),NA,NA),size = 1)
    allCats[[i]]@seThreshold <- sample(c(2*runif(1),NA,NA),size = 1)
    allCats[[i]]@infoThreshold <- sample(c(2*runif(1),NA,NA),size = 1)
    allCats[[i]]@gainThreshold <- sample(c(2*runif(1),NA,NA),size = 1)
    allCats[[i]]@lengthOverride <- sample(c(round(10*runif(1)),NA,NA),size = 1)
    allCats[[i]]@gainOverride <- sample(c(2*runif(1)+1,NA,NA),size = 1)
  }
  
  
  ### COMPARE THE TEST FUNCTION TO THE REAL FUNCTION
  realFunValues<-sapply(1:length(allCats), function(i){ return (checkRules(allCats[[i]]))})
  testFunValues<-sapply(1:length(allCats), function(i){ return (checkRules_test_fun(allCats[[i]]))})
  expect_equal(realFunValues, testFunValues)
  
})


