library(catSurv)
context("Likelihood")

########## BINARY LIKELIHOOD TEST ##############

test_that("binary likelihood calculates correctly", {
  
  ## creating new Cat object and filling in the slots
  allTheCats<-catBiCreator(10, fillAnswers=.3)
  
  ## creating vector of thetas
  setThetas<-function(seed="numeric"){
    set.seed(seed)
    thetas<-c(2*rnorm(length(allTheCats))) # drawing one theta value for each Cat (number of draws = length(allTheCats))... 
    ## ...and multiplying by 2 so the values cover a range of ~(-4,4)
  }
  thetaVec<-setThetas(1000)
  
  
  ## R test function
  
  likelihood_test_bi <- function(catBi = "Cat", theta = "numeric"){
    ## vector of probabilities for each question item that has been answered
    answered_indices<-which(!is.na(catBi@answers), arr.ind=T)
    
    if(length(answered_indices)>0){
      
      p_iVec<-sapply(answered_indices, function(i){
        probability(catBi, theta, i)$all.probabilities$probabilities
      })
      ## storing respondent's answers to these question items
      ansVec<-catBi@answers[answered_indices]
      
      ## creating a vector of values inside the product function in equation (3) [from the documentation]
      piqiVec<-sapply(1:length(p_iVec), function(i){
        (p_iVec[i]^(ansVec[i]))*((1-p_iVec[i])^(1-ansVec[i]))
      })
      ## applying product function over this vector
      likelihood<-prod(piqiVec) 
      
      return(likelihood)
    }
    else return (1)
  }
  
  ## applying real likelihood function on my cats
  realFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood(allTheCats[[x]], thetaVec[x])
  })
  
  ## applying test likelihood function on my cats  
  
  testFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood_test_bi(allTheCats[[x]], thetaVec[x])
  })
  
  expect_equal(realFunValues, testFunValues)
  
  
})

rm(list=ls())









########## POLYTOMOUS LIKELIHOOD TEST ##############

test_that("polytomous likelihood calculates correctly",{
  
  ## creating lots of new Cats  and filling in the slots
  
  allTheCats<-catPolyCreator(10, fillAnswers=0.4, seed=9576)
  #allTheCats[[1]]@guessing<-rep(0, length(allTheCats[[1]]@guessing))
  #allTheCats[[1]]@answers[2]<-3 
  #allTheCats[[1]]@difficulty
  
  ## creating vector of theta values
  setThetas<-function(seed="numeric"){
    set.seed(seed)
    thetas<-c(2*rnorm(length(allTheCats))) # drawing one theta value for each Cat (number of draws = length(allTheCats))... 
    ## ...and multiplying by 2 so the values cover a range of ~(-4,4)
  }
  thetaVec<-setThetas(18)
  
  
  ## R test function
  
  likelihood_test_poly <- function(catPoly = "Cat", theta = "numeric"){
    ## each element in p_ikList is a vector (of variable length, possibly length 1) corresponding to a question item 
    ##    (the output of the probability function for each question item)
    ## each vector will be of length k_i, where k_i is the number of possible response
    ##    categories to question i, and each value in the vector is the probability of
    ##    the respondent giving a response in a category strictly higher than k
    answered_indices<-which(!is.na(catPoly@answers), arr.ind=T)
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
      
      
      ##storing answers for question items that have been answered
      ansVec<-catPoly@answers[answered_indices]
      
      ## creating a list of vectors of (P_ijk)^I(y_ij = k) values... 
      ## ... each element of the list is a vector, corresponding to question item i, of length k_i
      ## ... and each element of each (length k_i) vector is the value of (P_ijk)^I(y_ij = k)
      probExpList<-p_ikListExact ##copying, for dimensions
      for(i in 1:length(probExpList)){
        for(k in 1:length(i)){
          probExpList[[i]][k]<-(p_ikListExact[[i]][k])^(ansVec[i]==k)
        }
      }
      
      ##multiplying over response categories
      productVec<-sapply(probExpList, prod)
      ##multiplying over question items
      likelihood<-prod(productVec)
      return(likelihood)
    }
    else return(1)
  }
  
  ### running real and test functions on allTheCats
  realFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood(allTheCats[[x]], thetaVec[x])
  })
  
  testFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood_test_poly(allTheCats[[x]], thetaVec[x])
  })
  
  expect_equal(realFunValues, testFunValues)
  
})