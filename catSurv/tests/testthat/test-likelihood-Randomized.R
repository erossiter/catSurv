library(catSurv)
context("Likelihood")

########## BINARY LIKELIHOOD TEST ##############

test_that("binary likelihood calculates correctly",{
  
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
  
  likelihood_test <- function(catBi = "Cat", theta = "numeric"){
    ## vector of probabilities for each question item
    p_iVec<-sapply(1:length(catBi@answers), function(x){
      probability(catBi, theta, x)
    })
    ## storing respondent's answers to these question items
    ansVec<-catBi@answers
    
    ## creating a vector of values inside the product function in equation (3) [from the documentation]
    piqiVec<-sapply(1:length(p_iVec), function(i){
      (p_iVec[i]^ansVec[i])*((1-p_iVec[i])^(1-ansVec[i]))
    })
    
    ## applying product function over this vector
    likelihood<-sapply(piqiVec, prod) 
    
    return(likelihood)
  }

  ## applying real likelihood function on my cats
  realFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood(allTheCats[[x]], thetaVec[x])
  })
  
  ## applying test likelihood function on my cats  
    
  testFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood(allTheCats[[x]], thetaVec[x])
  })

  expect_equal(realFunValues, testFunValues)

########## POLYTOMOUS LIKELIHOOD TEST ##############

test_that("polytomous likelihood calculates correctly",{
  
  ## creating new Cat object and filling in the slots
  
  catPoly_test <- new("Cat")
  catPoly_test@discrimination <- c(2,4,6,8)
  catPoly_test@difficulty <- list(q1=c(1,2,3,4), q2=c(-90.2, -87, -.003), q3=c(seq(-10, 10, .1)), q4=2)
  
  
  ## R test function
  
  likelihood_test <- function(catPoly = "Cat", theta = "numeric", items = "numeric"){
    ## each element in p_ikList is a vector (possibly of length 1) corresponding to a question item 
    ##    (the output of the binary or polytomous probability function for each question item)
    ## each vector will be of length k_i, where k_i is the number of possible response
    ##    categories to question i, and each value in the vector is the probability of
    ##    the respondent giving a response in a category strictly higher than k
    p_ikList<-lapply(items, function(i){
      probability(catPoly, theta, i)
    })
    
    ## now, need to convert each value in each vector...
    ##  ...from "the probability of a response in a category strictly higher than k"...
    ##  ...to, "the probability of a response in exactly category k
    
    p_ikListExact<-p_ikList ##copy list
    
    for (i in 1:length(p_ikList)){ ##iterating over items...
      for(k in 1:length(p_ikList[[i]])){ ##iterating over response categories
        if(k==1){ ## p_ikListExact[[i]][k] = p_ikList[[i]][k-1] - p_ikList[[i]][k]...
                  ## ...but p_ikList[[i]][0] = 1, as no responses are in category k=0 
                  ##  (see note in 3.1.2, between equations (4) and (5))
          p_ikListExact[[i]][k]<-1-p_ikList[[i]][k]
        }
        else p_ikListExact[[i]][k]<-p_ikList[[i]][k-1]-p_ikList[[i]][k]
      }
    }
    
    ##storing answers for question items of interest
    ansVec<-catPoly@answers[items]
    
    ## creating a list of vectors of (P_ijk)^I(y_ij = k) values... 
    ## ... each element of the list is a vector, corresponding to question item i, of length k_i
    ## ... and each element of each (length k) vector is the value of (P_ijk)^I(y_ij = k)
    probExpList<-p_ikListExact
    for(i in 1:length(probExpList)){
      thisItemValues<-c()
      for(k in 1:length(i)){
        hold<-(p_ikListExact[[i]][k])^(ansVec[i]==k)
        thisItemValues<-c(thisItemValues,hold)
      }
    }
    
    ##multiplying over response categories
    productList<-lapply(probExpList, prod)
   ##multiplying over question items
    likelihood<-prod(productList)
    return(likelihood)
  }
  expect_equal(likelihood(catPoly_test, t=1, q=1), likelihood_test(catPoly_test, 1, 1))
  expect_equal(likelihood(catPoly_test, t=1872, q=2), likelihood_test(catPoly_test, 1872, 2))
  expect_equal(likelihood(catPoly_test, t=.001, q=3), likelihood_test(catPoly_test, .001, 3))
  expect_equal(likelihood(catPoly_test, t=-90, q=4), likelihood_test(catPoly_test, -90, 4))
})