library(catSurv)
context("Likelihood")


test_that("binary likelihood calculates correctly",{
  
  ## creating new Cat object and filling in the slots
  
  catBi_test <- new("Cat")
  catBi_test@discrimination <- c(2,4,6,8)
  catBi_test@difficulty <- c(3,5,-12.27,9)
  catBi_test@guessing <- c(.5, .1, .32, .999)
  
  ## R test function
  
  likelihood_test <- function(catBi = "Cat", theta = "numeric", items = "numeric"){
    ## vector of probabilities for each question item
    p_iVec<-sapply(items, function(x){
      probability(catBi, theta, x)
    })
    ## storing respondent's answers to these question items
    ansVec<-catBi@answers[items]
    
    ## creating a vector of values inside the product function in equation (3) [from the documentation]
    piqiVec<-sapply(1:length(p_iVec), function(i){
      (p_iVec[i]^ansVec[i])*((1-p_iVec[i])^(1-ansVec[i]))
    })
    
    ## applying product function over this vector
    likelihood<-sapply(piqiVec, prod) 
    
    return(likelihood)
  }
  
  ## NOTE: the documentation says the likelihood functions takes an argument "items", a vector of 
  ##  the indices of the question items whose answers we want to consider...
  ## ...the version of the function in main.cpp does not take in an "items" argument, but refers to its 
  ## "applicable rows" slot instead. I don't know how to resolve this. 
  expect_equal(likelihood(catBi_test, t=1), likelihood_test(catBi_test, 1, 1))
  expect_equal(likelihood(catBi_test, t=1872), likelihood_test(catBi_test, 1872, 2))
  expect_equal(likelihood(catBi_test, t=.001), likelihood_test(catBi_test, .001, 3))
  expect_equal(likelihood(catBi_test, t=-90), likelihood_test(catBi_test, -90, 4))
})


test_that("polytomous likelihood calculates correctly",{
  
  ## creating new Cat object and filling in the slots
  
  catPoly_test <- new("Cat")
  catPoly_test@discrimination <- c(2,4,6,8)
  catPoly_test@difficulty <- list(q1=c(1,2,3,4), q2=c(-90.2, -87, -.003), q3=c(seq(-10, 10, .1)), q4=2)
  
  
  ## R test function
  
  likelihood_test <- function(catPoly = "Cat", theta = "numeric", items = "numeric"){
    ## each element in p_ikList is a vector corresponding to a question item
    ##    (the output of the polytomous probability function for each question item)
    ## each vector will be of length k_i, where k_i is the number of possible response
    ##    categories to question i, and each value in the vector is the probability of
    ##    the respondent giving a response in a category strictly higher than k
    p_ikList<-lapply(items, function(i){
      probability(catPoly, theta, i)
    })
    
    ## now, need to convert each value in each vector...
    ##  ...from "the probability of a response in a category strictly higher than k"...
    ##  ...to, "the probability of a response in exactly category k
    
    p_ikListExact<-lapply(p_ikList, function(i){ ## iterating over each question item...
      
      })
    })
    
    ansVec<-catPoly@answers[items]
    
    #piqiVec<-sapply(1:length(p_iVec), function(i){
    #  (p_iVec[i]^ansVec[i])*((1-p_iVec[i])^(1-ansVec[i]))
    #})
    
    #likelihood<-sapply(piqiVec, prod) 
    
    return(likelihood)
  }
  expect_equal(likelihood(catPoly_test, t=1, q=1), likelihood_test(catPoly_test, 1, 1))
  expect_equal(likelihood(catPoly_test, t=1872, q=2), likelihood_test(catPoly_test, 1872, 2))
  expect_equal(likelihood(catPoly_test, t=.001, q=3), likelihood_test(catPoly_test, .001, 3))
  expect_equal(likelihood(catPoly_test, t=-90, q=4), likelihood_test(catPoly_test, -90, 4))
})