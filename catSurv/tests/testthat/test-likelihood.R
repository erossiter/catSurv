library(catSurv)
context("Likelihood")

test_that("binary likelihood calculates correctly",{
  
  ## creating new Cat object and filling in the slots
  
  catBi <- new("Cat")
  catBi@discrimination <- c(2,4,6,8)
  catBi@difficulty <- c(3,5,-12.27,9)
  catBi@guessing <- c(.5, .1, .32, .999)
  
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
  expect_equal(likelihood(catBi, t=1), likelihood_test(catBi, 1, 1))
  expect_equal(likelihood(catBi, t=1872), likelihood_test(catBi, 1872, 2))
  expect_equal(likelihood(catBi, t=.001), likelihood_test(catBi, .001, 3))
  expect_equal(likelihood(catBi, t=-90), likelihood_test(catBi, -90, 4))
})


test_that("polytomous likelihood calculates correctly",{
  
  ## creating new Cat object and filling in the slots
  
  catPoly <- new("Cat")
  catPoly@discrimination <- c(2,4,6,8)
  catPoly@difficulty <- list(q1=c(1,2,3,4), q2=c(-90.2, -87, -.003), q3=c(seq(-10, 10, .1)), q4=2)
  
  
  ## R test function
  
  likelihood_test <- function(catBi = "Cat", theta = "numeric", items = "numeric"){
    p_iVec<-sapply(items, function(x){
      probability(catBi, theta, x)
    })
    ansVec<-catBi@answers[items]
    
    piqiVec<-sapply(1:length(p_iVec), function(i){
      (p_iVec[i]^ansVec[i])*((1-p_iVec[i])^(1-ansVec[i]))
    })
    
    likelihood<-sapply(piqiVec, prod) 
    return(likelihood)
  }
  expect_equal(likelihood(catBi, t=1, q=1), likelihood_test(catBi, 1, 1))
  expect_equal(likelihood(catBi, t=1872, q=2), likelihood_test(catBi, 1872, 2))
  expect_equal(likelihood(catBi, t=.001, q=3), likelihood_test(catBi, .001, 3))
  expect_equal(likelihood(catBi, t=-90, q=4), likelihood_test(catBi, -90, 4))
})