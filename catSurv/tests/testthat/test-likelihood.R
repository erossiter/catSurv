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