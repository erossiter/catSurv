library(catSurv)
context("Likelihood")

## should we keep error message below about weird proabbilities??

test_that("likelihood calculates correctly", {

  likelihood_test <- function(cat = "Cat", theta = "numeric"){
    
    # Identifies answered questions and combines them in vector
    answered_indices <- which(!is.na(cat@answers), arr.ind = T)
    ansVec <- cat@answers[answered_indices]
    
    # Create list of probability vectors for each question
    pList <- lapply(answered_indices, function(i) {
      probability(cat, theta, i)$all.probabilities$probabilities
    })
    
    if (length(answered_indices) > 0) {
      
      # For binary likelihood
      if (cat@poly == F) {
        # Creates a vector of values inside the product function in equation (3) [from the documentation]
        pqVec<-sapply(1:length(pList), function(i) {
          (pList[[i]]^(ansVec[i])) * ((1 - pList[[i]])^(1 - ansVec[i]))
        })
        # Apply product function over this vector
        print(pqVec)
        likelihood <- prod(pqVec) 
      } 
      
      #  For polytomous likelihood
      if (cat@poly == T) {
      
      pListFinal <- vector("list", length(pList))
      for (i in 1:length(pList)) {  # Iterating over question
        # Append 0 and 1 to front and back or each vector of probabilties
        pList[[i]] <- c(0, pList[[i]], 1)
        print(pList[[i]])
        for (k in 2:length(pList[[i]])) { #  iterating over response categories
          pListFinal[[i]][k - 1] <- pList[[i]][k] - pList[[i]][k - 1]
        }
      }
      
      probExpList <- pListFinal #  Copying for dimensions
      for (i in 1:length(probExpList)) {
        for (k in 1:length(probExpList[[i]])) {
          probExpList[[i]][k] <- (pListFinal[[i]][k])^(ansVec[i] == k)
        }
      }
      
      # Multiplying over response categories
      productVec <- sapply(probExpList, prod)
      if(any(productVec < 0)){
        stop("P_ijk shouldn't be negative?")
      }
      # Multiplying over question items
      likelihood <- prod(productVec)
      } 
      return(likelihood)
    } else return (1)
  }
  
  ##### Note that the following functions require data inputs before test_that can operate
  
  # Applying real likelihood function on my cats
  realFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood(allTheCats[[x]], thetaVec[x])
  })
  
  # Applying test likelihood function on my cats  
  
  testFunValues<-lapply(1:length(allTheCats), function(x){
    likelihood_test_bi(allTheCats[[x]], thetaVec[x])
  })
  
  expect_equal(realFunValues, testFunValues)
  
  
})