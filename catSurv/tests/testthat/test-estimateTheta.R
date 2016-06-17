library(catSurv)
library(testthat)
context("estimateTheta")

test_that("estimateTheta calculates correctly", {
  
  estimateTheta_test <- function(cat){
    library(stats)
    numerator <- function(theta){
      prior_values <- prior(theta, cat@priorName, cat@priorParams)
      return(theta * likelihood(cat, theta) * prior_values)
      }
    denominator <- function(theta){
      prior_values <- prior(theta, cat@priorName, cat@priorParams)
      return(likelihood(cat, theta) * prior_values)
      }
    if(cat@estimation == "EAP"){ 
      results <- (integrate(Vectorize(numerator), -6, 6)$value)/
        (integrate(Vectorize(denominator), -6, 6)$value)
      }
    
    if(cat@estimation == "MAP"){
      theta_hat_old <- 0
      theta_hat_new <- 1
      tolerance <- .00000001
      difference <- abs(theta_hat_new - theta_hat_old)
      while(difference > tolerance){
        theta_hat_new <- theta_hat_old - (dLL(cat, theta_hat_old, TRUE)/d2LL(cat, theta_hat_old, TRUE))
        difference <- abs(theta_hat_new - theta_hat_old)
        theta_hat_old <- theta_hat_new
        }
      results <- theta_hat_new
      }
    return(results)
  }
  
  ltm_EAP <- function(data) {
  test.ltm <- ltm(data ~ z1, control = list(GHk = 100))
  ltm.scores <- factor.scores.ltm(test.ltm, method = "EAP")$score.dat
  
  Cat.ltm <- ltmCat(object = test.ltm)
  
  estimates.ltm <- rep(NA,dim(ltm.scores)[1])
  for (i in 1:dim(ltm.scores)[1]) {
    Cat <- Cat.ltm
    Cat@answers <- as.numeric(as.vector(ltm.scores[i,1:40]))
    estimates.ltm[i] <- estimateTheta(Cat)
  }
  
  comparison.ltm <- data.frame(estimates.ltm, ltm.scores[1:length(estimates.ltm),43])
  comparison.ltm$difference <- abs(comparison.ltm[,1] - comparison.ltm[,2])
  #colnames(comparison.ltm) <- c("estimateTheta", "factor.scores.ltm", "differences")
  if(any(comparison.ltm[,3] > 0.01)){
    return("Max estimate difference greater than 0.1")
  } else {
    return("Estimates ok.")
  }
  }
  
  
  
  lapply(c(catBiCreator(5), catPolyCreator(5)),
          function(x) expect_equal(estimateTheta(x), estimateTheta_test(x), tolerance = .001))
  # for(i in 1:8){
  #   print(estimateTheta(testCats[[i]]))
  #   print(estimateTheta_test(testCats[[i]]))
  #   print(round(estimateTheta(testCats[[i]]) - estimateTheta_test(testCats[[i]]), 5))
  #   print("")
  # }
    
})

## 'stats' is the package integrate() is in.
