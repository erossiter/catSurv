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
  
  data("npi")
  data("nfc")
  data("AMTknowledge")
  ltm_data <- npi[1:100, ]
  tpm_data <- AMTknowledge[1:100, ]
  poly_data <- nfc[1:100, ]
  
  ### Test Cat estimateTheta against equation in R
  
  # binary (ltm)
  binary_cat.ltm <- ltmCat(ltm_data, quadraturePoints = 100)
  
  for(i in 1:100){
    binary_cat.ltm@answers <- as.numeric(ltm_data[i,])
    expect_equal(estimateTheta(binary_cat.ltm),
                 estimateTheta_test(binary_cat.ltm),
                 tolerance = .0001)
    #test fails at 4th decimal place
  }
  
  # binary (tpm)
  binary_cat.tpm <- tpmCat(tpm_data, quadraturePoints = 100)
  
  for(i in 1:100){
    binary_cat.tpm@answers <- as.numeric(tpm_data[i,])
    expect_equal(estimateTheta(binary_cat.tpm),
                 estimateTheta_test(binary_cat.tpm),
                 tolerance = .0001)
    #test fails at 4th decimal place
  }
  
  # poly (grm)
  poly_cat <- grmCat(poly_data, quadraturePoints = 100)  
  
  for(i in 1:100){
    poly_cat@answers <- as.numeric(poly_data[i,])
    expect_equal(estimateTheta(poly_cat),
                 estimateTheta_test(poly_cat),
                 tolerance = .0001)
    #test fails at 4th decimal place
  }
  
  ### Test Cat estimateTheta against ltm package
  
  library(ltm)
  
  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  
  ltm.scores <- factor.scores.ltm(binary_ltm.ltm, method = "EAP")$score.dat
  
  for(j in 1:dim(ltm.scores)[1]){
    binary_cat.ltm@answers <- as.numeric(ltm.scores[j,1:(dim(ltm.scores)[2]-4)])
    expect_equal(estimateTheta(binary_cat.ltm),
                 ltm.scores[j,"z1"],
                 tolerance = .0001)
    #test fails at 4th decimal place
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  
  tpm.scores <- factor.scores.tpm(binary_ltm.tpm, method = "EAP")$score.dat
# tpm.scores <- factor.scores.tpm.correct(binary_ltm.tpm, method = "EAP")$score.dat
  
  for(j in 1:dim(tpm.scores)[1]){
    binary_cat.tpm@answers <- as.numeric(tpm.scores[j,1:(dim(tpm.scores)[2]-4)])
    expect_equal(estimateTheta(binary_cat.tpm),
                 tpm.scores[j,"z1"],
                 tolerance = .0001)
    #test fails at 1st decimal place. This is probably a result of the error in factor.scores.tpm
    #the corrected version fails at 4 decimal places
  }
  
  # poly (grm)
  
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  
  grm.scores <- factor.scores.grm(poly_ltm, method = "EAP")$score.dat
  
  for(j in 1:dim(grm.scores)[1]){
    poly_cat@answers <- as.numeric(grm.scores[j,1:(dim(grm.scores)[2]-4)])
    expect_equal(estimateTheta(poly_cat),
                 grm.scores[j,"z1"],
                 tolerance = .0001)
  }
  
  detach(package:ltm)
    
})
