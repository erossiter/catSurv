library(catSurv)
library(testthat)
library(ltm)
library(stats)
context("estimateSE")

test_that("estimateSE calculates correctly for MAP", {
  
  # estimateSE_test <- function(cat){
  #   numerator <- function(theta){
  #     theta_hat <- estimateTheta(cat)
  #     diff <- (theta - theta_hat)^2
  #     prior_theta <- prior(theta, cat@priorName, cat@priorParams)
  #     L_theta <- likelihood(cat, theta)
  #     return(diff * prior_theta * L_theta)
  #   }
  #   denominator <- function(theta){
  #     prior_theta <- prior(theta, cat@priorName, cat@priorParams)
  #     L_theta <- likelihood(cat, theta)
  #     return(prior_theta * L_theta)
  #   }
  #   results <- (integrate(Vectorize(numerator), -6, 6)$value)/
  #     (integrate(Vectorize(denominator), -6, 6)$value)
  #   return(sqrt(results))
  # }
  # 
  data("npi")
  data("nfc")
  data("AMTknowledge")
  ltm_data <- npi[1:100, ]
  tpm_data <- AMTknowledge[1:100, ]
  poly_data <- nfc[1:100, ]
  # 
  # ## compare estimateSE with R code
  # 
  # # binary (ltm)
  # binary_cat.ltm <- ltmCat(ltm_data, quadraturePoints = 100)
  # 
  # expect_equal(estimateSE(binary_cat.ltm),
  #              estimateSE_test(binary_cat.ltm),
  #              tolerance = 0.0001)
  # 
  # # binary (tpm)  
  # binary_cat.tpm <- tpmCat(tpm_data, quadraturePoints = 100)
  # 
  # expect_equal(estimateSE(binary_cat.tpm),
  #              estimateSE_test(binary_cat.tpm),
  #              tolerance = 0.0001)
  # 
  # # poly (grm)
  # poly_cat <- grmCat(poly_data, quadraturePoints = 100)
  # 
  # expect_equal(estimateSE(poly_cat),
  #              estimateSE_test(poly_cat),
  #              threshold = 0.0001)
  
  ## compare Cat estimate SE against ltm package
  
  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  
  ltm.scores <- factor.scores.ltm(binary_ltm.ltm, method = "EB", prior = TRUE)$score.dat
  
  binary_cat.ltm <- ltmCat(binary_ltm.ltm)
  binary_cat.ltm@estimation <- "MAP"
  
  for(j in 1:dim(ltm.scores)[1]){
    binary_cat.ltm@answers <- as.numeric(ltm.scores[j,1:(dim(ltm.scores)[2]-4)])
    expect_equal(estimateSE(binary_cat.ltm),
                 ltm.scores[j,"se.z1"],
                 tolerance = .0001)
    # test fails at 3 decimal places
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  
  tpm.scores <- factor.scores.tpm(binary_ltm.tpm, method = "EB", prior = TRUE)$score.dat
  
  binary_cat.tpm <- tpmCat(binary_ltm.tpm)
  binary_cat.tpm@estimation <- "MAP"  
  
  for(j in 1:dim(tpm.scores)[1]){
    binary_cat.tpm@answers <- as.numeric(tpm.scores[j,1:(dim(tpm.scores)[2]-4)])
    expect_equal(estimateSE(binary_cat.tpm),
                 tpm.scores[j,"se.z1"],
                 tolerance = .0001)
    # test fails at 2 decimal places
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  
  grm.scores <- factor.scores.grm(poly_ltm, method = "EB", prior = TRUE)$score.dat
  
  poly_cat <- grmCat(poly_ltm, quadraturePoints = 100)
  poly_cat@estimation <- "MAP"  
  
  for(j in 1:dim(grm.scores)[1]){
    poly_cat@answers <- as.numeric(grm.scores[j,1:(dim(grm.scores)[2]-4)])
    expect_equal(estimateSE(poly_cat),
                 grm.scores[j,"se.z1"],
                 tolerance = .0001)
    # test fails at 3 decimal places
  }
  
  detach(package:ltm)
  
})

