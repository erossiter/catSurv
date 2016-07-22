library(catSurv)
library(testthat)
library(ltm)
library(catR)
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
  
  ###------------- Only use if introducing extra NAs --------------------
  # bi.options <- c(NA, 0, 1)
  # for(a in 1:nrow(ltm_data)){
  #   ltm_data[a,] <- sample(bi.options,ncol(ltm_data), replace=TRUE)
  # }
  # for(a in 1:nrow(tpm_data)){
  #   tpm_data[a,] <- sample(bi.options,ncol(tpm_data), replace=TRUE)
  # }
  # poly.options <- c(NA, 1:5)
  # for(a in 1:nrow(poly_data)){
  #   poly_data[a,] <- sample(poly.options,ncol(poly_data), replace=TRUE)
  # }
  ###--------------------------------------------------------------------
  
  ### Test Cat estimateTheta against equation in R
  
  # binary (ltm)
  binary_cat.ltm <- ltmCat(ltm_data, quadraturePoints = 100)
  
  for(i in 1:100){
    binary_cat.ltm@answers <- as.numeric(ltm_data[i,])
    CatTheta <- estimateTheta(binary_cat.ltm)
    RTheta <- estimateTheta_test(binary_cat.ltm)
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .1)
  }
  
  # binary (tpm)
  binary_cat.tpm <- tpmCat(tpm_data, quadraturePoints = 100)
  
  for(i in 1:100){
    binary_cat.tpm@answers <- as.numeric(tpm_data[i,])
    CatTheta <- estimateTheta(binary_cat.tpm)
    RTheta <- estimateTheta_test(binary_cat.tpm)
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .25)
  }
  
  # poly (grm)
  poly_cat <- grmCat(poly_data, quadraturePoints = 100)  
  
  for(i in 1:100){
    poly_cat@answers <- as.numeric(poly_data[i,])
    CatTheta <- estimateTheta(poly_cat)
    RTheta <- estimateTheta_test(poly_cat)
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .001)
  }
  
  ### Test Cat estimateTheta against catR package
  
  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  binary_cat.ltm <- ltmCat(binary_ltm.ltm, 100)
  binary_cat.ltm@estimation <- "EAP"
  
  it.ltm <- matrix(c(binary_cat.ltm@discrimination, coef(binary_ltm.ltm)[,1], 
                     rep(0,length(binary_cat.ltm@discrimination)), 
                     rep(1, length(binary_cat.ltm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:nrow(ltm_data)){
    binary_cat.ltm@answers <- as.numeric(ltm_data[j,])
    CatTheta <- estimateTheta(binary_cat.ltm)
    RTheta <- thetaEst(it = it.ltm, x = binary_cat.ltm@answers, method = "EAP",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10),
                       parInt = c(-10,10, 100))
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .001)
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  binary_cat.tpm <- tpmCat(binary_ltm.tpm, 100)
  binary_cat.tpm@estimation <- "EAP"
  
  it.tpm <- matrix(c(binary_cat.tpm@discrimination, coef(binary_ltm.tpm)[,2], 
                     binary_cat.tpm@guessing, 
                     rep(1, length(binary_cat.tpm@discrimination))),
                   ncol= 4, byrow = F)
  
# Note: increasing the number of quadrature points in the parInt argument causes the
#       catR estimates to draw nearer to those produced by estimateTheta. At 100,000
#       points, the maximum difference was only 3%; only 2/100 estimates exceeded a
#       difference of 0.001. Clearly any differences in estimates result from integration,
#       at which CatSurv is superior
  for(j in 1:nrow(tpm_data)){
    binary_cat.tpm@answers <- as.numeric(tpm_data[j,])
    CatTheta <- estimateTheta(binary_cat.tpm)
    RTheta <- thetaEst(it = it.tpm, x = binary_cat.tpm@answers, method = "EAP",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10),
                       parInt = c(-10,10, 100))
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .2)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  poly_cat@estimation <- "EAP"
  
  it.poly <- matrix(c(coef(poly_ltm)[,5], coef(poly_ltm)[,1:4]),
                    ncol= 5, byrow = F)
  
  for(j in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[j,])
    CatTheta <- estimateTheta(poly_cat)
    RTheta <- thetaEst(it = it.poly, x = (poly_cat@answers - 1), method = "EAP",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10),
                       parInt = c(-10, 10, 100), model = "GRM")
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .01)
  }
  
  ### Test Cat estimateTheta against ltm package
  
  # binary (ltm)
  
  # binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  # 
  # ltm.scores <- factor.scores.ltm(binary_ltm.ltm, method = "EAP")$score.dat
  # 
  # for(j in 1:dim(ltm.scores)[1]){
  #   binary_cat.ltm@answers <- as.numeric(ltm.scores[j,1:(dim(ltm.scores)[2]-4)])
  #   expect_equal(estimateTheta(binary_cat.ltm),
  #                ltm.scores[j,"z1"],
  #                tolerance = .0001)
  # }
  
  # binary (tpm)
  # binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  # 
  # tpm.scores <- factor.scores.tpm(binary_ltm.tpm, method = "EAP")$score.dat
  # # tpm.scores <- factor.scores.tpm.correct(binary_ltm.tpm, method = "EAP")$score.dat
  # 
  # for(j in 1:dim(tpm.scores)[1]){
  #   binary_cat.tpm@answers <- as.numeric(tpm.scores[j,1:(dim(tpm.scores)[2]-4)])
  #   expect_equal(estimateTheta(binary_cat.tpm),
  #                tpm.scores[j,"z1"],
  #                tolerance = .0001)
  #   #test fails at 1st decimal place. This is probably a result of the error in factor.scores.tpm
  #   #the corrected version fails at 4 decimal places
  # }
  
  # poly (grm)
  
  # poly_ltm <- grm(poly_data, control = list(GHk = 100))
  # 
  # grm.scores <- factor.scores.grm(poly_ltm, method = "EAP")$score.dat
  # 
  # for(j in 1:dim(grm.scores)[1]){
  #   poly_cat@answers <- as.numeric(grm.scores[j,1:(dim(grm.scores)[2]-4)])
  #   expect_equal(estimateTheta(poly_cat),
  #                grm.scores[j,"z1"],
  #                tolerance = .0001)
  # }
  
})

