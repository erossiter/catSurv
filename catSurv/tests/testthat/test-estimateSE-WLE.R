library(catSurv)
library(testthat)
library(catR)
context("estimateSE")

test_that("estimateSE calculates correctly for WLE", {

  data("npi")
  data("nfc")
  data("AMTknowledge")
  ltm_data <- npi[1:100, ]
  tpm_data <- AMTknowledge[1:100, ]
  poly_data <- nfc[1:100, ]

  # binary (ltm)
  binary_cat.ltm <- ltmCat(ltm_data, 100)
  binary_cat.ltm@estimation <- "WLE"
  binary_cat.ltm@answers <- sample(0:1, length(binary_cat.ltm@answers), replace=T)
  
  it.ltm <- matrix(c(binary_cat.ltm@discrimination, binary_cat.ltm@difficulty, 
                 rep(0,length(binary_cat.ltm@discrimination)), 
                 rep(1, length(binary_cat.ltm@discrimination))),
               ncol= 4, byrow = F)
  
  theta.ltm <- thetaEst(it = it.ltm, x = binary_cat.ltm@answers, method = "WL",
                    priorDist = "norm", priorPar = c(0,1), range = c(-10,10))

  
  expect_equal(estimateSE(binary_cat.ltm),
               semTheta(thEst = theta.ltm, it = it.ltm, x = binary_cat.ltm@answers, method = "WL",
                        priorDist = "norm", priorPar = c(0,1)),
                        tolerance = .0001)
  
  # binary (tpm)
  binary_cat.tpm <- tpmCat(tpm_data, 100)
  binary_cat.tpm@estimation <- "WLE"
  binary_cat.tpm@answers <- sample(0:1, length(binary_cat.tpm@answers), replace=T)
  
  it.tpm <- matrix(c(binary_cat.tpm@discrimination, binary_cat.tpm@difficulty, 
                     binary_cat.tpm@guessing, 
                     rep(1, length(binary_cat.tpm@discrimination))),
                   ncol= 4, byrow = F)
  
  theta.tpm <- thetaEst(it = it.tpm, x = binary_cat.tpm@answers, method = "WL",
                        priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
  
  expect_equal(estimateSE(binary_cat.tpm),
               semTheta(thEst = theta.tpm, it = it.tpm, x = binary_cat.tpm@answers, method = "WL",
                        priorDist = "norm", priorPar = c(0,1)),
               tolerance = .0001)
  
  # poly (grm)
  poly_cat <- grmCat(poly_data, 100)
  poly_cat.ltm@estimation <- "WLE"
  poly_cat@answers <- sample(0:4,length(poly_cat@answers), replace=T)
  
  it.poly <- matrix(NA, ncol = 5, nrow = length(poly_cat@discrimination))
  for(i in 1:length(poly_cat@discrimination)){
    it.poly[i,1] <- poly_cat@discrimination[i]
    for(j in 1:length(poly_cat@difficulty[[i]])){
      it.poly[i,j+1] <- poly_cat@difficulty[[i]][j]
    }
  }
  
  theta.poly <- thetaEst(it = it.poly, x = poly_cat@answers, method = "WL",
                         priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
  
  expect_equal(estimateSE(poly_cat),
               semTheta(thEst = theta.poly, it = it.poly, x = poly_cat@answers, method = "WL",
                        priorDist = "norm", priorPar = c(0,1)),
               tolerance = .0001)
  
  detach(package:catR)
  
})