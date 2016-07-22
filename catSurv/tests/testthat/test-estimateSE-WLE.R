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
  
  ### Test Cat estimateSE against catR package

  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  binary_cat.ltm <- ltmCat(binary_ltm.ltm, 100)
  binary_cat.ltm@estimation <- "WLE"
  
  it.ltm <- matrix(c(binary_cat.ltm@discrimination, coef(binary_ltm.ltm)[,1], 
                     rep(0,length(binary_cat.ltm@discrimination)), 
                     rep(1, length(binary_cat.ltm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:nrow(ltm_data)){
    binary_cat.ltm@answers <- as.numeric(ltm_data[j,])
    theta.ltm <- estimateTheta(binary_cat.ltm)
    CatSE <- estimateSE(binary_cat.ltm)
    RSE <- semTheta(thEst = theta.ltm, it = it.ltm, x = ltm_data[j,], method = "WL", 
                    priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100))
    expect_equal(abs(CatSE - RSE)/CatSE, 0, tolerance = .4)
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  binary_cat.tpm <- tpmCat(binary_ltm.tpm, 100)
  binary_cat.tpm@estimation <- "WLE"
  
  it.tpm <- matrix(c(binary_cat.tpm@discrimination, coef(binary_ltm.tpm)[,2], 
                     binary_cat.tpm@guessing, 
                     rep(1, length(binary_cat.tpm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:nrow(tpm_data)){
    binary_cat.tpm@answers <- as.numeric(tpm_data[j,])
    theta.tpm <- estimateTheta(binary_cat.tpm)
    CatSE <- estimateSE(binary_cat.tpm)
    RSE <- semTheta(thEst = theta.tpm, it = it.tpm, x = tpm_data[j,], method = "WL", 
                    priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100))
    expect_equal(abs(CatSE - RSE)/CatSE, 0, tolerance = .001)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  poly_cat@estimation <- "WLE"
  
  it.poly <- matrix(c(coef(poly_ltm)[,5], coef(poly_ltm)[,1:4]),
                    ncol= 5, byrow = F)
  
  diff <- matrix(NA, nrow = 100, ncol =3)
  for(j in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[j,])
    theta.poly <- estimateTheta(poly_cat)
    CatSE <-estimateSE(poly_cat)
    RSE <- semTheta(thEst = theta.poly, it = it.poly, x = as.numeric(poly_data[j,] - 1), 
                    model = "GRM", method = "WL", priorDist = "norm", priorPar = c(0,1), 
                    parInt = c(-10, 10, 100))
    diff[j,1] <- CatSE
    diff[j,2] <- RSE
    diff[j,3] <- abs(CatSE - RSE)/CatSE
    #expect_equal(abs(CatSE - RSE)/CatSE, 0, tolerance = .25)
  }
  
})