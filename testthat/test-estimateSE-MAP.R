library(catSurv)
library(testthat)
library(ltm)
library(catR)
library(stats)
context("estimateSE")

test_that("estimateSE calculates correctly for MAP", {

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
  binary_cat.ltm@estimation <- "MAP"
  
  it.ltm <- matrix(c(binary_cat.ltm@discrimination, coef(binary_ltm.ltm)[,1], 
                     rep(0,length(binary_cat.ltm@discrimination)), 
                     rep(1, length(binary_cat.ltm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:nrow(ltm_data)){
    binary_cat.ltm@answers <- as.numeric(ltm_data[j,])
    theta.ltm <- estimateTheta(binary_cat.ltm)
    CatSE <- estimateSE(binary_cat.ltm)
    RSE <- semTheta(thEst = theta.ltm, it = it.ltm, x = ltm_data[j,], method = "BM", 
                    priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100))
    expect_equal(abs(CatSE - RSE)/CatSE, 0, tolerance = .001)
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  binary_cat.tpm <- tpmCat(binary_ltm.tpm, 100)
  binary_cat.tpm@estimation <- "MAP"
  
  it.tpm <- matrix(c(binary_cat.tpm@discrimination, coef(binary_ltm.tpm)[,2], 
                     binary_cat.tpm@guessing, 
                     rep(1, length(binary_cat.tpm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:nrow(tpm_data)){
    binary_cat.tpm@answers <- as.numeric(tpm_data[j,])
    theta.tpm <- estimateTheta(binary_cat.tpm)
    CatSE <- estimateSE(binary_cat.tpm)
    RSE <- semTheta(thEst = theta.tpm, it = it.tpm, x = tpm_data[j,], method = "BM", 
                    priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100))
    expect_equal(abs(CatSE - RSE)/CatSE, 0, tolerance = .001)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  poly_cat@estimation <- "MAP"
  
  it.poly <- matrix(c(coef(poly_ltm)[,5], coef(poly_ltm)[,1:4]),
                    ncol= 5, byrow = F)
  
  diff <- matrix(NA, nrow = 100, ncol = 4)
  for(j in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[j,])

    theta.poly <- estimateTheta(poly_cat)
    CatSE <-estimateSE(poly_cat)
    RSE <- semTheta(thEst = theta.poly, it = it.poly, x = as.numeric(poly_data[j,] - 1), 
                    model = "GRM", method = "BM", priorDist = "norm", priorPar = c(0,1), 
                    parInt = c(-10, 10, 100))
    expect_equal(abs(CatSE - RSE)/CatSE, 0, tolerance = .001)
  }
  
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
  # binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  # 
  # ltm.scores <- factor.scores.ltm(binary_ltm.ltm, method = "EB", prior = TRUE)$score.dat
  # 
  # binary_cat.ltm <- ltmCat(binary_ltm.ltm)
  # binary_cat.ltm@estimation <- "MAP"
  # 
  # for(j in 1:dim(ltm.scores)[1]){
  #   binary_cat.ltm@answers <- as.numeric(ltm.scores[j,1:(dim(ltm.scores)[2]-4)])
  #   expect_equal(estimateSE(binary_cat.ltm),
  #                ltm.scores[j,"se.z1"],
  #                tolerance = .0001)
  #   # test fails at 3 decimal places
  # }
  
  # binary (tpm)
  # binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  # 
  # tpm.scores <- factor.scores.tpm(binary_ltm.tpm, method = "EB", prior = TRUE)$score.dat
  # 
  # binary_cat.tpm <- tpmCat(binary_ltm.tpm)
  # binary_cat.tpm@estimation <- "MAP"  
  # 
  # for(j in 1:dim(tpm.scores)[1]){
  #   binary_cat.tpm@answers <- as.numeric(tpm.scores[j,1:(dim(tpm.scores)[2]-4)])
  #   expect_equal(estimateSE(binary_cat.tpm),
  #                tpm.scores[j,"se.z1"],
  #                tolerance = .0001)
  #   # test fails at 2 decimal places
  # }
  
  # poly (grm)
  # poly_ltm <- grm(poly_data, control = list(GHk = 100))
  # 
  # grm.scores <- factor.scores.grm(poly_ltm, method = "EB", prior = TRUE)$score.dat
  # 
  # poly_cat <- grmCat(poly_ltm, quadraturePoints = 100)
  # poly_cat@estimation <- "MAP"  
  # 
  # for(j in 1:dim(grm.scores)[1]){
  #   poly_cat@answers <- as.numeric(grm.scores[j,1:(dim(grm.scores)[2]-4)])
  #   expect_equal(estimateSE(poly_cat),
  #                grm.scores[j,"se.z1"],
  #                tolerance = .0001)
  #   # test fails at 3 decimal places
  # }
  
})

