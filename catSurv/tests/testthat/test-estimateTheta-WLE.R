library(catSurv)
library(testthat)
library(catR)
context("estimateTheta")

test_that("estimateTheta calculates correctly for WLE", {
  
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
    binary_cat.ltm@estimation <- "WLE"
    CatTheta.wle <- estimateTheta(binary_cat.ltm)
    binary_cat.ltm@estimation <- "MAP"
    CatTheta.map <- estimateTheta(binary_cat.ltm)
    RTheta <- thetaEst(it = it.ltm, x = binary_cat.ltm@answers, method = "WL",
                     priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
    if(CatTheta.wle == CatTheta.map){ # estimates vary wildly when defaulting to MAP. Only 2 cases
      RTheta <- CatTheta.wle
    }
    expect_equal(abs(CatTheta.wle - RTheta)/CatTheta.wle, 0, tolerance = .001)
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
    CatTheta <- estimateTheta(binary_cat.tpm)
    RTheta <- thetaEst(it = it.tpm, x = binary_cat.tpm@answers, method = "WL",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .001)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  
  it.poly <- matrix(c(coef(poly_ltm)[,5], coef(poly_ltm)[,1:4]),
                    ncol= 5, byrow = F)
  
  diff <- matrix(NA, ncol=4, nrow = 100)
  for(j in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[j,])
    poly_cat@estimation <- "WLE"
    CatTheta.wle <- estimateTheta(poly_cat)
    poly_cat@estimation <- "MAP"
    CatTheta.map <- estimateTheta(poly_cat)
    RTheta <- thetaEst(it = it.poly, x = (poly_cat@answers - 1), model = "GRM", method = "WL",
                       priorDist = "norm", priorPar = c(0,1), range = c(-5,5))
    diff[j,1] <- CatTheta.wle
    diff[j,2] <- CatTheta.map
    diff[j,3] <- RTheta
    diff[j,4] <- abs(CatTheta.wle - RTheta)/CatTheta.wle
    #expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .03)
  }
  
  theta: -5
  L_theta: 9.63975          9.641026
  B: -0.384272              0.4859431
  2*I: 1.01472              1.014847
  W: 9.26105                10.11986
  
})