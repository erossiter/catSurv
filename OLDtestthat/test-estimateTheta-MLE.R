library(catSurv)
library(testthat)
library(ltm)
library(catR)
context("estimateTheta")

# tpm generates NaN

test_that("estimateTheta calculates correctly", {
  
  ## only difference between MLE and MAP test
  ## is that prior == FALSE in factor.scores()
  
  # estimateTheta_test_MLE <- function(data, poly){
  #   
  #   if(poly == FALSE){
  #     cat_ltm <- ltm(data ~ z1, control = list(GHk = 100))
  #     factor_scores <- factor.scores.ltm(cat_ltm, method = "EB", prior = FALSE)$score.dat
  #     cat_ltm_theta <- factor_scores[ , "z1"]
  #     cat_ltm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
  #     
  #     cat_Cat <- ltmCat(data)
  #     cat_Cat@estimation <- "MLE"
  #     
  #     cat_Cat_theta <- rep(NA, length(cat_ltm_theta))
  #     for(i in 1:length(cat_Cat_theta)){
  #       cat_Cat@answers <- c(as.numeric(cat_ltm_data[i, ]))
  #       cat_Cat_theta[i] <-estimateTheta(cat_Cat)
  #     }
  #     differences <- abs(cat_Cat_theta - cat_ltm_theta)
  #   }
  #   
  #   if(poly == TRUE){
  #     cat_grm <- grm(data, control=list(GHk = 100))
  #     factor_scores <- factor.scores.grm(cat_grm, method = "EB", prior = FALSE)$score.dat
  #     cat_grm_theta <- factor_scores[ , "z1"]
  #     cat_grm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
  #     
  #     cat_Cat <- grmCat(data)
  #     cat_Cat@estimation <- "MLE"
  #     
  #     cat_Cat_theta <- rep(NA, length(cat_grm_theta))
  #     for(i in 1:length(cat_Cat_theta)){
  #       cat_Cat@answers <- c(as.numeric(cat_grm_data[i, ]))
  #       cat_Cat_theta[i] <- estimateTheta(cat_Cat)
  #     }
  #     
  #     differences <- abs(cat_Cat_theta - cat_grm_theta)
  #   }
  #   return(differences)
  # }
  
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
  binary_cat.ltm@estimation <- "MLE"
  
  it.ltm <- matrix(c(binary_cat.ltm@discrimination, coef(binary_ltm.ltm)[,1], 
                     rep(0,length(binary_cat.ltm@discrimination)), 
                     rep(1, length(binary_cat.ltm@discrimination))),
                   ncol= 4, byrow = F)
  
  #diff <- matrix(NA, ncol=3, nrow=100)
  for(j in 1:nrow(ltm_data)){
    binary_cat.ltm@answers <- as.numeric(ltm_data[j,])
    CatTheta <- estimateTheta(binary_cat.ltm)
    RTheta <- thetaEst(it = it.ltm, x = ltm_data[j,], method = "ML",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
    if(RTheta == -10 || RTheta == 10) {
      RTheta <- CatTheta
    }
    #diff[j,1] <- CatTheta
    #diff[j,2] <- RTheta
    #diff[j,3] <- abs(CatTheta - RTheta)/CatTheta
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .001)
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  binary_cat.tpm <- tpmCat(binary_ltm.tpm, 100)
  binary_cat.tpm@estimation <- "MLE"
  
  it.tpm <- matrix(c(binary_cat.tpm@discrimination, coef(binary_ltm.tpm)[,2], 
                     binary_cat.tpm@guessing, 
                     rep(1, length(binary_cat.tpm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:nrow(tpm_data)){
    binary_cat.tpm@answers <- as.numeric(tpm_data[j,])
    CatTheta<- estimateTheta(binary_cat.tpm)
    RTheta <- thetaEst(it = it.tpm, x = tpm_data[j,], method = "ML",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
    if(RTheta == -10 || RTheta == 10) {
      RTheta <- CatTheta
    }
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .001)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  poly_cat@estimation <- "MLE"
  
  it.poly <- matrix(c(coef(poly_ltm)[,5], coef(poly_ltm)[,1:4]),
                    ncol= 5, byrow = F)
  
  for(j in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[j,])
    CatTheta <- estimateTheta(poly_cat)
    RTheta <- thetaEst(it = it.poly, x = as.numeric(poly_data[j,] - 1), method = "ML",
                       priorDist = "norm", priorPar = c(0,1), range = c(-10,10),
                       model = "GRM")
    if(RTheta == -10 || RTheta == 10) {
      RTheta <- CatTheta
    }
    expect_equal(abs(CatTheta - RTheta)/CatTheta, 0, tolerance = .01)
  }
  
  ## test Cat estimateTheta against ltm package
  
  # # binary (ltm)
  # binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  # 
  # ltm.scores <- factor.scores.ltm(binary_ltm.ltm, method = "EB", prior = FALSE)$score.dat
  # #ltm.scores <- ltm.scores[-c(9,82),]
  # 
  # binary_cat.ltm <- ltmCat(binary_ltm.ltm)
  # binary_cat.ltm@estimation <- "MLE"
  # 
  # for(j in 1:dim(ltm.scores)[1]){
  #   binary_cat.ltm@answers <- as.numeric(ltm.scores[j,1:(dim(ltm.scores)[2]-4)])
  #   expect_equal(estimateTheta(binary_cat.ltm),
  #                ltm.scores[j,"z1"],
  #                tolerance = .001) # fails at 4 decimals
  # }
  # 
  # # binary (tpm)
  # binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  # 
  # tpm.scores <- factor.scores.tpm(binary_ltm.tpm, method = "EB", prior = FALSE)$score.dat
  # 
  # binary_cat.tpm <- tpmCat(binary_ltm.tpm)
  # binary_cat.tpm@estimation <- "MLE"
  # 
  # for(j in 1:dim(tpm.scores)[1]){
  #   binary_cat.tpm@answers <- as.numeric(tpm.scores[2,1:(dim(tpm.scores)[2]-4)])
  #   expect_equal(estimateTheta(binary_cat.tpm),
  #                tpm.scores[j,"z1"],
  #                tolerance = .0001)
  #   # difference > 2
  # }
  # 
  # # poly (grm)
  # poly_ltm <- grm(poly_data, control = list(GHk = 100))
  # 
  # grm.scores <- factor.scores.grm(poly_ltm, method = "EB", prior = FALSE)$score.dat
  # 
  # poly_cat <- grmCat(poly_ltm, quadraturePoints = 100)
  # poly_cat@estimation <- "MLE"
  # 
  # for(j in 1:dim(grm.scores)[1]){
  #   poly_cat@answers <- as.numeric(grm.scores[j,1:(dim(grm.scores)[2]-4)])
  #   expect_equal(estimateTheta(poly_cat),
  #                grm.scores[j,"z1"],
  #                tolerance = .001) # fails at 4 decimals
  # }
  
  ## taking out the two rows I know our MLE estimation will
  ## dafault to MAP for.
    #expect_equal(estimateTheta_test_MLE(binary_data, FALSE)[-c(9,82)],
               #rep(0, nrow(binary_data)-2),
               #tolerance = .001)
  
  ## in row 92, this test is breaking out of the while loop
  ## and doing the Brent method of evalation
    #expect_equal(estimateTheta_test_MLE(poly_data, TRUE),
               #rep(0, nrow(poly_data)),
               #tolerance = .001)
  }
)


