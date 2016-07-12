library(catSurv)
library(testthat)
library(ltm)
context("estimateTheta")

test_that("estimateTheta calculates correctly", {
  
  # estimateTheta_test_MAP <- function(data, poly){
  # 
  #   if(poly == FALSE){
  #     cat_ltm <- ltm(data ~ z1, control = list(GHk = 100))
  #     factor_scores <- factor.scores.ltm(cat_ltm, method = "EB", prior = TRUE)$score.dat
  #     cat_ltm_theta <- factor_scores[ , "z1"]
  #     cat_ltm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
  #     
  #     cat_Cat <- ltmCat(data)
  #     cat_Cat@estimation <- "MAP"
  #     
  #     cat_Cat_theta <- rep(NA, length(cat_ltm_theta))
  #     for(i in 1:length(cat_Cat_theta)){
  #       cat_Cat@answers <- c(as.numeric(cat_ltm_data[i, ]))
  #       cat_Cat_theta[i] <- estimateTheta(cat_Cat)
  #     }
  #     differences <- abs(cat_Cat_theta - cat_ltm_theta)
  #   }
  #   
  #   if(poly == TRUE){
  #     cat_grm <- grm(data, control=list(GHk = 100))
  #     factor_scores <- factor.scores.grm(cat_grm, method = "EB")$score.dat
  #     cat_grm_theta <- factor_scores[ , "z1"]
  #     cat_grm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
  #     
  #     cat_Cat <- grmCat(data)
  #     cat_Cat@estimation <- "MAP"
  #     cat_Cat@poly <- TRUE
  #     
  #     cat_Cat_theta <- rep(NA, length(cat_grm_theta))
  #     for(i in 1:length(cat_Cat_theta)){
  #       cat_Cat@answers <- c(as.numeric(cat_grm_data[i, ]))
  #       cat_Cat_theta[i] <- estimateTheta(cat_Cat)
  #     }
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
  
  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  
  ltm.scores <- factor.scores.ltm(binary_ltm.ltm, method = "EB", prior = TRUE)$score.dat

  binary_cat.ltm <- ltmCat(binary_ltm.ltm)
  binary_cat.ltm@estimation <- "MAP"
  
  for(j in 1:dim(ltm.scores)[1]){
    binary_cat.ltm@answers <- as.numeric(ltm.scores[j,1:(dim(ltm.scores)[2]-4)])
    expect_equal(estimateTheta(binary_cat.ltm),
                 ltm.scores[j,"z1"],
                 tolerance = .0001)
  }

  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  
  tpm.scores <- factor.scores.tpm(binary_ltm.tpm, method = "EB", prior = TRUE)$score.dat
  
  binary_cat.tpm <- tpmCat(binary_ltm.tpm)
  binary_cat.tpm@estimation <- "MAP"
  
  for(j in 1:dim(tpm.scores)[1]){
    binary_cat.tpm@answers <- as.numeric(tpm.scores[j,1:(dim(tpm.scores)[2]-4)])
    expect_equal(estimateTheta(binary_cat.tpm),
                 tpm.scores[j,"z1"],
                 tolerance = .0001)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  
  grm.scores <- factor.scores.grm(poly_ltm, method = "EB", prior = TRUE)$score.dat
  
  poly_cat <- grmCat(poly_ltm, quadraturePoints = 100)
  poly_cat@estimation <- "MAP"
  
  for(j in 1:dim(grm.scores)[1]){
    poly_cat@answers <- as.numeric(grm.scores[j,1:(dim(grm.scores)[2]-4)])
    expect_equal(estimateTheta(poly_cat),
                 grm.scores[j,"z1"],
                 tolerance = .0001)
    # test fails at 4 decimal places
  }
  
  detach(package:ltm)
  
  }
)


