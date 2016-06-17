library(catSurv)
library(testthat)
library(ltm)
context("estimateTheta")

test_that("estimateTheta calculates correctly", {
  
  estimateTheta_test_MAP <- function(data, poly){

    if(poly == FALSE){
      cat_ltm <- ltm(data ~ z1, control = list(GHk = 100))
      factor_scores <- factor.scores.ltm(cat_ltm, method = "EB", prior = TRUE)$score.dat
      cat_ltm_theta <- factor_scores[ , "z1"]
      cat_ltm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
      
      cat_Cat <- ltmCat(data)
      cat_Cat@estimation <- "MAP"
      
      cat_Cat_theta <- rep(NA, length(cat_ltm_theta))
      for(i in 1:length(cat_Cat_theta)){
        cat_Cat@answers <- c(as.numeric(cat_ltm_data[i, ]))
        cat_Cat_theta[i] <- estimateTheta(cat_Cat)
      }
      differences <- abs(cat_Cat_theta - cat_ltm_theta)
    }
    
    if(poly == TRUE){
      cat_grm <- grm(data, control=list(GHk = 100))
      factor_scores <- factor.scores.grm(cat_grm, method = "EB")$score.dat
      cat_grm_theta <- factor_scores[ , "z1"]
      cat_grm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
      
      cat_Cat <- grmCat(data)
      cat_Cat@estimation <- "MAP"
      cat_Cat@poly <- TRUE
      
      cat_Cat_theta <- rep(NA, length(cat_grm_theta))
      for(i in 1:length(cat_Cat_theta)){
        cat_Cat@answers <- c(as.numeric(cat_grm_data[i, ]))
        cat_Cat_theta[i] <- estimateTheta(cat_Cat)
      }
      differences <- abs(cat_Cat_theta - cat_grm_theta)
    }
    return(differences)
  }
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal(estimateTheta_test_MAP(binary_data, FALSE),
               rep(0, nrow(binary_data)),
               tolerance = .0001)
  
  expect_equal(estimateTheta_test_MAP(poly_data, TRUE),
               rep(0, nrow(poly_data)),
               tolerance = .0001)
  }
)


