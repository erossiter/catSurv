library(catSurv)
library(testthat)
library(ltm)
library(stats)
context("estimateTheta")

test_that("estimateTheta calculates correctly", {
  
  estimateTheta_test_MAP <- function(data, poly){

    if(poly == FALSE){
      cat_ltm_bi <- ltm(data ~ z1, control = list(GHk = 100))
      factor_scores <- factor.scores.ltm(cat_ltm_bi, method = "EB", prior = TRUE)$score.dat
      cat_ltm_bi_theta <- factor_scores[ , "z1"]
      cat_ltm_bi_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
      
      cat_Cat_bi <- ltmCat(data)
      cat_Cat_bi@estimation <- "MAP"
      
      cat_Cat_bi_theta <- rep(NA, length(cat_ltm_bi_theta))
      for(i in 1:length(cat_Cat_bi_theta)){
        cat_Cat_bi@answers <- c(as.numeric(cat_ltm_bi_data[i, ]))
        cat_Cat_bi_theta[i] <- estimateTheta(cat_Cat_bi)
      }
    }
    
    if(poly == TRUE){
      print("no test yet")
    }
  
    return(abs(cat_Cat_bi_theta - cat_ltm_bi_theta))
  }
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal(estimateTheta_test_MAP(binary_data, FALSE),
               rep(0, nrow(binary_data)),
               tolerance = .0001)
  
  expect_equal(estimateTheta_test_MAP(poly_data, TRUE),
               rep(0, nrow(binary_data)),
               tolerance = .0001)
  }
)


