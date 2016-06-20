library(catSurv)
library(testthat)
library(ltm)
context("estimateTheta")

test_that("estimateTheta calculates correctly", {
  
  estimateTheta_test_MLE <- function(data, poly){

    if(poly == FALSE){
      cat_ltm <- ltm(binary_data ~ z1, control = list(GHk = 100))
      factor_scores <- factor.scores.ltm(cat_ltm, method = "EB", prior = FALSE)$score.dat
      cat_ltm_theta <- factor_scores[ , "z1"]
      cat_ltm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
      
      cat_Cat <- ltmCat(binary_data)
      cat_Cat@estimation <- "MLE"
      
      ## if MLE was not the method used because of certain contingencies
      ## we need to know which rows so we can throw them out of the
      ## expect_equal() test and maybe they will have their own test
      max_response <- 1
      min_response <- 0
      
      cat_Cat_theta <- rep(NA, length(cat_ltm_theta))
      for(i in 1:length(cat_Cat_theta)){
        cat_Cat@answers <- c(as.numeric(cat_ltm_data[i, ]))
    
        minAnswer_negDiscrim <- maxAnswer_negDiscrim <- minAnswer_posDiscrim <- maxAnswer_posDiscrim <- ans_not_extreme <- c()
        for(i in 1:length(cat_Cat@answers)){
          if(cat_Cat@discrimination[i] < 0 & cat_Cat@answers[i] == min_response) append(minAnswer_negDiscrim, i)
          else if(cat_Cat@discrimination[i] < 0 & cat_Cat@answers[i] == max_response) append(maxAnswer_negDiscrim, i)
          else if(cat_Cat@discrimination[i] > 0 & cat_Cat@answers[i] == min_response) append(minAnswer_posDiscrim, i)
          else if(cat_Cat@discrimination[i] > 0 & cat_Cat@answers[i] == max_response) append(maxAnswer_posDiscrim, i)
          else ans_not_extreme <- append(ans_not_extreme, i)
        }
      
      	  if(length(minAnswer_posDiscrim) != 0 &
      	    length(maxAnswer_negDiscrim) != 0 &
      	    length(minAnswer_negDiscrim) == 0 &
      	    length(maxAnswer_posDiscrim) == 0 &
      	    length(ans_not_extreme) == 0){
      	    all_extreme = TRUE
      	  } else if(length(minAnswer_posDiscrim) == 0 &
      	            length(maxAnswer_negDiscrim) == 0 &
      	            length(minAnswer_negDiscrim) != 0 &
      	            length(maxAnswer_posDiscrim) != 0 &
      	            length(ans_not_extreme) == 0){
      	    all_extreme = TRUE
      	  } else {
      	    all_extreme = FALSE
      	  }
      
        if(all_extreme){
          cat_Cat_theta[i] <- NA
        } else {
          cat_Cat_theta[i] <- estimateTheta(cat_Cat)
        }
      }
      differences <- abs(cat_Cat_theta - cat_ltm_theta)
    }
    
    
    
    
    
    
    if(poly == TRUE){
      cat_grm <- grm(data, control=list(GHk = 100))
      factor_scores <- factor.scores.grm(cat_grm, method = "EB", prior = FALSE)$score.dat
      cat_grm_theta <- factor_scores[ , "z1"]
      cat_grm_data <- factor_scores[ ,1:(ncol(factor_scores)-4)]
      
      cat_Cat <- grmCat(data)
      cat_Cat@estimation <- "MLE"
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
  
  expect_equal(estimateTheta_test_MLE(binary_data, FALSE),
               rep(0, nrow(binary_data)),
               tolerance = .0001)
  
  expect_equal(estimateTheta_test_MLE(poly_data, TRUE),
               rep(0, nrow(poly_data)),
               tolerance = .0001)
  }
)


