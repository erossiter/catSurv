library(catSurv)
library(testthat)
library(ltm)
context("estimateSE")


test_that("our estimateSE function is consistent with ltm package"){
  estimateSE_test_vs_ltm<-function(data, poly){
    if(poly==F){
      ### ltm package:
      data = binary_data
      cat_ltm<-ltm(data~z1, control = list(GHk = 100))  ## run ltm's ltm method on the data
      factor_scores <- factor.scores.ltm(cat_ltm, method = "EB", prior = T)$score.dat ## store all of the output
      ## note: ltm package's 'method = "EB" ' corresponds to our 'estimation = 'MAP' "
      #cat_ltm_theta <- factor_scores[ , "z1"] ## extract the theta estimates
      cat_ltm_SE <- factor_scores[ , "se.z1"] ## extrat the SE estimates
      cat_ltm_data<-factor_scores[ , 1:(ncol(factor_scores)-4)] ## extract the answers
      
      ### catSurv package:
      cat_Cat <- ltmCat(data) ## run our ltm method on the data
      cat_Cat@estimation <- "MAP" 
      
      #cat_Cat_theta<-rep(NA, length(cat_ltm_theta)) ## empty vector of theta values
      cat_Cat_SE<-rep(NA, length(cat_ltm_SE)) ## empty vector of SE values
      
      for (i in 1:length(cat_ltm_SE)){ ## iterating over each respondent
        cat_Cat@answers <- c(as.numeric(cat_ltm_data[i, ])) ## extract respondent i's answers from cat_ltm_data
        #cat_Cat_theta[i]<-estimateTheta(cat_Cat) ## estimate respondent i's theta, and store it in the theta vector
        cat_Cat_SE[i]<-estimateSE(cat_Cat) ## estimate respondent i's SE, and store it in the SE vector
      }
      differences<-abs(cat_Cat_SE - cat_ltm_SE) ## store difference in SE values
      summary(differences)
      
      print(summary(differences))
      print ("cat_Cat_SE:")
      print(summary(cat_Cat_SE))
      print( head(cat_Cat_SE, n=20))
      print ("cat_ltm_SE: ")
      print(summary(cat_ltm_SE))
      print(head(cat_ltm_SE, n = 20))
      #return(differences)
    }
    if (poly == T){
      ### ltm package
      data = poly_data
      cat_grm <- grm(data, control = list(GHk = 100))
      factor_scores <- factor.scores.grm(cat_grm, method = "EB")$score.dat
      #cat_grm_theta <- factor_scores[ , "z1"]
      cat_grm_SE <- factor_scores[ , "se.z1"]
      cat_grm_data<-factor_scores[,1:(ncol(factor_scores)-4)]
      
      ### catSurv package
      cat_Cat<-grmCat(data)
      cat_Cat@estimation <- "MAP"
      cat_Cat@poly <- T
      #cat_Cat_theta<-rep(NA, length(cat_grm_theta))
      cat_Cat_se<-rep(NA, length(cat_grm_SE))
      
      for(i in 1:length(cat_grm_SE)){
        cat_Cat@answers<-c(as.numeric(cat_grm_data[i,]))
        #cat_Cat_theta[i]<-estimateTheta(cat_Cat)
        cat_Cat_SE[i]<-estimateSE(cat_Cat)
      }
      differences <- abs(cat_Cat_SE - cat_grm_SE)
      print(summary(differences))
      print ("cat_Cat_SE:")
      print(summary(cat_Cat_SE))
      print( head(cat_Cat_SE, n=20))
      print ("cat_ltm_SE: ")
      print(summary(cat_ltm_SE))
      print(head(cat_ltm_SE, n = 20))
      #return(differences)
      
    }
    
  }
  
  data("npi")
  data("nfc")
  binary_data<-npi[c(8000:8020),]
  poly_data<-nfc[c(760:780),]
  
  expect_equal(estimateSE_test_vs_ltm(binary_data, F), rep(0, nrow(binary_data)), tolerance = 0.001)
  estimateSE_test_vs_ltm(poly_data, T)
}

cat_grmX <- grm(poly_data)
