# context("checkStopRules")
# load("cat_catects.Rdata")
# 
# checkStopRules_test <- function(cat){
#   stop <- FALSE
#     
#   n_answered <- sum(!is.na(cat@answers))
#   unanswered <- which(is.na(cat@answers))
#   se_est <- estimateSE(cat)
#   theta_est <- estimateTheta(cat)
#   fish_inf <- sapply(unanswered, function(x) fisherInf(cat, theta_est, x))
#   gain_all <- sapply(unanswered, function(i) abs(seHat - (expectedPV(cat, x))^.5))
# 
#   ## lengthThreshold
#   if(!is.na(cat@lengthThreshold)){
#     if(numAnswered >= cat@lengthThreshold) stop <- TRUE
#   }
#     
#   ## seThreshold
#   if(!is.na(cat@seThreshold)){
#     if(se_est < cat@seThreshold) stop <- TRUE
#   }
# 
#   ## infoThreshold
#   if(!is.na(cat@infoThreshold)){
#     if(all(fishAll < cat@infoThreshold)) stop <- TRUE
#   }
# 
#   ## gainThreshold
#   if(!is.na(cat@gainThreshold)){
#     if(all(gainAll < cat@gainThreshold)) stop <- TRUE
#   }
#     
#   ## lengthOverride
#   if(!is.na(cat@lengthOverride)){
#     if(numAnswered<cat@lengthOverride) stop <- FALSE
#   }
#     
#   ## gainOverride
#   if(!is.na(cat@gainOverride)){
#     if(isTRUE(all(gainAll>cat@gainThreshold))) stop <- FALSE
#   }
#   
#   return (stop)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# test_that("checkStopRules functions correctly", {
#   
#   
#   ### MAKE SOME RANDOM CATS
#   biCats = catBiCreator(20, seed = 456)
#   polyCats = catPolyCreator(20, seed = 789)
#   allCats = c(biCats, polyCats)
#  
#   ## assign threshold/override values
# 
#   for (i in 1:length(allCats)){
#     allCats[[i]]@lengthThreshold <- sample(c(round(100*runif(1)),NA,NA),size = 1)
#     allCats[[i]]@seThreshold <- sample(c(2*runif(1),NA,NA),size = 1)
#     allCats[[i]]@infoThreshold <- sample(c(2*runif(1),NA,NA),size = 1)
#     allCats[[i]]@gainThreshold <- sample(c(2*runif(1),NA,NA),size = 1)
#     allCats[[i]]@lengthOverride <- sample(c(round(10*runif(1)),NA,NA),size = 1)
#     allCats[[i]]@gainOverride <- sample(c(2*runif(1)+1,NA,NA),size = 1)
#   }
#   
#   
#   ### COMPARE THE TEST FUNCTION TO THE REAL FUNCTION
#   realFunValues<-sapply(1:length(allCats), function(i){ return (checkStopRules(allCats[[i]]))})
#   testFunValues<-sapply(1:length(allCats), function(i){ return (checkRules_test_fun(allCats[[i]]))})
#   expect_equal(realFunValues, testFunValues)
#   
# })
