library(catSurv)
library(catR)
library(ltm)
context("obsInf")

test_that("obsInf calculates correctly", {
  
  obsInf_test_CatR <- function(data, poly){
    
    if(poly == FALSE){
      cat <- ltmCat(data)
      ltm_cat <- ltm(data ~ z1, control = list(GHk = 100))
      cat_coefs <- coef(ltm_cat)

      # obsInf for one person, for all items
      cat@answers <- unlist(data[1,])
      our_obsInf <- c()
      for(i in 1:ncol(data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }

      it <- matrix(c(cat_coefs[,1],
                     cat@difficulty,
                     cat@guessing,
                     rep(1, length(cat@guessing))), ncol = 4)
      their_obsInf <- OIi(th = 1, it = it, x = cat@answers-1, model = NULL)
      
      print(summary(abs(our_obsInf - their_obsInf)))
      return(abs(our_obsInf - their_obsInf))
    }
    
    if(poly == TRUE){
      cat <- grmCat(data)
      cat_coefs <- coef(grm(data))
      
      it <- matrix(c(as.numeric(cat@discrimination),
                 cat_coefs[,1],
                 cat_coefs[,2],
                 cat_coefs[,3],
                 cat_coefs[,4]),
                 ncol = 5)
      #trial_data <- genPattern(1, it, "GRM")
      trial_data <- unlist(data[1,])
      their_obsInf <- OIi(th = 1, it = it, x = trial_data-1, model = "GRM")
      
      our_obsInf <- c()
      cat@answers <- trial_data

      for(i in 1:ncol(data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }
              
      print(summary(abs(our_obsInf - their_obsInf)))
      return(abs(our_obsInf - their_obsInf))
    }
  }

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal(obsInf_test_CatR(binary_data, F),
               rep(0,ncol(binary_data)),
               tolerance = .9)
  
  expect_equal(obsInf_test_CatR(poly_data, T),
               rep(0,ncol(poly_data)),
               tolerance = .9)
})