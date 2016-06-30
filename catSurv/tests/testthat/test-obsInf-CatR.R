library(catSurv)
library(catR)
library(ltm)
context("obsInf")

test_that("obsInf calculates correctly", {
  
  obsInf_test_CatR <- function(poly){
    
    if(poly == FALSE){
      cat <- ltmCat(binary_data)
      ltm_cat <- ltm(binary_data ~ z1, control = list(GHk = 100))
      cat_coefs <- coef(ltm_cat)
      
      # obsInf for one person, for all items
      cat@answers <- unlist(binary_data[1,])
      cat@answers 
      our_obsInf <- c()
      for(i in 1:ncol(binary_data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }

      it <- matrix(c(cat@discrimination,
                     cat_coefs[,1],
                     cat@guessing,
                     rep(1, length(cat@guessing))), ncol = 4)
      their_obsInf <- OIi(th = 1, it = it, x = cat@answers, model = NULL)
      names(their_obsInf) <- NULL
      
      return(round(abs(our_obsInf - their_obsInf), 4))
    }
    
    if(poly == TRUE){
      cat <- grmCat(poly_data)
      cat_coefs <- coef(grm(poly_data, IRT.param=TRUE, control = list(GHk = 100)))
      
      it <- matrix(c(as.numeric(cat@discrimination),
                 cat_coefs[,1],
                 cat_coefs[,2],
                 cat_coefs[,3],
                 cat_coefs[,4]),
                 ncol = 5)
      
      cat@answers <- unlist(poly_data[1,])
      their_obsInf <- OIi(th = 1, it = it, x = cat@answers-1, model = "GRM")
      
      our_obsInf <- c()
      for(i in 1:(ncol(poly_data))){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }
      
      return(round(abs(our_obsInf - their_obsInf), 3))
    }
  }

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal(obsInf_test_CatR(F), rep(0, ncol(binary_data)))
  
  expect_equal(obsInf_test_CatR(T), rep(0, ncol(poly_data)))
}
)