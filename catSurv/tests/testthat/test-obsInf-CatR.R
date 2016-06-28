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
      cat@answers <- unlist(data[9,])
      cat@answers 
      our_obsInf <- c()
      for(i in 1:ncol(data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }

      it <- matrix(c(cat@discrimination,
                     cat@difficulty,
                     cat@guessing,
                     rep(1, length(cat@guessing))), ncol = 4)
      their_obsInf <- OIi(th = 1, it = it, x = cat@answers, model = NULL)
      
      return(abs(our_obsInf - their_obsInf))
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
      
      trial_data <- unlist(poly_data[1,])
      their_obsInf <- OIi(th = 1, it = it, x = trial_data-1, model = "GRM")
    
      for(j in 1:nrow(poly_data)){
        cat@answers <- unlist(poly_data[j,])  
        print(estimateSE(cat))
      }
      
      
      
      #cat@answers <- trial_data
      
      our_obsInf <- c()
      for(i in 1:(ncol(poly_data))){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }
      
      
      print(round(abs(our_obsInf - their_obsInf), 5))
      return(abs(our_obsInf - their_obsInf))
    }
  }

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  #expec(obsInf_test_CatR(binary_data, F), rep(.0001, ncol(binary_data)))
  
  #expect_less_than(obsInf_test_CatR(poly_data, T), rep(.0001, ncol(poly_data)))
}
)