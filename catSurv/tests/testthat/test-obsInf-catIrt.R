library(catSurv)
library(catR)
library(ltm)
library(catIrt)
context("obsInf")

test_that("obsInf calculates correctly", {
  
  obsInf_test_catIrt <- function(data, poly){
    
    if(poly == FALSE){
      cat <- ltmCat(data)
      ltm_cat <- ltm(data ~ z1, control = list(GHk = 100))
      cat_coefs <- coef(ltm_cat)

      # obsInf for one person, for all items
      cat@answers <- unlist(data[2,])
      our_obsInf <- c()
      for(i in 1:ncol(data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }

      params <- matrix(c(cat_coefs[,1],
                      cat@difficulty,
                      cat@guessing) , ncol = 3)
      their_obsInf <- OIi(th = 1, it = it, x = cat@answers, model = NULL)
      
      catIrt_obsInf <- FI(params = params, theta = 1, type = "observed", resp = unlist(binary_data[10,]))$item
      
      identical(catIrt_obsInf2, catIrt_obsInf)
      
      abs(their_obsInf - catIrt_obsInf)
      
      return(abs(our_obsInf - their_obsInf))
    }
  }

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal(obsInf_test_catIrt(binary_data, F),
               rep(0,ncol(binary_data)),
               tolerance = .9)
  
  expect_equal(obsInf_test_catIrt(poly_data, T),
               rep(0,ncol(poly_data)),
               tolerance = .9)
})