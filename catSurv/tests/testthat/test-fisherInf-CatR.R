library(catSurv)
library(catR)
library(ltm)
context("fisherInf")

test_that("fisherInf calculates correctly", {

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  fisherInf_test_CatR <- function(){
    ## Categorical
    cat <- grmCat(poly_data)
    cat_coefs <- coef(grm(poly_data))

    our_fisherInf <- c()
    for(i in 1:18){
      our_fisherInf <- append(our_fisherInf, fisherInf(cat,1,i))
    }
                
    it <- matrix(c(cat@discrimination,
                 cat_coefs[,1],
                 cat_coefs[,2],
                 cat_coefs[,3],
                 cat_coefs[,4]),
                 ncol = 5)
  
    their_fisherInf <- Ii(1, it, model = "GRM")$Ii
  
    return(abs(our_fisherInf - their_fisherInf))
  }

  expect_equal(fisherInf_test_CatR(),
               rep(0,length(cat@answers)),
               tolerance = .01)
})