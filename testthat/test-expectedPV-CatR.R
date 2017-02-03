library(catSurv)
library(catR)
library(ltm)
context("expectedPV")

test_that("expectedPV calculates correctly", {
  
  expectedPV_test_CatR <- function(poly){
  
    if(poly == FALSE){
        binary_cat <- ltmCat(binary_data)
        ltm_cat <- ltm(binary_data ~ z1, control = list(GHk = 100))
        cat_coefs <- coef(ltm_cat)
        
        bank <- matrix(c(binary_cat@discrimination,
                  cat_coefs[,1],
                  binary_cat@guessing,
                  rep(1, length(binary_cat@guessing))), ncol = 4)
        
        ## looping over each person.. all answered the first 5 questions
        ## calculating EPV for question 6
        binary_differences <- numeric(100)
        for(i in 1:nrow(binary_data)){
          binary_cat@answers[1:5] <- unlist(binary_data[i,1:5])
        
          theirEPV <- EPV(itemBank = bank,
                        it.given <- bank[1:5, ],
                        x = unlist(binary_data[i,1:5]),
                        item = 6,
                        theta = estimateTheta(binary_cat),
                        parInt = c(-5,5,101))
        
          ourEPV <- expectedPV(binary_cat, 6)
          
          binary_differences[i] <- abs(theirEPV - ourEPV)
        }
        
        return(round(binary_differences, 3))
    }

    if(poly == TRUE){
      poly_cat <- grmCat(poly_data)
      cat_coefs <- coef(grm(poly_data, IRT.param=TRUE, control = list(GHk = 100)))
      
      bank <- matrix(c(poly_cat@discrimination,
                 cat_coefs[,1],
                 cat_coefs[,2],
                 cat_coefs[,3],
                 cat_coefs[,4]),
                 ncol = 5)
      
      ## looping over each person.. all answered the first 5 questions
      ## calculating EPV for question 6
      poly_differences <- numeric(100)
      for(j in 1:nrow(poly_data)){
        poly_cat@answers[1:length(poly_cat@guessing)] <- rep(NA, length(poly_cat@guessing))
        poly_cat@answers[1:5] <- unlist(poly_data[j, 1:5]) 
        theirEPV <- EPV(itemBank = bank,
                        it.given <- bank[1:5, ],
                        x = unlist(poly_data[j, 1:5]) -1,
                        item = 6,
                        theta = estimateTheta(poly_cat),
                        model = "GRM",
                        parInt = c(-5,5,101))
        ourEPV <- expectedPV(poly_cat, 6)
        
        poly_differences[j] <- abs(theirEPV - ourEPV)
        }
        
        return(round(poly_differences, 3))
    }
    
  }
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]

  expect_equal(expectedPV_test_CatR(FALSE)[-49], rep(0, 99))
  expect_equal(expectedPV_test_CatR(TRUE), rep(0, 100))
})

  

