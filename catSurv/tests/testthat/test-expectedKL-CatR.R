library(catSurv)
library(catR)
library(ltm)
context("expectedKL")

test_that("expectedKL calculates correctly", {
  
  expectedKL_test_CatR <- function(poly){
  
    if(poly == FALSE){
        binary_cat <- ltmCat(binary_data)
        ltm_cat <- ltm(binary_data ~ z1, control = list(GHk = 100))
        cat_coefs <- coef(ltm_cat)
        
        bank <- matrix(c(binary_cat@discrimination,
                  cat_coefs[,1],
                  binary_cat@guessing,
                  rep(1, length(binary_cat@guessing))), ncol = 4)
        
        ## looping over each person.. all answered the first 5 questions
        ## calculating KL for question 6
        theirKL <- ourKL <- numeric(100)
        for(i in 1:nrow(binary_data)){
          binary_cat@answers[1:length(binary_cat@guessing)] <- rep(NA, length(binary_cat@guessing))
          binary_cat@answers[1:5] <- unlist(binary_data[100,1:5])
          
          ourKL[i] <- expectedKL(binary_cat, 9)
        }
          theirKL[i] <- KL(item = 9,
                           itemBank = bank,
                           it.given <- bank[1:5, ],
                           x = unlist(binary_data[100,1:5]),
                           lower = -4,
                           upper = 4,
                           type = "KL",
                           model = NULL,
                           theta = estimateTheta(binary_cat))
        }
      binary_differences <- abs(theirMEI - ourMEI)
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
      theirMEI <- ourMEI <- numeric(100)
      for(i in 1:nrow(poly_data)){
        poly_cat@answers[1:length(poly_cat@guessing)] <- rep(NA, length(poly_cat@guessing))
        poly_cat@answers[1:5] <- unlist(poly_data[i, 1:5])
        theirMEI[i] <- MEI(itemBank = bank,
                        it.given <- bank[1:5, ],
                        x = unlist(poly_data[i, 1:5]) -1,
                        item = 6,
                        theta = estimateTheta(poly_cat),
                        model = "GRM",
                        parInt = c(-5,5,101),
                        infoType = "observed")
        ourMEI[i] <- expectedObsInf(poly_cat, 6)
      }
      poly_differences <- abs(theirMEI - ourMEI)
      return(round(poly_differences, 3))
    }
    
  }
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]

  expect_true(all.equal(expectedKL_test_CatR(FALSE), rep(0, 100), tolerance = .01))
  expect_true(all.equal(expectedKL_test_CatR(TRUE), rep(0, 100), tolerance = .01))
})
 