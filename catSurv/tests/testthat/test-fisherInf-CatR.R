library(catSurv)
library(catR)
library(ltm)
context("fisherInf")

test_that("fisherInf calculates correctly", {

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  # cat <- ltmCat(binary_data)
  # cat@answers <- as.numeric(binary_data[1,])
  # fisherInf(cat, 0, 5)
  # probability(cat, 1, 1)
  # 
  # it <- matrix(c(cat@discrimination,
  #                cat@difficulty,
  #                cat@guessing,
  #                rep(0, length(cat@guessing))),
  #              ncol = 4)
  # 
  # Ii(th = 1, it = it, model = NULL)$Ii

  ## Categorical
  cat <- grmCat(poly_data)
  cat_coefs <- coef(grm(poly_data))

  for(i in 1:5){
    print(fisherInf(cat,1,i))
  }
                
  it <- matrix(c(cat@discrimination,
                 cat_coefs[,1],
                 cat_coefs[,2],
                 cat_coefs[,3],
                 cat_coefs[,4]),
               ncol = 5)
  Ii(1, it, model = "GRM")$Ii[1:5]
  
  Pi(1, it, "GRM")$Pi[1,]
  probability(cat, 1, 1)

})