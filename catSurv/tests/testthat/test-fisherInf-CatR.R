library(catSurv)
library(catR)
library(ltm)
context("fisherInf")

test_that("fisherInf calculates correctly", {

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  cat <- ltmCat(binary_data)
  cat@answers <- as.numeric(binary_data[1,])
  fisherInf(cat, 0, 5)
  probability(cat, 1, 1)
  
  it <- matrix(c(cat@discrimination,
                 cat@difficulty,
                 cat@guessing,
                 rep(0, length(cat@guessing))),
               ncol = 4)
  
  Ii(th = 1, it = it, model = NULL)$Ii

  ## Categorical
  cat <- grmCat(poly_data)
  cat@answers <- as.numeric(poly_data[1,])
  cat@poly <- TRUE
  for(i in 1:5) print(fisherInf(cat,1,i))
                
  it <- matrix(c(#cat@discrimination,
                 unlist(lapply(cat@difficulty, function(x) x[[1]])),
                 unlist(lapply(cat@difficulty, function(x) x[[2]])),
                 unlist(lapply(cat@difficulty, function(x) x[[3]])),
                 unlist(lapply(cat@difficulty, function(x) x[[4]]))),
               ncol = 4)
  Ii(1, it, model = "GRM")$Ii[1]
  Pi(1, it, "GRM")$Pi
  

})