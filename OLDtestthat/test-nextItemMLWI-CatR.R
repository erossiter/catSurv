library(catSurv)
library(catR)
library(ltm)
context("nextItemMLWI")

test_that("nextItemMLWI calculates correctly", {
  
  nextItemMLWI_catR <- function(poly){
  
    if(poly == FALSE){
        binary_cat <- ltmCat(binary_data)
        ltm_cat <- ltm(binary_data ~ z1, control = list(GHk = 100))
        cat_coefs <- coef(ltm_cat)
        
        bank <- matrix(c(binary_cat@discrimination,
                  cat_coefs[,1],
                  binary_cat@guessing,
                  rep(1, length(binary_cat@guessing))), ncol = 4)
        
      
        ## excluding people that have a question with NA
        exclude_people <- which(sapply(1:100, function(x) any(is.na(binary_data[x, ]))))
        these_people <- c(1:100)[-exclude_people]
        
        ours <- theirs <- numeric(length(these_people))
        for(j in 1:length(these_people)){
          binary_cat@answers <- rep(NA, length(binary_cat@guessing))
          binary_cat@answers[13:40] <- unlist(binary_data[these_people[j],13:40])
          binary_cat@estimation <- "MAP"
          binary_cat@selection <- "MLWI"

          ours[j] <- selectItem(binary_cat)$next.item
        
          theirs[j] <- nextItem(itemBank = bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[these_people[j],13:40]),
                                criterion = "MLWI", 
                                parInt = c(-5,5,101)
                                )$item
        }
        
      if(length(which(ours != theirs)) == 0){
        return_this <- NULL
      }
      
      return(return_this)
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
      
      ours <- theirs <- numeric(100)
        for(j in 1:length(ours)){
          poly_cat@answers <- rep(NA, length(poly_cat@guessing))
          poly_cat@answers[3:14] <- unlist(poly_data[j, 3:14])
          poly_cat@estimation <- "MAP"
          poly_cat@selection <- "MLWI"
          
          ours[j] <- selectItem(poly_cat)$next.item
          theirs[j] <- nextItem(itemBank = bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[j, 3:14])-1,
                                criterion = "MLWI", 
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item
        }
      
      if(length(which(ours != theirs)) == 0){
        return_this <- NULL
      }
      
      return(return_this)
    }
  }
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_null( nextItemMLWI_catR(FALSE))
  expect_null( nextItemMLWI_catR(TRUE))
  
}
)

  

