library(catSurv)
library(catR)
library(ltm)
context("nextItemMFI")

test_that("nextItemMFI calculates correctly", {
  
  nextItemMFI_catR <- function(poly){
  
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
          binary_cat@selection <- "MEI"

          ours[j] <- selectItem(binary_cat)$next.item
        
          theirs[j] <- nextItem(itemBank = bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[these_people[j],13:40]),
                                criterion = "MEI", 
                                parInt = c(-5,5,101)
                                )$item
        }
      
        ## 8 not equal   
        return(which(ours != theirs))
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
      
      ours <- theirs <- numeric(8)
        for(j in 1:length(ours)){
          poly_cat@answers <- rep(NA, length(poly_cat@guessing))
          poly_cat@answers[3:14] <- unlist(poly_data[j, 3:14])
          poly_cat@estimation <- "MAP"
          poly_cat@selection <- "MEI"
          
          ours[j] <- selectItem(poly_cat)$next.item
          theirs[j] <- nextItem(itemBank = bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[j, 3:14])-1,
                                criterion = "MEI", 
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item
        }
      return(which(ours != theirs))
    }
  }
  
  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal( nextItemMFI_catR(TRUE), c(43, 58, 61, 63, 69, 72))
  expect_null( nextItemMFI_catR(FALSE))
  
}
)

  

