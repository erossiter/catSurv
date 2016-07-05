library(ltm)
library(catR)
library(microbenchmark)
data("npi")
data("nfc")
binary_data <- npi[1:100,]
poly_data <- nfc[1:100,]

row = 100

## setting up binary cat
binary_cat <- ltmCat(binary_data)
ltm_cat <- ltm(binary_data ~ z1, control = list(GHk = 100))
binary_coefs <- coef(ltm_cat)
binary_bank <- matrix(c(binary_cat@discrimination,
                binary_coefs[,1],
                binary_cat@guessing,
                rep(1, length(binary_cat@guessing))), ncol = 4)
binary_cat@answers <- rep(NA, length(binary_cat@guessing))
binary_cat@answers[13:40] <- unlist(binary_data[row,13:40])
binary_cat@estimation <- "MAP"

## setting up poly cat
poly_cat <- grmCat(poly_data)
poly_coefs <- coef(grm(poly_data, IRT.param=TRUE, control = list(GHk = 100)))
poly_bank <- matrix(c(poly_cat@discrimination,
                 poly_coefs[,1],
                 poly_coefs[,2],
                 poly_coefs[,3],
                 poly_coefs[,4]),
                 ncol = 5)
poly_cat@answers <- rep(NA, length(poly_cat@guessing))
poly_cat@answers[3:14] <- unlist(poly_data[row, 3:14])
poly_cat@estimation <- "MAP"


#### MFI ####
binary_cat@selection <- "MFI"
poly_cat@selection <- "MFI"

MFI_benchmark <- microbenchmark("catSurv binary" = selectItem(binary_cat)$next.item,
               "catR binary" = nextItem(itemBank = binary_bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[row, 13:40]),
                                criterion = "MFI", 
                                parInt = c(-5,5,101)
                                )$item,
               "catSurv poly" = selectItem(poly_cat)$next.item,
               "catR poly" = nextItem(itemBank = poly_bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[row, 3:14])-1,
                                criterion = "MFI", 
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item,
               times = 20,
               unit = "s")


#### MEI ####
binary_cat@selection <- "MEI"
poly_cat@selection <- "MEI"

MEI_benchmark <- microbenchmark("catSurv binary" = selectItem(binary_cat)$next.item,
               "catR binary" = nextItem(itemBank = binary_bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[row, 13:40]),
                                criterion = "MEI", 
                                parInt = c(-5,5,101)
                                )$item,
               "catSurv poly" = selectItem(poly_cat)$next.item,
               "catR poly" = nextItem(itemBank = poly_bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[row, 3:14])-1,
                                criterion = "MEI", 
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item,
               times = 20,
               unit = "s")
        
    


#### MPWI ####
binary_cat@selection <- "MPWI"
poly_cat@selection <- "MPWI"

MPWI_benchmark <- microbenchmark("catSurv binary" = selectItem(binary_cat)$next.item,
               "catR binary" = nextItem(itemBank = binary_bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[row, 13:40]),
                                criterion = "MPWI", 
                                parInt = c(-5,5,101)
                                )$item,
               "catSurv poly" = selectItem(poly_cat)$next.item,
               "catR poly" = nextItem(itemBank = poly_bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[row, 3:14])-1,
                                criterion = "MPWI", 
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item,
               times = 20,
               unit = "s")
        
    


#### MLWI ####
binary_cat@selection <- "MLWI"
poly_cat@selection <- "MLWI"

MLWI_benchmark <- microbenchmark("catSurv binary" = selectItem(binary_cat)$next.item,
               "catR binary" = nextItem(itemBank = binary_bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[row, 13:40]),
                                criterion = "MLWI", 
                                parInt = c(-5,5,101)
                                )$item,
               "catSurv poly" = selectItem(poly_cat)$next.item,
               "catR poly" = nextItem(itemBank = poly_bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[row, 3:14])-1,
                                criterion = "MLWI",
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item,
               times = 20,
               unit = "s")
        
    


#### EPV ####
binary_cat@selection <- "EPV"
poly_cat@selection <- "EPV"

EPV_benchmark <- microbenchmark("catSurv binary" = selectItem(binary_cat)$next.item,
               "catR binary" = nextItem(itemBank = binary_bank,
                                theta = estimateTheta(binary_cat),
                                out = 13:40,
                                x = unlist(binary_data[row,13:40]),
                                criterion = "MEPV", 
                                parInt = c(-5,5,101)
                                )$item,
               "catSurv poly" = selectItem(poly_cat)$next.item,
               "catR poly" = nextItem(itemBank = poly_bank,
                                theta = estimateTheta(poly_cat),
                                out = 3:14,
                                x = unlist(poly_data[row, 3:14])-1,
                                criterion = "MEPV", 
                                model = "GRM",
                                parInt = c(-5,5,101)
                                )$item,
               times = 20,
               unit = "s")
        
    



# Binary: person 100, 28/40 questions answered
# Poly: person 100, 12/18 questions answered
EPV_benchmark
MFI_benchmark
MEI_benchmark
MPWI_benchmark
MLWI_benchmark

