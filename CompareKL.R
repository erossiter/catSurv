library(catR)

## binary
data("npi")
binary_data <- npi[1:100,]
binary_cat <- ltmCat(binary_data, 100)
binary_cat@answers[c(13,27)] <- c(1,0)

bank <- matrix(c(binary_cat@discrimination, 
                 binary_cat@difficulty,
                 binary_cat@guessing,
                 rep(1,length(binary_cat@discrimination))),
               ncol=4, nrow = length(binary_cat@discrimination), byrow = FALSE
)
bank

# LKL
binary_cat@selection <- "LKL"
CatKLs <- selectItem(binary_cat)[[1]][,"LKL"]

KLs <- rep(NA, length(binary_cat@answers))
for(i in 1:length(binary_cat@answers)){
  KLs[i] <- KL(itemBank = bank,
               item = i,
               x = c(1,0),
               it.given = bank[c(13, 27),],
               lower = binary_cat@lowerBound,
               upper = binary_cat@upperBound,
               nqp = binary_cat@points)
}
KLs <- KLs[-c(13,27)]

KLdifferences <- matrix(c(CatKLs, KLs, rep(NA, length(CatKLs))),
                      ncol = 3, nrow = length(CatKLs), byrow = FALSE)
colnames(KLdifferences) <- c("CatSurv", "CatR", "Diff")
KLdifferences[,3] <- abs(CatKLs - KLs)
summary(KLdifferences)

# PKL
binary_cat@selection <- "PKL"
CatPKLs <- selectItem(binary_cat)[[1]][,"PKL"]

PKLs <- rep(NA, length(binary_cat@answers))
for(i in 1:length(binary_cat@answers)){
  PKLs[i] <- KL(itemBank = bank,
               item = i,
               x = c(1,0),
               it.given = bank[c(13, 27),],
               lower = binary_cat@lowerBound,
               upper = binary_cat@upperBound,
               nqp = binary_cat@points,
               type = "KLP")
}
PKLs <- PKLs[-c(13,27)]

PKLdifferences <- matrix(c(CatPKLs, PKLs, rep(NA, length(CatPKLs))),
                        ncol = 3, nrow = length(CatPKLs), byrow = FALSE)
colnames(PKLdifferences) <- c("CatSurv", "CatR", "Diff")
PKLdifferences[,3] <- abs(CatPKLs - PKLs)
summary(PKLdifferences)



## categorical
data("nfc")
poly_data <- nfc[1:100,]
poly_cat <- grmCat(poly_data, 100)
poly_cat@lowerBound <- -2
poly_cat@upperBound <- 2
poly_cat@answers[c(14,17)] <- c(1, 3)
selectItem(poly_cat)

poly_bank <- matrix(NA, ncol = 5, nrow = length(poly_cat@discrimination))
for(i in 1:length(poly_cat@discrimination)){
  poly_bank[i,1] <- poly_cat@discrimination[i]
  for(j in 1:length(poly_cat@difficulty[[i]])){
  poly_bank[i,j+1] <- poly_cat@difficulty[[i]][j]
  }
}
poly_bank

# LKL
poly_cat@selection <- "LKL"
PolyCatKLs <- selectItem(poly_cat)[[1]][,"LKL"]

PolyKLs <- rep(NA, length(poly_cat@answers))
for(i in 1:length(poly_cat@answers)){
  PolyKLs[i] <- KL(itemBank = poly_bank,
               item = i,
               x = c(1,3),
               it.given = poly_bank[c(14, 17),],
               lower = poly_cat@lowerBound,
               upper = poly_cat@upperBound,
               nqp = 100,
               model = "GRM")
}
PolyKLs <- PolyKLs[-c(14,17)]

PolyKLdifferences <- matrix(c(PolyCatKLs, PolyKLs, rep(NA, length(PolyCatKLs))),
                        ncol = 3, nrow = length(PolyCatKLs), byrow = FALSE)
colnames(PolyKLdifferences) <- c("CatSurv", "CatR", "Diff")
PolyKLdifferences[,3] <- abs(PolyCatKLs - PolyKLs)
summary(PolyKLdifferences)

# PKL
poly_cat@selection <- "PKL"
PolyCatPKLs <- selectItem(poly_cat)[[1]][,"PKL"]

PolyPKLs <- rep(NA, length(poly_cat@answers))
for(i in 1:length(poly_cat@answers)){
  PolyPKLs[i] <- KL(itemBank = poly_bank,
                item = i,
                x = c(1,3),
                it.given = bank[c(14, 17),],
                lower = poly_cat@lowerBound,
                upper = poly_cat@upperBound,
                nqp = 100,
                type = "KLP",
                model = "GRM")
}
PolyPKLs <- PolyPKLs[-c(14,17)]

PolyPKLdifferences <- matrix(c(PolyCatPKLs, PolyPKLs, rep(NA, length(PolyCatPKLs))),
                         ncol = 3, nrow = length(PolyCatPKLs), byrow = FALSE)
colnames(PolyPKLdifferences) <- c("CatSurv", "CatR", "Diff")
PolyPKLdifferences[,3] <- abs(PolyCatPKLs - PolyPKLs)
summary(PolyPKLdifferences)
