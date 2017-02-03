data(npi)
data(nfc)

binary_cat <- ltmCat(npi[1:100, ], 100)
binary_cat@answers[1:40] <- rep(NA, 40)

poly_cat <- grmCat(nfc[1:100, ], 100)
poly_cat@answers[1:length(poly_cat@guessing)] <- rep(NA, length(poly_cat@guessing))

cat <- binary_cat
## probability looks fine
probability(cat, 1, 1)
probability(cat, -6, 1)
probability(cat, 6, 1)

## likelihood always returns 1
likelihood(cat, -6)
likelihood(cat, 0)
likelihood(cat, 6)

## all routines return 0
cat@estimation <- "MAP"
estimateTheta(cat)

cat@estimation <- "EAP"
estimateTheta(cat)

cat@estimation <- "MLE"
estimateTheta(cat)

cat@estimation <- "WLE"
estimateTheta(cat)

#### SELECT ITEM ROUTINES ####

## returns NaN
cat@selection <- "EPV"
expectedPV(cat, 1)
selectItem(cat)

## works fine
cat@selection <- "MEI"
selectItem(cat)

## throws appropriate error
cat@selection <- "MFI"
selectItem(cat)

## throws appropriate error
cat@selection <- "MLWI"
selectItem(cat)

## throws appropriate error
cat@selection <- "MPWI"
selectItem(cat)

## returns 0
cat@selection <- "KL"
selectItem(cat)

## works fine
cat@selection <- "PKL"
selectItem(cat)

## works fine
cat@selection <- "LKL"
selectItem(cat)

## all 0
cat@selection <- "RANDOM"
selectItem(cat)



