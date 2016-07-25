data(npi)
binary_cat <- ltmCat(npi[1:100, ], 100)
binary_cat@priorName <- "NORMAL"
binary_cat@priorParams <- c(0,1)
binary_cat@lowerBound <- -4
binary_cat@upperBound <- 4
binary_cat@answers[1:40] <- rep(NA, 40)

## probability looks fine
probability(binary_cat, 1, 1)
probability(binary_cat, -6, 1)
probability(binary_cat, 6, 1)

## likelihood always returns 1
likelihood(binary_cat, -6)
likelihood(binary_cat, 0)
likelihood(binary_cat, 6)

## all routines return 0
binary_cat@estimation <- "MAP"
estimateTheta(binary_cat)

binary_cat@estimation <- "EAP"
estimateTheta(binary_cat)

binary_cat@estimation <- "MLE"
estimateTheta(binary_cat)

binary_cat@estimation <- "WLE"
estimateTheta(binary_cat)

#### SELECT ITEM ROUTINES ####

## returns NaN
binary_cat@selection <- "EPV"
expectedPV(binary_cat, 1)
selectItem(binary_cat)

## works fine
binary_cat@selection <- "MEI"
selectItem(binary_cat)

## throws appropriate error
binary_cat@selection <- "MFI"
selectItem(binary_cat)

## throws appropriate error
binary_cat@selection <- "MLWI"
selectItem(binary_cat)

## throws appropriate error
binary_cat@selection <- "MPWI"
selectItem(binary_cat)

## returns 0
binary_cat@selection <- "KL"
selectItem(binary_cat)

## works fine
binary_cat@selection <- "PKL"
selectItem(binary_cat)

## works fine
binary_cat@selection <- "LKL"
selectItem(binary_cat)

## all 0
binary_cat@selection <- "RANDOM"
selectItem(binary_cat)



