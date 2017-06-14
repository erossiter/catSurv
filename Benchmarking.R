library(catSurv)
library(ltm)
library(catR)
library(microbenchmark)

data("npi")
data("nfc")
data("AMTknowledge")
ltm_data <- npi[1:100, ]
tpm_data <- AMTknowledge[1:100, ]
poly_data <- nfc[1:100, ]

ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
Cat.ltm <- ltmCat(ltm.ltm, quadraturePoints = 100)
Cat.ltm@answers[1:5] <- sample(0:1, 5, replace = T)

it.ltm <- matrix(c(Cat.ltm@discrimination, coef(ltm.ltm)[,1], 
                   rep(0,length(Cat.ltm@discrimination)), 
                   rep(1, length(Cat.ltm@discrimination))),
                 ncol= 4, byrow = F)

x.ltm <- Cat.ltm@answers

ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
Cat.tpm <- tpmCat(ltm.tpm, 100)
Cat.tpm@answers[1:5] <- sample(0:1, 5, replace = T)


it.tpm <- matrix(c(Cat.tpm@discrimination, coef(ltm.tpm)[,2], 
                   Cat.tpm@guessing, 
                   rep(1, length(Cat.tpm@discrimination))),
                 ncol= 4, byrow = F)  

x.tpm <- Cat.tpm@answers

ltm.grm <- grm(poly_data, control = list(GHk = 100))
Cat.grm <- grmCat(ltm.grm, 100)
Cat.grm@answers[1:5] <- sample(1:5, 5, replace = T)

it.grm <- matrix(c(coef(ltm.grm)[,5], coef(ltm.grm)[,1:4]),
                  ncol= 5, byrow = F)

x.grm <- Cat.grm@answers-1

### Probability

microbenchmark(probability(Cat.ltm, 0, 1), Pi(0, it.ltm)$Pi[1])

microbenchmark(probability(Cat.tpm, 0, 1), Pi(0, it.tpm)$Pi[1])

microbenchmark(probability(Cat.grm, 0, 1), Pi(0, it.grm, model="GRM")$Pi[1,])

### Prior (benchmarked against R base functions)

## STUDENT_T
Cat.ltm@priorName <- "STUDENT_T"
microbenchmark(prior(1, Cat.ltm@priorName, Cat.ltm@priorParams), 
               dt(1, df = Cat.ltm@priorParams[2], ncp = Cat.ltm@priorParams[1]))

Cat.tpm@priorName <- "STUDENT_T"
microbenchmark(prior(1, Cat.tpm@priorName, Cat.tpm@priorParams), 
               dt(1, df = Cat.tpm@priorParams[2], ncp = Cat.tpm@priorParams[1]))

Cat.grm@priorName <- "STUDENT_T"
microbenchmark(prior(1, Cat.grm@priorName, Cat.grm@priorParams), 
               dt(1, df = Cat.grm@priorParams[2], ncp = Cat.grm@priorParams[1]))

## UNIFORM
Cat.ltm@priorName <- "UNIFORM"
microbenchmark(prior(1, Cat.ltm@priorName, Cat.ltm@priorParams), 
               dunif(1, Cat.ltm@lowerBound, Cat.ltm@upperBound)) 

Cat.tpm@priorName <- "UNIFORM"
microbenchmark(prior(1, Cat.tpm@priorName, Cat.tpm@priorParams), 
               dunif(1, Cat.tpm@lowerBound, Cat.tpm@upperBound)) 

Cat.grm@priorName <- "UNIFORM"
microbenchmark(prior(1, Cat.grm@priorName, Cat.grm@priorParams), 
               dunif(1, Cat.grm@lowerBound, Cat.grm@upperBound)) 

## NORMAL

Cat.ltm@priorName <- "NORMAL"
microbenchmark(prior(1, Cat.ltm@priorName, Cat.ltm@priorParams), 
               dnorm(1, Cat.ltm@priorParams[1], Cat.ltm@priorParams[2]))

Cat.tpm@priorName <- "NORMAL"
microbenchmark(prior(1, Cat.tpm@priorName, Cat.tpm@priorParams), 
               dnorm(1, Cat.tpm@priorParams[1], Cat.tpm@priorParams[2]))

Cat.grm@priorName <- "NORMAL"
microbenchmark(prior(1, Cat.grm@priorName, Cat.grm@priorParams), 
               dnorm(1, Cat.grm@priorParams[1], Cat.grm@priorParams[2]))

### Observed Information

microbenchmark(obsInf(Cat.ltm, 0, 1), OIi(0, it.ltm, x.ltm)[1])

microbenchmark(obsInf(Cat.tpm, 0, 1), OIi(0, it.tpm, x.tpm)[1])

microbenchmark(obsInf(Cat.grm, 0, 1), OIi(0, it.grm, x.grm, model = "GRM"))

### selectItem

## EPV
Cat.ltm@selection <- "EPV"
microbenchmark(selectItem(Cat.ltm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MEPV"))

Cat.tpm@selection <- "EPV"
microbenchmark(selectItem(Cat.tpm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MEPV"))

Cat.grm@selection <- "EPV"
microbenchmark(selectItem(Cat.grm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MEPV"))

## MFI
Cat.ltm@selection <- "MFI"
microbenchmark(selectItem(Cat.ltm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MFI"))

Cat.ltm@selection <- "MFI"
microbenchmark(selectItem(Cat.tpm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MFI"))

Cat.ltm@selection <- "MFI"
microbenchmark(selectItem(Cat.grm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MFI"))

## MEI
Cat.ltm@selection <- "MEI"
microbenchmark(selectItem(Cat.ltm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MEI"))

Cat.ltm@selection <- "MEI"
microbenchmark(selectItem(Cat.tpm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MEI"))

Cat.ltm@selection <- "MEI"
microbenchmark(selectItem(Cat.grm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MEI"))

## MLWI
Cat.ltm@selection <- "MLWI"
microbenchmark(selectItem(Cat.ltm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MLWI"))

Cat.ltm@selection <- "MLWI"
microbenchmark(selectItem(Cat.tpm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MLWI"))

Cat.ltm@selection <- "MLWI"
microbenchmark(selectItem(Cat.grm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MLWI"))

## MPWI
Cat.ltm@selection <- "MPWI"
microbenchmark(selectItem(Cat.ltm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MPWI"))

Cat.ltm@selection <- "MPWI"
microbenchmark(selectItem(Cat.tpm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MPWI"))

Cat.ltm@selection <- "MPWI"
microbenchmark(selectItem(Cat.grm), 
               nextItem(itemBank = it.ltm, theta = estimateTheta(Cat.ltm), criterion = "MPWI"))

### Fisher's Information

microbenchmark(fisherInf(Cat.ltm, 0, 1), Ii(0, it.ltm)$Ii[1])

microbenchmark(fisherInf(Cat.tpm, 0, 1), Ii(0, it.tpm)$Ii[1])

microbenchmark(fisherInf(Cat.grm, 0, 1), Ii(0, it.grm, model = "GRM")$Ii[1])

### Expected Observed Information

microbenchmark(expectedObsInf(Cat.ltm, 1), 
               MEI(itemBank = it.ltm, it.given = it.ltm[1,], x = 1, item = 1, theta = estimateTheta(Cat.ltm)))

microbenchmark(expectedObsInf(Cat.tpm, 1), 
               MEI(itemBank = it.tpm, it.given = it.tpm[1,], x = 1, item = 1, theta = estimateTheta(Cat.tpm)))

microbenchmark(expectedObsInf(Cat.grm, 1), 
               MEI(itemBank = it.grm, it.given = it.grm[1,], x = 1, item = 1, theta = estimateTheta(Cat.grm), model = "GRM"))

### Expected PV

microbenchmark(expectedPV(Cat.ltm, 1),
               EPV(itemBank = it.ltm, it.given = it.ltm[1,], x = 1, item = 1, theta = estimateTheta(Cat.ltm)))

microbenchmark(expectedPV(Cat.tpm, 1),
               EPV(itemBank = it.tpm, it.given = it.tpm[1,], x = 1, item = 1, theta = estimateTheta(Cat.tpm)))

microbenchmark(expectedPV(Cat.grm, 1),
               EPV(itemBank = it.grm, it.given = it.grm[1,], x = 1, item = 1, theta = estimateTheta(Cat.grm), model = "GRM"))

### Expected KL (KL estimates are not the same between packages)(catR has no LKL analog)

## expectedKL

microbenchmark(expectedKL(Cat.ltm, 1), 
               KL(itemBank = it.ltm, it.given = it.ltm[1,], x = 1, item = 1, theta = estimateTheta(Cat.ltm)))

microbenchmark(expectedKL(Cat.tpm, 1), 
               KL(itemBank = it.tpm, it.given = it.tpm[1,], x = 1, item = 1, theta = estimateTheta(Cat.tpm)))

microbenchmark(expectedKL(Cat.grm, 1), 
               KL(itemBank = it.grm, it.given = it.grm[1,], x = 1, item = 1, theta = estimateTheta(Cat.grm), model = "GRM"))

## posteriorKL

microbenchmark(posteriorKL(Cat.ltm, 1), 
               KL(itemBank = it.ltm, it.given = it.ltm[1,], x = 1, item = 1, theta = estimateTheta(Cat.ltm), type = "KLP"))

microbenchmark(posteriorKL(Cat.tpm, 1), 
               KL(itemBank = it.tpm, it.given = it.tpm[1,], x = 1, item = 1, theta = estimateTheta(Cat.tpm), type = "KLP"))

microbenchmark(posteriorKL(Cat.grm, 1), 
               KL(itemBank = it.grm, it.given = it.grm[1,], x = 1, item = 1, theta = estimateTheta(Cat.grm), type = "KLP", model = "GRM"))

### EstimateTheta

## EAP

Cat.ltm@estimation <- "EAP"
microbenchmark(estimateTheta(Cat.ltm), 
               thetaEst(it.ltm, x.ltm, method = "EAP"))

Cat.tpm@estimation <- "EAP"
microbenchmark(estimateTheta(Cat.tpm),
               thetaEst(it.tpm, x.tpm, method = "EAP"))

Cat.grm@estimation <- "EAP"
microbenchmark(estimateTheta(Cat.grm), 
               thetaEst(it = it.grm, x = x.grm, method = "EAP", model = "GRM"))

## MAP

Cat.ltm@estimation <- "MAP"
microbenchmark(estimateTheta(Cat.ltm), 
               thetaEst(it.ltm, x.ltm, method = "BM"))

Cat.tpm@estimation <- "MAP"
microbenchmark(estimateTheta(Cat.tpm), 
               thetaEst(it.tpm, x.tpm, method = "BM"))

Cat.grm@estimation <- "MAP"
microbenchmark(estimateTheta(Cat.grm), 
               thetaEst(it = it.grm, x = x.grm, method = "BM", model = "GRM"))

## MLE

Cat.ltm@estimation <- "MLE"
microbenchmark(estimateTheta(Cat.ltm),
               thetaEst(it.ltm, x.ltm, method = "ML"))

Cat.tpm@estimation <- "MLE"
microbenchmark(estimateTheta(Cat.tpm), #estimateTheta is giving a bad value here
               thetaEst(it.tpm, x.tpm, method = "ML"))

Cat.grm@estimation <- "MLE"
microbenchmark(estimateTheta(Cat.grm),
               thetaEst(it = it.grm, x = x.grm, method = "ML", model = "GRM"))

## WLE

Cat.ltm@estimation <- "WLE"
microbenchmark(estimateTheta(Cat.ltm), 
               thetaEst(it.ltm, x.ltm, method = "WL"))

Cat.tpm@estimation <- "WLE"
microbenchmark(estimateTheta(Cat.tpm), 
               thetaEst(it.tpm, x.tpm, method = "WL"))

Cat.grm@estimation <- "WLE"
microbenchmark(estimateTheta(Cat.grm), 
               thetaEst(it = it.grm, x = x.grm, method = "WL", model = "GRM"))

### EstimateSE

## EAP

Cat.ltm@estimation <- "EAP"
microbenchmark(estimateSE(Cat.ltm), 
               semTheta(thEst = estimateTheta(Cat.ltm), it = it.ltm, x = x.ltm, method = "EAP", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))

Cat.tpm@estimation <- "EAP"
microbenchmark(estimateSE(Cat.tpm),
               semTheta(thEst = estimateTheta(Cat.tpm), it = it.tpm, x = x.tpm, method = "EAP", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))

Cat.grm@estimation <- "EAP"
microbenchmark(estimateSE(Cat.grm), 
               semTheta(thEst = estimateTheta(Cat.grm), it = it.grm, x = x.grm, method = "EAP", 
                        priorDist = "norm", model = "GRM", priorPar = c(0,1), parInt = c(-10, 10, 100)))

## MAP

Cat.ltm@estimation <- "MAP"
microbenchmark(estimateSE(Cat.ltm), 
               semTheta(thEst = estimateTheta(Cat.ltm), it = it.ltm, x = x.ltm, method = "BM", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))


Cat.tpm@estimation <- "MAP"
microbenchmark(estimateSE(Cat.tpm), 
               semTheta(thEst = estimateTheta(Cat.tpm), it = it.tpm, x = x.tpm, method = "BM", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))


Cat.grm@estimation <- "MAP"
microbenchmark(estimateSE(Cat.grm), 
               semTheta(thEst = estimateTheta(Cat.grm), it = it.grm, x = x.grm, method = "BM", 
                        priorDist = "norm", model = "GRM", priorPar = c(0,1), parInt = c(-10, 10, 100)))


## MLE

Cat.ltm@estimation <- "MLE"
microbenchmark(estimateSE(Cat.ltm),
               semTheta(thEst = estimateTheta(Cat.ltm), it = it.ltm, x = x.ltm, method = "ML", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))


Cat.tpm@estimation <- "MLE"
microbenchmark(estimateSE(Cat.tpm), # abnormal estimate
               semTheta(thEst = estimateTheta(Cat.tpm), it = it.tpm, x = x.tpm, method = "ML", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))


Cat.grm@estimation <- "MLE"
microbenchmark(estimateSE(Cat.grm),
               semTheta(thEst = estimateTheta(Cat.grm), it = it.grm, x = x.grm, method = "ML", 
                        priorDist = "norm", model = "GRM", priorPar = c(0,1), parInt = c(-10, 10, 100)))


## WLE

Cat.ltm@estimation <- "WLE"
microbenchmark(estimateSE(Cat.ltm), 
               semTheta(thEst = estimateTheta(Cat.ltm), it = it.ltm, x = x.ltm, method = "WL", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))


Cat.tpm@estimation <- "WLE"
microbenchmark(estimateSE(Cat.tpm), 
               semTheta(thEst = estimateTheta(Cat.tpm), it = it.tpm, x = x.tpm, method = "WL", 
                        priorDist = "norm", priorPar = c(0,1), parInt = c(-10, 10, 100)))


Cat.grm@estimation <- "WLE"
microbenchmark(estimateSE(Cat.grm), 
               semTheta(thEst = estimateTheta(Cat.grm), it = it.grm, x = x.grm, method = "WL", 
                        priorDist = "norm", model = "GRM", priorPar = c(0,1), parInt = c(-10, 10, 100)))


# ------------------functions without catR analogs------------------- #

### Likelihood 

likelihood(Cat.ltm, 0)

likelihood(Cat.tpm, 0)

likelihood(Cat.grm, 0)

### dLL

dLL(Cat.ltm, 0, FALSE)

dLL(Cat.ltm, 0, TRUE)

dLL(Cat.tpm, 0, FALSE)

dLL(Cat.tpm, 0, TRUE)

dLL(Cat.grm, 0, FALSE)

dLL(Cat.grm, 0, TRUE)

### d2LL


