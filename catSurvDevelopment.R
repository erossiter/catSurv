## Load libraries and set working directory
rm(list=ls())
library(devtools)
library(roxygen2)
# library(Rcpp)
# library(testthat)
# library(ltm)
setwd("~/Dropbox/Spring2016/Rclass/CATsurv")

## Loading the package
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)


# ## GRM
# data("nfc")
# grmcat <- grmCat(nfc[1:100, ])
# grmcat@estimation <- "MAP"
# estimateTheta(grmcat)
# probability(grmcat, 1, 1)
# grmcat@answers[1:5] <- as.numeric(nfc[2,1:5])
# likelihood(grmcat, 1)

## GPCM
gpcm_ltm <- gpcm(nfc[1:100, ])
gpcmcat <- gpcmCat(gpcm_ltm)
gpcmcat@modelFit <- "gpcm"
gpcmcat@estimation <- "EAP"
gpcmcat@selection <- "EPV"

## checking probability
probability(gpcmcat, 1, 1)
prob(gpcmcat, 1, 1)

## checking likelihood -- d2LL is not the same....
gpcmcat@answers[1:18] <- as.numeric(nfc[3,1:18])
likelihood(gpcmcat, 1)
dLL(gpcmcat, 1, FALSE)
d2LL(gpcmcat, 1, FALSE)
like(gpcmcat, theta = 1)

library(catR)
##  EAP estimate of theta -- getting the same thing!
gpcmcat@estimation <- "EAP"
estimateTheta(gpcmcat)
it_poly <- cbind(gpcmcat@discrimination, matrix(unlist(gpcmcat@difficulty),
                                                ncol = 4, byrow = T))
eapEst(as.matrix(it_poly), x = c(gpcmcat@answers)-1, model = "GPCM")
eap(gpcmcat)
factor.scores.gpcm(gpcm_ltm, resp.patterns = matrix(gpcmcat@answers, ncol=18, nrow=1),
                   method = "EAP")



## MAP estimate of theta -- works!
gpcmcat@estimation <- "MAP"
estimateTheta(gpcmcat)
thetaEst(as.matrix(it_poly), x = c(gpcmcat@answers)-1, model = "GPCM", method = "BM")
map(gpcmcat)
factor.scores.gpcm(gpcm_ltm, resp.patterns = matrix(gpcmcat@answers, ncol=18, nrow=1),
                   method = "EB")

## MAP estimateSE
estimateSE(gpcmcat)
factor.scores.gpcm(gpcm_ltm, resp.patterns = matrix(gpcmcat@answers, ncol=18, nrow=1),
                   method = "EB")

## EPV selection (EAP estimation) of next item -- getting the same thing!
gpcmcat@estimation <- "EAP"
gpcmcat@answers[10:18] <- NA
for(q in 10:18){
  a <- expectedPV(gpcmcat, q) ## package
  ## b <- epv(gpcmcat, q) ## my function (changed it to MAP)
  c <- EPV(it_poly, q, model = "GPCM", theta = estimateTheta(gpcmcat),
    x = gpcmcat@answers[1:9]-1, it.given = it_poly[1:9,]) ##catR
  print(c(a,c))
}

## EPV selection (MAP estimation) of next item
## -- getting the same thing as my function, but NOT catR
## I think this is because they don't actually give you the option to
## choose which method to estimate theta (which is needed because 
## you estimate the standard error in the process... this is different
## for EAP and MAP)
gpcmcat@estimation <- "MAP"
gpcmcat@answers[10:18] <- NA
for(q in 10:18){
  a <- expectedPV(gpcmcat, q) ## package
  b <- epv(gpcmcat, q) ## my function
  c <- EPV(it_poly, q, model = "GPCM", theta = estimateTheta(gpcmcat),
    x = gpcmcat@answers[1:9]-1, it.given = it_poly[1:9,]) ##catR
  print(c(a,b,c))
}


## observed information
OIi(th = 1, it = it_poly, x = gpcmcat@answers[1:9]-1, model = "GPCM", D = 1)
obsInf(gpcmcat, 1, 1)

## fisher inf
Ii(th = 1, it = it_poly, model = "GPCM")$Ii
fisherInf(gpcmcat, theta = 1, item = 3)

## expected obs inf
## do the same process and expectedPV, but with obsInf instead...
MEI(theta = estimateTheta(gpcmcat), item = 10, itemBank = it_poly,
    it.given = it_poly[1:9,], x = gpcmcat@answers[1:9]-1, model = "GPCM")
expectedObsInf(gpcmcat, 10)



# ## LTM
# data("npi")
# ltmcat <- ltmCat(npi[1:100,])
# ltmcat@modelFit
# probability(ltmcat, 1, 1)



