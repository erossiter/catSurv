## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(Rcpp)
#setwd("/Users/iramalis/Desktop/gitstuff/CATSurv")
setwd("/Users/erinrossiter/Dropbox/Spring2016/Rclass/CATsurv")


current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)

## Install the package
install(pkg=current.code, local=TRUE, args="--no-multiarch")

## For Testing
setwd("C:/Users/Haley/Documents/PDSL/CAT_Survey/CAT-Survey")
library(rjson)
library(CATPack)

json_cat <- fromJSON(file="eqModel.txt") ## for poly
json_cat <- fromJSON(file="narcModel.txt") ## for binary

cat <- new("CATsurv")
cat@guessing <- json_cat$guessing
cat@discrimination <- unlist(json_cat$discrimination)
cat@answers <- as.numeric(json_cat$answers)
cat@priorName <- json_cat$priorName
cat@priorParams <- json_cat$priorParams
cat@lowerBound <- json_cat$lowerBound
cat@upperBound <- json_cat$upperBound
cat@quadPoints <- json_cat$quadPoints
cat@D <- json_cat$D
cat@X <- json_cat$X

## for poly
cat@difficulty <- lapply(json_cat$difficulty, unlist)
cat@poly <- TRUE

## for binary
cat@difficulty <- unlist(json_cat$difficulty)
cat@poly <- FALSE

## Change cat@selection to change nextItem algorithm
nextItem(cat) 

## Run this if you change code and need to resource the cpp file
sourceCpp("./catSurv/src/epv.cpp")

## Build a version of the package to share manually
build(current.code, path=getwd())