## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(Rcpp)
library(testthat)
setwd("/Users/iramalis/Desktop/gitstuff/CATSurv")
setwd("/Users/erinrossiter/Dropbox/Spring2016/Rclass/CATsurv")
setwd("~/Dropbox/2016Spring_Programming/CATSurv")

getwd()
source("Users/iramalis/Desktop/gitstuff/CATSurv/catSurv/tests/testthat/test-probability.R")
test("/Users/iramalis/Desktop/gitstuff/CATSurv/catSurv/tests/testthat/test-probability.R")

current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)


cat_dnorm <- new("Cat")
cat_dnorm@discrimination <- c(2)
cat_dnorm@difficulty <- c(1)
cat_dnorm@priorName <- "NORMAL"
cat_dnorm@priorParams <- c(0,1.5)

cat_dnorm
probability(cat_dnorm,1,1)

test_cat <- new("Cat")

?setters
setguessing(test_cat)<-c(0)
test_cat@discrimination<-.5
test_cat@difficulty<-.8
test_cat@guessing<-.1

probability(test_cat, 1, 1)



?setters
?setdiscrimination



## Checking the "set" methods.
test_cat <- new("Cat")

setguessing(test_cat) <- 5
test_cat@guessing

setdiscrimination(test_cat) <- 5
test_cat@discrimination

validObject(test_cat)


setdifficulty(test_cat)<-c(0,1,2,3)






test_dir("/Users/iramalis/Desktop/gitstuff/CATSurv/catSurv/tests/testthat/probs")


?probability
?likelihood
warnings()
?estimateSE
?estimateTheta

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
