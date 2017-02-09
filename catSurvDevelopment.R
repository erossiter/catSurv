## libraries for package maintenece
rm(list=ls())
library(devtools)
library(roxygen2)
setwd("~/Dropbox/Spring2016/Rclass/CATsurv")

## Loading the package
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)
test(current.code)

## loading objects for the purposes of creating tests
load("catSurv/tests/testthat/cat_objects.Rdata")

## checking what version of packages I have installed
## to update DESCRIPTION when needed
library(utils)
version #3.3.2
packageVersion("Rcpp") #‘0.12.7’
packageVersion("RcppGSL") #‘0.3.1’
packageVersion("BH") #‘1.60.0.2’
packageVersion("ltm") #‘1.0.0’
packageVersion("catR") #‘3.10’
packageVersion("catIrt") #‘0.5.0’
packageVersion("testthat") #‘1.0.2’


