## libraries for package maintenece
rm(list = ls())
library(devtools)
library(roxygen2)
#setwd("~/Github/CATsurv")
setwd("~/Dropbox/Spring2016/Rclass/CATSurv/")

## loading the package
current.code <- as.package("catSurv")
load_all(current.code)#, recompile = FALSE)
document(current.code)
#test(current.code)
#check(current.code)

## for looking at pdf of documentation
unlink("catSurv.pdf") ## deleting current version
path <- find.package("catSurv")
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))

## loading objects for the purposes of creating tests
load("catSurv/tests/testthat/cat_objects.Rdata")

## checking what version of packages I have installed to update
## DESCRIPTION when needed (comments are what's currently 
## in description file)
library(utils)
version #3.3.2
packageVersion("RcppArmadillo") #‘0.7.700.0.0’
packageVersion("stats") #‘3.3.2’
packageVersion("Rcpp") #‘0.12.9’
packageVersion("RcppGSL") #‘0.3.1’
packageVersion("BH") #‘1.60.0.2’
packageVersion("ltm") #‘1.0.0’
packageVersion("catR") #‘3.10’
packageVersion("catIrt") #‘0.5.0’
packageVersion("testthat") #‘1.0.2’
packageVersion("methods") #‘3.3.2’


