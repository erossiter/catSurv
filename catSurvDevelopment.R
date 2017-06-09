## libraries for package maintenance
rm(list = ls())
library(devtools)
library(roxygen2)
#setwd("~/Github/CATsurv")
setwd("~/Dropbox/Spring2016/Rclass/CATSurv/")

## loading the package
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)
#test(current.code)
#check(current.code)
#release(current.code, check = FALSE)
submit_cran(current.code)

## loading objects for the purposes of creating tests
load("catSurv/tests/testthat/cat_objects.Rdata")

## Checking downstream dependencies
revdep_check(current.code)
revdep_check_print_problems(current.code)

## Checking package on windows platform
#build_win(current.code)

## Building other important files
#use_readme_rmd(current.code)
#use_build_ignore("NEWS.md", pkg = "catSurv")
#use_build_ignore("README.md", pkg = "catSurv")

## for looking at pdf of documentation
unlink("catSurv.pdf") ## deleting current version
path <- find.package("catSurv")
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))

## checking what version of packages I have installed to update
## DESCRIPTION when needed (comments are what's currently
## in description file)
library(utils)
version #3.4.0
packageVersion("RcppArmadillo") #‘0.7.900.2.0’
packageVersion("stats") #‘3.4.0’
packageVersion("Rcpp") #‘0.12.11’
packageVersion("RcppGSL") #‘0.3.2’
packageVersion("BH") #‘1.62.0.1’
packageVersion("ltm") #‘1.0.0’
packageVersion("catR") #‘3.12’
packageVersion("catIrt") #‘0.5.0’
packageVersion("testthat") #‘1.0.2’
packageVersion("methods") #‘3.4.0’


