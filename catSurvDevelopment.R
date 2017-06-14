## libraries for package maintenance
rm(list = ls())
library(devtools)
library(roxygen2)
#setwd("~/Github/CATsurv")
setwd("~/Dropbox/Spring2016/Rclass/CATSurv/")

## loading the package
current.code <- as.package("catSurv")
load_all(current.code)
#document(current.code)
#build(current.code)
#test(current.code)
#check(current.code)
#release(current.code)

load("catSurv/tests/testthat/cat_objects.Rdata")
gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
probability(gpcm_cat, 0, 1)


gpcm_cat@difficulty
estimateTheta(gpcm_cat)


estimateTheta(catObj) ## in main.cpp
- estimateTheta() ## defined in Cat.cpp, there are many routines to this function, but this catObj is using EAP
-- estimateTheta() ## defined in EAPEstimator.cpp
--- integralQuotient() ## defined in EAPEstimator.cpp
## does this once for "numerator"
---- GSLFunctionWrapper().asGSLfunction() ## both defined in GSLFunctionWrapper.cpp
---- integrate() ## defined in Integrator.cpp
----- gsl_integration_qag() ## included from GSL via #include <gsl/gsl_integration.h>
------ likelihood() ## defined in Estimator.cpp
------- likelihood_gpcm() ## defined in Estimator.cpp
-------- probability() ## defined in Estimator.cpp
--------- prob_gpm() ## defined in Estimator.cpp
------ prior() ## defined in Prior.cpp
------- normal() ## defined in Prior.cpp via Boost includes
## does this again for "denominator"
---- GSLFunctionWrapper().asGSLfunction() ## both defined in GSLFunctionWrapper.cpp
---- integrate() ## defined in Integrator.cpp
----- gsl_integration_qag() ## included from GSL via #include <gsl/gsl_integration.h>
------ likelihood() ## defined in Estimator.cpp
------- likelihood_gpcm() ## defined in Estimator.cpp
-------- probability() ## defined in Estimator.cpp
--------- prob_gpm() ## defined in Estimator.cpp
------ prior() ## defined in Prior.cpp
------- normal() ## defined in Prior.cpp via Boost includes
## I did not include calls to the questionSet struct to access data which occur throughout


## checking on other platforms
#install_github("r-hub/rhub", force = TRUE)
library(rhub)
platforms()
validate_email("erinrossiter@wustl.edu", token = "7344ccb49bec4905a87d5feb23f9e11e")
#check("catSurv_1.0.0.tar.gz", platform = "debian-gcc-devel", email = "erinrossiter@wustl.edu")
check_with_valgrind("catSurv_1.0.0.tar.gz", email = "erinrossiter@wustl.edu")
check_with_sanitizers("catSurv_1.0.0.tar.gz", email = "erinrossiter@wustl.edu")

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


