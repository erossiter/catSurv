## Release Summary
* This is a release to address all instances of the errors listed here https://cran.r-project.org/web/checks/check_results_catSurv.html regarding the RcppArmadillo::sample function.


## Test environments
* local OS X install, R 3.6.1
* win-builder to use the development and release version of R


## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking for GNU extensions in Makefiles ... NOTE GNU make is listed in the SystemRequirements.


## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv. All packages passed.