## Release Summary
* This patch release streamlines code to reduce run time significantly and adds a new function to the package.

## Test environments
* local OS X install, R 3.4.0
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking for GNU extensions in Makefiles ... NOTE GNU make is a SystemRequirements.

*checking CRAN incoming feasibility ... NOTE Maintainer: ‘Erin Rossiter <erinrossiter@wustl.edu>’ Days since last update: 3

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv using `devtools::revdep_check()`.
All packages that I could install passed with result "No ERRORs or WARNINGs found :)".