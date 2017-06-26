## Release Summary
* This patch release arallelizes the main functionality of the package --- `selectItem()`.

## Test environments
* local OS X install, R 3.4.0
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* Maintainer: ‘Erin Rossiter <erinrossiter@wustl.edu>’ Days since last update: 1

* checking for GNU extensions in Makefiles ... NOTE GNU make is a SystemRequirements.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv using `devtools::revdep_check()`.
All packages that I could install passed with result "No ERRORs or WARNINGs found :)".