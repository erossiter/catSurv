## Release Summary
* This patch release fixes C++ errors regarding portability uncovered during the 'r-patched-solaris-x86' flavor of CRAN package checks.

## Test environments
* local OS X install, R 3.4.0
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv using `devtools::revdep_check()`.
All packages that I could install passed with result "No ERRORs or WARNINGs found :)".