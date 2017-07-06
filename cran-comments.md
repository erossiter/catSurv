## Release Summary
* We tried to address the one UBSAN error found here: https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-UBSAN/catSurv/catSurv-Ex.Rout. However, we cannot reproduce error using `check_with_sanitizers()` from rhub package.  We suspect it may be a problem with RcppParallel passing the UBSAN tests, as noted here: https://github.com/RcppCore/RcppParallel/issues/36.

* In addition, this patch release adds two important functions, `estimateThetas()` and `simulateThetas()`.

## Test environments
* local OS X install, R 3.4.0
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking for GNU extensions in Makefiles ... NOTE GNU make is a SystemRequirements.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv using `devtools::revdep_check()`.
All packages that I could install passed with result "No ERRORs or WARNINGs found :)".