## Resubmission
This is a resubmission. In this version I have:

* Removed 'OS_type: unix' from from DESCRIPTION file.  Package now works under Windows.

## Test environments
* local OS X install, R 3.4.0
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTES:

1. New submission.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv using `devtools::revdep_check()`.
All packages that I could install passed with result "No ERRORs or WARNINGs found :)".