## Resubmission
This is a resubmission. In this version I have:

* Removed \dontrun{} from examples, such that each .Rd file has an example that is tested.  Please note that 8 of the 35 .Rd files still have part of the Examples section wrapped in \dontrun{} because the code is comutationally expensive but important to display for users.

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