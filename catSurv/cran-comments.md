## Test environments
* local OS X install, R 3.4.0

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

1. New submission.
  
2. File ‘catSurv/R/onLoad.R’: .onLoad calls: packageStartupMessage(msg) See section ‘Good practice’ in '?.onAttach'.
    * Consulted and complied with '?.onAttach'.
  

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv using `devtools::revdep_check()`.
All packages that I could install passed with result "No ERRORs or WARNINGs found :)".