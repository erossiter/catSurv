## Test environments
* local OS X install, R 3.3.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

1. New submission.
  
2. File ‘catSurv/R/onLoad.R’: .onLoad calls: packageStartupMessage(msg) See section ‘Good practice’ in '?.onAttach'.
    * Consulted and complied with '?.onAttach'.
  

## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv.
All packages that I could install passed.