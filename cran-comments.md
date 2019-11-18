## Resubmission
This is a resubmission.  In this version I have:

* Corrected the reference to the method in the Description file  in the form Authors (year) <doi:.....>.  I appologize for this error.


## Prior Resubmission
This is a resubmission.  In this version I have:

* Provided valid links, including the portocol, to external URLs referenced in the documentation.

* Added a reference to the method in the Description file  in the form Authors (year) <doi:.....>.



## Release Summary
* I addressed all instances of the warnings listed here https://cran.r-project.org/web/checks/check_results_catSurv.html regarding catching errors by value instead of by reference.

* I addressed an email from Brian Ripley regarding use of C++ code with bind2nd -- I have ommitted use of that function.

* This release adds several important functions making the package functionality integrable with Qualtrics survey administration.


## Test environments
* local OS X install, R 3.6.1
* win-builder to use the development and release version of R


## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking for GNU extensions in Makefiles ... NOTE GNU make is listed in the SystemRequirements.


## Downstream dependencies
I have also run R CMD check on downstream dependencies of catSurv. All packages passed.