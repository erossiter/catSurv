# catSurv 1.0.3

### Major Changes
* New functions `estimateThetas()` and `simulateThetas()` allow for estimation of ability parameter for dataframe of response sets.

### Minnor Changes
* Streamlined `checkStopRules()`.


# catSurv 1.0.2

### Major Changes
* Parallelized main functionality of package --- `selectItem()`.


### Bug Fixes
* Fixed bug in `ltmCat()`, `tpmCat()`, `grmCat()`, and `gpcmCat()` by adding the `ltm` package to Imports.
* Fixed bug in KL functions, where we were integrating over a function that itself called integration.
* Removed versioning from `stats` and `methods` imports as it caused errors in testing with r-oldrel.

# catSurv 1.0.1
* Corrected errors regarding portability of code to Solaris operating systems.
