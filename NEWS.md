# catSurv 1.5.0

### Minor Changes

* Addressed warning messages about bitwise operators in C++ code
* Removed 'boost' C++ libraries because no longer using for distribution functions


# catSurv 1.4.0


### Minor Changes

* All `Cat` objects are reverted to the bounds of [-5,5].  Likewise, the `Cat` class defaults are now [-5,5].

* Newton Raphson bug fixed.  Bug was causing estimates when no questions have been answered to go to extremes rather than prior mean.

* `simulateThetas()` has a new argument (defaulted to FALSE for backwards compatibility) that when TRUE returns a list of dataframes containing adaptive answer profiles for each Cat objected involved in the simulation


# catSurv 1.3.0


### Minor Changes

* All `Cat` objects now use integration bounds of [-4,4].  Likewise, the `Cat` class defaults are now [-4,4].  This narrows the bounds of integration from [-5,5] to avoid computational issues that arise at "extreme" values of the latent trait.

* `Cat` object `z` slot still defaults to .9, but now in calculating delta for certain integration routines, the package executes qnorm(z).

* `oracle()` function adds option for parallel computing using `plyr`

* `probability()` for categorical data now throws errors to account for extreme values of latent trait that may  cause computational issues



# catSurv 1.2.0


### Minor Changes
* `simulateRespondents()` bug fix when respondent's answer in raw data is `NA`, now transform to -1 to indicate a skip

* added dataset of Need to Evaluate raw response profiles

* `readQualtrics()` now has respondent ID as rownames instead of a column.

* Added example data for the `readQualtrics()` function.



# catSurv 1.1.3

### Major Changes
* New functions `simulateRespondents()`, `simulateThetas()`, `simulateFisherInfo()`, and `oracle()` allow for simulation exercises to evaluate model quality and performace.

* New function `plot.Cat()` allows for visual representation of item parameters.

* New functions `fromJSONCat()`, `toJSONCat()`, and `readQualtrics()` aid the user in creating an adaptive battery in Qualtrics using `catSurv`.

### Minor Changes
* The slot `ids` was added to the `Cat` object representing each question item's unique identifier.

* `selectItem()` returns a third item, `next_item_name` which represents the unique identifier of the item that should be asked next.

* `lookAhead()` now returns the next best item given the question is skipped.



# catSurv 1.0.3

### Major Changes
* New functions `estimateThetas()` and `simulateThetas()` allow for estimation of ability parameter for dataframe of response sets.

### Minor Changes
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
