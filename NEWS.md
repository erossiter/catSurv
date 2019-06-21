# catSurv 1.0.4

### Major Changes
* Package functionality is now integratable with Qualtrics.  New functions `fromJSONCat()`, `toJSONCat()`, and `readQualtrics()` aid the user in creating an adaptive battery in Qualtrics using `catSurv`.

* New functions `simulateRespondents()`, `allEst()`, `allFish()`, and `oracle()` allow for simulation exercises.

* New function `plot.Cat()` allows for visual representation of item parameters.

### Minor Changes
* The slot `ids` was added to the `Cat` object representing each question item's unique identifier.

* `selectItem()` returns a third item, `next_item_name` which represents the unique identifier of the item that should be asked next.

* `lookAhead()` now returns the next best item given the question is skipped.



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
