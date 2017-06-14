Sys.setenv("R_TESTS" = "")
library(testthat)
library(catSurv)

test_check("catSurv")
