Sys.setenv("R_TESTS" = "")
library(testthat)
library(catSurv)

print("Starting testthat.R")

test_check("catSurv")

print("Made it through testthat.R")