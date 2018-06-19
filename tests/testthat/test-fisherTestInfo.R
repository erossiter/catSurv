library(catR, quietly = TRUE)
context("fisherTestInfo")
load("cat_objects.Rdata")

test_that("fisherTestInfo calculates correctly", {
  ltm_cat@answers[1:5] <- c(1, 0, 0, 1, 1)
  package_fisherTI <- fisherTestInfo(ltm_cat, 2)
  catR_fisherTI <- sum(Ii(th = 2, it = it_ltm)$Ii[1:5])
    
  expect_equal(package_fisherTI, catR_fisherTI)
})

test_that("fisherTestInfo calculates correctly", {
  grm_cat@answers[1:5] <- c(5, 4, 1, 1, 1)
  package_fisherTI <- fisherTestInfo(grm_cat, 2)
  catR_fisherTI <- sum(Ii(th = 2, it = it_grm, model = "GRM")$Ii[1:5])
  
  expect_equal(round(package_fisherTI, 3), round(catR_fisherTI, 3))
})

test_that("fisherTestInfo calculates correctly", {
  gpcm_cat@answers[1:5] <- c(5, 4, 1, 1, 1)
  package_fisherTI <- fisherTestInfo(gpcm_cat, 2)
  catR_fisherTI <- sum(Ii(th = 2, it = it_gpcm, model = "GPCM")$Ii[1:5])
  
  expect_equal(package_fisherTI, catR_fisherTI)
})


