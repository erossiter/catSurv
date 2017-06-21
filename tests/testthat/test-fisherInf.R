library(catR, quietly = TRUE)
context("fisherInf")
load("cat_objects.Rdata")

test_that("fisherInf calculates correctly", {
  package_fisherInf <- fisherInf(ltm_cat, theta = 1, item = 6)
  catR_fisherInf <- Ii(th = 1, it = it_ltm)$Ii[6]
    
  expect_equal(package_fisherInf, catR_fisherInf)
})

test_that("fisherInf calculates correctly", {
  package_fisherInf <- fisherInf(grm_cat, theta = 1, item = 6)
  catR_fisherInf <- Ii(th = 1, it = it_grm, model = "GRM")$Ii[6]
  
  expect_equal(round(package_fisherInf, 5), round(catR_fisherInf, 5))
})

test_that("fisherInf calculates correctly", {
  package_fisherInf <- fisherInf(gpcm_cat, theta = 1, item = 6)
  catR_fisherInf <- Ii(th = 1, it = it_gpcm, model = "GPCM")$Ii[6]
  
  expect_equal(package_fisherInf, catR_fisherInf)
})


