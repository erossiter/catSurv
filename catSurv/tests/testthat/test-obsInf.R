library(catR, quietly = TRUE)
context("obsInf")
load("cat_objects.Rdata")

test_that("obsInf calculates correctly", {
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_obsInf <- obsInf(ltm_cat, theta = 1, item = 5)
  catR_obsInf <- OIi(th = 1, it = it_ltm, x = ltm_cat@answers[1:5])[5]
    
  expect_equal(package_obsInf, catR_obsInf)
})

test_that("obsInf calculates correctly", {
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_obsInf <- obsInf(grm_cat, theta = 1, item = 5)
  catR_obsInf <- OIi(th = 1, it = it_grm, x = grm_cat@answers[1:5] - 1,
                     model = "GRM")[5]
  
  expect_equal(round(package_obsInf, 3), round(catR_obsInf, 3))
})

test_that("obsInf calculates correctly", {
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_obsInf <- obsInf(gpcm_cat, theta = 1, item = 5)
  catR_obsInf <- OIi(th = 1, it = it_gpcm, x = gpcm_cat@answers[1:5] - 1,
                     model = "GPCM")[5]
  
  expect_equal(package_obsInf, catR_obsInf)
})


test_that("obsInf throws error when no questions asked", {
  ltm_cat@answers[1:5] <- rep(NA, 5)
  expect_error(obsInf(ltm_cat, theta = 1, item = 1))
  
  grm_cat@answers[1:5] <- rep(NA, 5)
  expect_error(obsInf(grm_cat, theta = 1, item = 1))
  
  gpcm_cat@answers[1:5] <- rep(NA, 5)
  expect_error(obsInf(gpcm_cat, theta = 1, item = 1))
})


