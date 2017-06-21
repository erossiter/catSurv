library(catR)
context("estimateSE-WLE")
load("cat_objects.Rdata")

test_that("ltm WLE theta standard error calculates correctly", {
  ltm_cat@estimation <- "WLE"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_se <- estimateSE(ltm_cat)
  catR_se <- semTheta(thEst = estimateTheta(ltm_cat), it = it_ltm,
                      x = ltm_cat@answers, method = "WL")

  expect_equal(package_se, catR_se)
})

test_that("grm WLE theta standard error calculates correctly", {
  grm_cat@estimation <- "WLE"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(grm_cat)
  catR_se <- semTheta(thEst = estimateTheta(grm_cat), it = it_grm,
                       x = c(grm_cat@answers) - 1, method = "WL", model = "GRM")

  expect_equal(round(package_se, 4), round(catR_se, 4))
})

test_that("gpcm WLE theta standard error calculates correctly", {
  gpcm_cat@estimation <- "WLE"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(gpcm_cat)
  catR_se <- semTheta(thEst = estimateTheta(gpcm_cat), it = it_gpcm,
                       x = c(gpcm_cat@answers)-1, model = "GPCM", method = "WL")

  expect_equal(round(package_se, 6), round(catR_se, 6))
})
