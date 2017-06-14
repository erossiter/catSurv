library(catR)
context("estimateSE-MLE")
load("cat_objects.Rdata")

test_that("ltm MLE theta standard error calculates correctly", {
  ltm_cat@estimation <- "MLE"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_se <- estimateSE(ltm_cat)
  catR_se <- semTheta(thEst = estimateTheta(ltm_cat), it = it_ltm,
                      x = ltm_cat@answers, method = "ML")

  expect_equal(package_se, catR_se)
})

test_that("grm MLE theta standard error calculates correctly", {
  grm_cat@estimation <- "MLE"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(grm_cat)
  catR_se <- semTheta(thEst = estimateTheta(grm_cat), it = it_grm,
                       x = c(grm_cat@answers) - 1, method = "ML", model = "GRM")

  expect_equal(round(package_se, 3), round(catR_se, 3))
})

test_that("gpcm MLE theta standard error calculates correctly", {
  gpcm_cat@estimation <- "MLE"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(gpcm_cat)
  catR_se <- semTheta(thEst = estimateTheta(gpcm_cat), it = it_gpcm,
                       x = c(gpcm_cat@answers)-1, model = "GPCM", method = "ML")

  expect_equal(round(package_se, 6), round(catR_se, 6))
})
