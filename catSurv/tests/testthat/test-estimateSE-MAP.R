library(catR)
library(ltm)
context("estimateSE-MAP")
load("cat_objects.Rdata")

test_that("ltm MAP theta standard error calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_se <- estimateSE(ltm_cat)
  catR_se <- semTheta(thEst = estimateTheta(ltm_cat), it = it_ltm,
                      x = ltm_cat@answers, method = "BM")
  ltm_se <- factor.scores(ltm_fit, matrix(ltm_cat@answers, nrow = 1),
                           method = "EB")$score.dat$se.z1

  expect_equal(package_se, catR_se)
  expect_equal(round(package_se, 6), round(ltm_se, 6))
})

test_that("grm MAP theta standard error calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(grm_cat)
  catR_se <- semTheta(thEst = estimateTheta(grm_cat), it = it_grm,
                       x = c(grm_cat@answers) - 1, method = "BM", model = "GRM")

  expect_equal(round(package_se, 4), round(catR_se, 4))
})

test_that("gpcm MAP theta standard error calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(gpcm_cat)
  catR_se <- semTheta(thEst = estimateTheta(gpcm_cat), it = it_gpcm,
                       x = c(gpcm_cat@answers)-1, model = "GPCM", method = "BM")
  ltm_se <- factor.scores(gpcm_fit, matrix(gpcm_cat@answers, nrow = 1),
                           method = "EB")$score.dat$se.z1

  expect_equal(round(package_se, 6), round(catR_se, 6))
  expect_equal(round(package_se, 6), round(ltm_se, 6))
})
