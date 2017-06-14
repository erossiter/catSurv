library(catR, quietly = TRUE)
library(ltm, quietly = TRUE)
context("estimateSE-EAP")
load("cat_objects.Rdata")

test_that("ltm EAP theta standard error calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_se <- estimateSE(ltm_cat)

  catR_se <- semTheta(thEst = estimateTheta(ltm_cat), it = it_ltm,
                      x = ltm_cat@answers, method = "EAP")

  ltm_se <- factor.scores(ltm_fit, matrix(ltm_cat@answers, nrow = 1),
                           method = "EAP")$score.dat$se.z1

  expect_equal(round(package_se, 4), round(catR_se), 4)
  expect_equal(round(package_se, 4), round(ltm_se), 4)
})

test_that("grm EAP theta standard error calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(grm_cat)
  catR_se <- semTheta(thEst = estimateTheta(grm_cat), it = it_grm,
                       x = c(grm_cat@answers) - 1, method = "EAP", model = "GRM")
  ltm_se <- factor.scores(grm_fit, matrix(grm_cat@answers, nrow = 1),
                           method = "EAP")$score.dat$se.z1

  expect_equal(round(package_se, 2), round(catR_se, 2))
  expect_equal(round(package_se, 5), round(ltm_se, 5))
})

test_that("gpcm EAP theta standard error calculates correctly", {
  gpcm_cat@estimation <- "EAP"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_se <- estimateSE(gpcm_cat)
  catR_se <- semTheta(thEst = estimateTheta(gpcm_cat), it = it_gpcm,
                       x = c(gpcm_cat@answers)-1, model = "GPCM", method = "EAP")
  ltm_se <- factor.scores(gpcm_fit, matrix(gpcm_cat@answers, nrow = 1),
                           method = "EAP")$score.dat$se.z1

  expect_equal(round(package_se, 2), round(catR_se, 2))
  expect_equal(round(package_se, 2), round(ltm_se, 2))
})
