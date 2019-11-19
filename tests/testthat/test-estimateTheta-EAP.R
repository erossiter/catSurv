library(catR, quietly = TRUE)
context("estimateTheta-EAP")
load("cat_objects.Rdata")

test_that("ltm EAP estimation calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_eap <- estimateTheta(ltm_cat)
  catR_eap <- thetaEst(it_ltm, x = ltm_cat@answers, method = "EAP")
  ltm_eap <- factor.scores(ltm_fit, matrix(ltm_cat@answers, nrow = 1),
                           method = "EAP")$score.dat$z1

  expect_equal(round(package_eap, 5), round(catR_eap, 5))
  expect_equal(package_eap, ltm_eap)
})

test_that("grm EAP estimation calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_eap <- estimateTheta(grm_cat)
  catR_eap <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "EAP")
  ltm_eap <- factor.scores(grm_fit, matrix(grm_cat@answers, nrow = 1),
                           method = "EAP")$score.dat$z1

  expect_equal(round(package_eap, 3), round(catR_eap, 3))
  expect_equal(round(package_eap, 5), round(ltm_eap, 5))
})

test_that("gpcm EAP estimation calculates correctly", {
  gpcm_cat@estimation <- "EAP"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_eap <- estimateTheta(gpcm_cat)
  catR_eap <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "EAP")
  ltm_eap <- factor.scores(gpcm_fit, matrix(gpcm_cat@answers, nrow = 1),
                           method = "EAP")$score.dat$z1

  expect_equal(round(package_eap, 3), round(catR_eap, 3))
  expect_equal(round(package_eap, 3), round(ltm_eap, 3))
})

test_that("estimatTheta with EAP works when questions skipped", {
  ltm_cat@answers[1:10] <- c(rep(-1, 5), 1, 1, 0, 0, 1)
  grm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  gpcm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)

  catR_ltm <- thetaEst(it_ltm, method = "EAP",
                       x = c(NA, NA, NA, NA, NA, 1, 1, 0, 0, 1))
  catR_grm <- thetaEst(it_grm, model = "GRM", method = "EAP",
                       x = c(NA, NA, 5, 4, 3)-1)
  catR_gpcm <- thetaEst(it_gpcm, model = "GPCM", method = "EAP",
                       x = c(NA, NA, 5, 4, 3)-1)

  expect_equal(round(estimateTheta(ltm_cat), 4), round(catR_ltm, 4))
  expect_equal(round(estimateTheta(grm_cat), 3), round(catR_grm, 3))
  expect_equal(round(estimateTheta(gpcm_cat), 2), round(catR_gpcm, 2))
})

