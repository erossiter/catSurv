library(catR)
library(ltm)
context("estimateTheta-EAP")
load("cat_objects.Rdata")

test_that("ltm EAP estimation calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@answers <- c(0, 1, 0, 0, 1)
  package_eap <- estimateTheta(ltm_cat)
  catR_eap <- thetaEst(it_ltm, x = ltm_cat@answers, method = "EAP")
  ltm_eap <- factor.scores(ltm_fit, matrix(c(ltm_cat@answers, rep(NA, 35)), nrow = 1),
                           method = "EAP")$score.dat$z1
    
  expect_equal(round(package_eap, 5), round(catR_eap, 5))
  expect_equal(package_eap, ltm_eap)
})

test_that("grm EAP estimation calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@answers <- c(4, 5, 2, 4, 4)
  package_eap <- estimateTheta(grm_cat)
  catR_eap <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "EAP")
  ltm_eap <- factor.scores(grm_fit, matrix(c(grm_cat@answers, rep(NA, 13)), nrow = 1),
                           method = "EAP")$score.dat$z1

  expect_equal(round(package_eap, 3), round(catR_eap, 3))
  expect_equal(package_eap, ltm_eap)
})

test_that("gpcm EAP estimation calculates correctly", {
  gpcm_cat@estimation <- "EAP"
  gpcm_cat@answers <- c(4, 5, 2, 4, 4)
  package_eap <- estimateTheta(gpcm_cat)
  catR_eap <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "EAP")
  ltm_eap <- factor.scores(gpcm_fit, matrix(c(gpcm_cat@answers, rep(NA, 13)), nrow = 1),
                           method = "EAP")$score.dat$z1
    
  expect_equal(round(package_eap, 5), round(catR_eap, 5))
  expect_equal(round(package_eap, 5), round(ltm_eap, 5))
})
