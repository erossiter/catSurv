library(catR, quietly = TRUE)
context("estimateTheta-MLE")
load("cat_objects.Rdata")


test_that("ltm MLE estimation calculates correctly", {
  ltm_cat@estimation <- "MLE"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_mle <- estimateTheta(ltm_cat)
  catR_mle <- thetaEst(it_ltm, x = ltm_cat@answers, method = "ML")
    
  expect_equal(round(package_mle, 4), round(catR_mle, 4))
})

test_that("grm MLE estimation calculates correctly", {
  grm_cat@estimation <- "MLE"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_mle <- estimateTheta(grm_cat)
  catR_mle <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "ML")

  expect_equal(round(package_mle, 3), round(catR_mle, 3))
})

test_that("gpcm MLE estimation calculates correctly", {
  gpcm_cat@estimation <- "MLE"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_mle <- estimateTheta(gpcm_cat)
  catR_mle <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "ML")
    
  expect_equal(round(package_mle, 4), round(catR_mle, 4))
})


test_that("estimatTheta with MLE works when questions skipped", {
  ltm_cat@estimation <- "MLE"
  grm_cat@estimation <- "MLE"
  gpcm_cat@estimation <- "MLE"

  ltm_cat@answers[1:10] <- c(rep(-1, 5), 1, 1, 0, 0, 1)
  grm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  gpcm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  
  catR_ltm <- thetaEst(it_ltm, method = "ML",
                       x = c(NA, NA, NA, NA, NA, 1, 1, 0, 0, 1))
  catR_grm <- thetaEst(it_grm, model = "GRM", method = "ML",
                       x = c(NA, NA, 5, 4, 3)-1)
  catR_gpcm <- thetaEst(it_gpcm, model = "GPCM", method = "ML",
                       x = c(NA, NA, 5, 4, 3)-1)
  
  expect_equal(round(estimateTheta(ltm_cat), 5), round(catR_ltm, 5))
  expect_equal(round(estimateTheta(grm_cat), 3), round(catR_grm, 3))
  expect_equal(round(estimateTheta(gpcm_cat), 4), round(catR_gpcm, 4))
})
