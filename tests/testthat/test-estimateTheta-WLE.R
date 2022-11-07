library(catR, quietly = TRUE)
context("estimateTheta-WLE")
load("cat_objects.Rdata")


test_that("ltm WLE estimation calculates correctly", {
  ltm_cat@estimation <- "WLE"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_wle <- estimateTheta(ltm_cat)
  catR_wle <- thetaEst(it_ltm, x = ltm_cat@answers, method = "WL")

  expect_equal(round(package_wle, 4), round(catR_wle, 4))
})

test_that("grm WLE estimation calculates correctly", {
  grm_cat@estimation <- "WLE"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_wle <- estimateTheta(grm_cat)
  catR_wle <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "WL")

  expect_equal(round(package_wle, 3), round(catR_wle, 3))
})

test_that("gpcm WLE estimation calculates correctly", {
  gpcm_cat@estimation <- "WLE"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_wle <- estimateTheta(gpcm_cat)
  catR_map <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "WL")

  expect_equal(round(package_wle, 4), round(catR_map, 4))
})

test_that("WLE (grm) with all extreme answers defaults correctly", {
  grm_cat@answers[1:5] <- c(1, 1, 5, 1, 1)
  grm_cat@estimation <- "WLE"
  grm_cat@estimationDefault <- "MAP"
  package_wle_grm <- expect_warning(estimateTheta(grm_cat))

  grm_cat@estimation <- "MAP"
  package_map_grm <- estimateTheta(grm_cat)

  expect_equal(package_wle_grm, package_map_grm)
})

test_that("WLE (grm) with all extreme answers defaults correctly", {
    gpcm_cat@answers[1:5] <- c(1, 1, 1, 1, 1)
    gpcm_cat@estimation <- "WLE"
    gpcm_cat@estimationDefault <- "MAP"
    package_wle_gpcm <- expect_warning(estimateTheta(gpcm_cat))
    
    gpcm_cat@estimation <- "MAP"
    package_map_gpcm <- estimateTheta(gpcm_cat)
    
    expect_equal(package_wle_gpcm, package_map_gpcm)
})

test_that("estimatTheta with WLE works when questions skipped", {
  ltm_cat@estimation <- "WLE"
  grm_cat@estimation <- "WLE"
  gpcm_cat@estimation <- "WLE"

  ltm_cat@answers[1:10] <- c(rep(-1, 5), 1, 1, 0, 0, 1)
  grm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  gpcm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)

  catR_ltm <- thetaEst(it_ltm, method = "WL",
                       x = c(NA, NA, NA, NA, NA, 1, 1, 0, 0, 1))
  catR_grm <- thetaEst(it_grm, model = "GRM", method = "WL",
                       x = c(NA, NA, 5, 4, 3)-1)
  catR_gpcm <- thetaEst(it_gpcm, model = "GPCM", method = "WL",
                       x = c(NA, NA, 5, 4, 3)-1)

  expect_equal(round(estimateTheta(ltm_cat), 3), round(catR_ltm, 3))
  expect_equal(round(estimateTheta(grm_cat), 2), round(catR_grm, 2))
  expect_equal(round(estimateTheta(gpcm_cat), 4), round(catR_gpcm, 4))
})
