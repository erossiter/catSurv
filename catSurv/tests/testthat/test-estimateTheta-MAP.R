library(catR, quietly = TRUE)
context("estimateTheta-MAP")
load("cat_objects.Rdata")


test_that("ltm MAP estimation calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_map <- estimateTheta(ltm_cat)
  catR_map <- thetaEst(it_ltm, x = ltm_cat@answers, method = "BM")
  ltm_map <- factor.scores(ltm_fit, matrix(ltm_cat@answers, nrow = 1),
                           method = "EB")$score.dat$z1
    
  expect_equal(round(package_map, 4), round(catR_map, 4))
  expect_equal(round(package_map, 5), round(ltm_map, 5))
})

test_that("grm MAP estimation calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_map <- estimateTheta(grm_cat)
  catR_map <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "BM")
  ltm_map <- factor.scores(grm_fit, matrix(grm_cat@answers, nrow = 1),
                           method = "EB")$score.dat$z1

  expect_equal(round(package_map, 3), round(catR_map, 3))
  expect_equal(round(package_map, 3), round(ltm_map, 3))
})

test_that("gpcm MAP estimation calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_map <- estimateTheta(gpcm_cat)
  catR_map <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "BM")
  ltm_map <- factor.scores(gpcm_fit, matrix(gpcm_cat@answers, nrow = 1),
                           method = "EB")$score.dat$z1
    
  expect_equal(round(package_map, 4), round(catR_map, 4))
  expect_equal(round(package_map, 5), round(ltm_map, 5))
})

test_that("estimatTheta with MAP works when questions skipped", {
  ltm_cat@estimation <- "MAP"
  grm_cat@estimation <- "MAP"
  gpcm_cat@estimation <- "MAP"

  ltm_cat@answers[1:10] <- c(rep(-1, 5), 1, 1, 0, 0, 1)
  grm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  gpcm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  
  catR_ltm <- thetaEst(it_ltm, method = "BM",
                       x = c(NA, NA, NA, NA, NA, 1, 1, 0, 0, 1))
  catR_grm <- thetaEst(it_grm, model = "GRM", method = "BM",
                       x = c(NA, NA, 5, 4, 3)-1)
  catR_gpcm <- thetaEst(it_gpcm, model = "GPCM", method = "BM",
                       x = c(NA, NA, 5, 4, 3)-1)
  
  expect_equal(round(estimateTheta(ltm_cat), 4), round(catR_ltm, 4))
  expect_equal(round(estimateTheta(grm_cat), 3), round(catR_grm, 3))
  expect_equal(round(estimateTheta(gpcm_cat), 6), round(catR_gpcm, 6))
})
