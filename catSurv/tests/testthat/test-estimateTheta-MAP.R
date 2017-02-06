context("estimateTheta-MAP")
load("cat_objects.Rdata")


test_that("ltm MAP estimation calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@answers <- c(0, 1, 0, 0, 1)
  package_map <- estimateTheta(ltm_cat)
  catR_map <- thetaEst(it_ltm, x = ltm_cat@answers, method = "BM")
  ltm_map <- factor.scores(ltm_fit, matrix(c(ltm_cat@answers, rep(NA, 35)), nrow = 1),
                           method = "EB")$score.dat$z1
    
  expect_equal(package_map, catR_map)
  expect_equal(package_map, round(ltm_map), 5)
})

test_that("grm MAP estimation calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@answers <- c(4, 5, 2, 4, 4)
  package_map <- estimateTheta(grm_cat)
  catR_map <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "BM")
  ltm_map <- factor.scores(grm_fit, matrix(c(grm_cat@answers, rep(NA, 13)), nrow = 1),
                           method = "EB")$score.dat$z1

  expect_equal(round(package_map, 3), round(catR_map), 3)
  expect_equal(round(package_map, 3), round(ltm_map), 3)
})

test_that("gpcm MAP estimation calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@answers <- c(4, 5, 2, 4, 4)
  package_map <- estimateTheta(gpcm_cat)
  catR_map <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "BM")
  ltm_map <- factor.scores(gpcm_fit, matrix(c(gpcm_cat@answers, rep(NA, 13)), nrow = 1),
                           method = "EB")$score.dat$z1
    
  expect_equal(round(package_map, 4), round(catR_map), 4)
  expect_equal(round(package_map, 5), round(ltm_map), 5)
})
