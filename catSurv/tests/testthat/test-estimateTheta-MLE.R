context("estimateTheta-MLE")
load("cat_objects.Rdata")


test_that("ltm MLE estimation calculates correctly", {
  ltm_cat@estimation <- "MLE"
  ltm_cat@answers <- c(0, 1, 0, 0, 1)
  package_mle <- estimateTheta(ltm_cat)
  catR_mle <- thetaEst(it_ltm, x = ltm_cat@answers, method = "ML")
    
  expect_equal(round(package_mle, 5), round(catR_mle), 5)
})

test_that("grm MLE estimation calculates correctly", {
  grm_cat@estimation <- "MLE"
  grm_cat@answers <- c(4, 5, 2, 4, 4)
  package_mle <- estimateTheta(grm_cat)
  catR_mle <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "ML")

  expect_equal(round(package_mle, 3), round(catR_mle), 3)
})

test_that("gpcm MLE estimation calculates correctly", {
  gpcm_cat@estimation <- "MLE"
  gpcm_cat@answers <- c(4, 5, 2, 4, 4)
  package_mle <- estimateTheta(gpcm_cat)
  catR_mle <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "ML")
    
  expect_equal(round(package_mle, 4), round(catR_mle), 4)
})

test_that("MLE with all extreme answers defaults correctly", {
  grm_cat@answers <- c(1, 1, 5, 1, 1)
  grm_cat@estimation <- "MLE"
  package_mle <- estimateTheta(grm_cat)
  grm_cat@estimationDefault <- "MAP"
  package_map <- estimateTheta(grm_cat)

  expect_equal(package_mle, package_map)
})
