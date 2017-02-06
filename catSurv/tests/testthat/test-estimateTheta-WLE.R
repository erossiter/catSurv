context("estimateTheta-WLE")
load("cat_objects.Rdata")


test_that("ltm WLE estimation calculates correctly", {
  ltm_cat@estimation <- "WLE"
  ltm_cat@answers <- c(0, 1, 0, 0, 1)
  package_wle <- estimateTheta(ltm_cat)
  catR_wle <- thetaEst(it_ltm, x = ltm_cat@answers, method = "WL")
    
  expect_equal(round(package_wle, 5), round(catR_wle, 4))
})

test_that("grm WLE estimation calculates correctly", {
  grm_cat@estimation <- "WLE"
  grm_cat@answers <- c(4, 5, 2, 4, 4)
  package_wle <- estimateTheta(grm_cat)
  catR_wle <- thetaEst(it_grm, x = c(grm_cat@answers)-1, model = "GRM",
                       method = "WL")

  expect_equal(round(package_wle, 3), round(catR_wle, 3))
})

test_that("gpcm WLE estimation calculates correctly", {
  gpcm_cat@estimation <- "WLE"
  gpcm_cat@answers <- c(4, 5, 2, 4, 4)
  package_wle <- estimateTheta(gpcm_cat)
  catR_map <- thetaEst(it_gpcm, x = c(gpcm_cat@answers)-1, model = "GPCM",
                       method = "WL")
    
  expect_equal(round(package_wle, 4), round(catR_map, 4))
})

test_that("WLE with all extreme answers defaults correctly", {
  grm_cat@answers <- gpcm_cat@answers <- c(1, 1, 5, 1, 1)
  grm_cat@estimation <- "WLE"
  gpcm_cat@estimation <- "WLE"
  package_wle_grm <- estimateTheta(grm_cat)
  package_wle_gpcm <- estimateTheta(gpcm_cat)
  
  grm_cat@estimationDefault <- "MAP"
  gpcm_cat@estimationDefault <- "MAP"
  package_map_grm <- estimateTheta(grm_cat)
  package_map_gpcm <- estimateTheta(gpcm_cat)

  expect_equal(package_wle_grm, package_map_grm)
  expect_equal(package_wle_gpcm, package_map_gpcm)
})
