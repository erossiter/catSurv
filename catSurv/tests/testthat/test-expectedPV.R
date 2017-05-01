library(catR, quietly = TRUE)
context("expectedPV")
load("cat_objects.Rdata")

test_that("ltm expectedPV calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_epv <- expectedPV(ltm_cat, item = 6)
  catR_epv <- EPV(itemBank = it_ltm, item = 6, x = ltm_cat@answers[1:5],
                  theta = estimateTheta(ltm_cat), it.given = it_ltm[1:5, ])
  
  expect_equal(round(package_epv, 5), round(catR_epv, 5))
})

test_that("grm expectedPV calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_epv <- expectedPV(grm_cat, item = 6)
  catR_epv <- EPV(itemBank = it_grm, item = 6, x = c(grm_cat@answers[1:5]) - 1,
                  model = "GRM", theta = estimateTheta(grm_cat), it.given = it_grm[1:5, ])
  
  expect_equal(round(package_epv, 3), round(catR_epv, 3))
})

test_that("gpcm expectedPV calculates correctly", {
  gpcm_cat@estimation <- "EAP"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_epv <- expectedPV(gpcm_cat, item = 6)
  catR_epv <- EPV(itemBank = it_gpcm, item = 6, x = c(gpcm_cat@answers[1:5]) - 1,
                  model = "GPCM", theta = estimateTheta(gpcm_cat), it.given = it_gpcm[1:5, ])
  
  expect_equal(round(package_epv, 2), round(catR_epv, 2))
})
