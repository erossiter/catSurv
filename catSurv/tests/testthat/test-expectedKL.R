detach("package:catR", unload = TRUE)
library(catIrt)
context("expectedKL")
load("cat_objects.Rdata")

test_that("ltm expectedKL calculates correctly", {
  # ltm_cat@answers[1:5] <- c(0, 1, 1, 0, 0)
  # #delta <- ltm_cat@z * fisherTestInfo(ltm_cat)^2
  # package_kl <- expectedKL(ltm_cat, item = 9)
  # catIrt_kl <- KL.brm(it_ltm[,1:3], estimateTheta(ltm_cat), .1)$item[9]
  # 
  # expect_equal(round(package_epv, 5), round(catR_epv, 5))
})

test_that("grm expectedKL calculates correctly", {
  # grm_cat@answers[1:9] <- c(2, 1, 4, 1, 5, 3, 5, 3, 4)
  # package_kl <- expectedKL(grm_cat, item = 2)
  # catR_kl <- KL(it_grm, estimateTheta(grm_cat), delta = .1)
  # 
  # expect_equal(round(package_epv, 4), round(catR_epv, 4))
})

test_that("gpcm expectedKL calculates correctly", {
  # gpcm_cat@estimation <- "EAP"
  # gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  # package_epv <- expectedKL(gpcm_cat, item = 6)
  # catR_epv <- EPV(itemBank = it_gpcm, item = 6, x = c(gpcm_cat@answers[1:5]) - 1,
  #                 model = "GPCM", theta = estimateTheta(gpcm_cat), it.given = it_gpcm[1:5, ])
  # 
  # expect_equal(round(package_epv, 4), round(catR_epv, 4))
})
