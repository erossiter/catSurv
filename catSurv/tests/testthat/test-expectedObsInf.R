library(catR, quietly = TRUE)
context("expectedObsInf")
load("cat_objects.Rdata")

test_that("ltm expectedObsInf calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_mei <- expectedObsInf(ltm_cat, item = 6)
  catR_mei <- MEI(itemBank = it_ltm, item = 6, theta = estimateTheta(ltm_cat),
                  x = ltm_cat@answers[1:5], method = "BM", it.given = it_ltm[1:5, ])
    
  expect_equal(round(package_mei, 5), round(catR_mei, 5))
})

test_that("grm expectedObsInf calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_mei <- expectedObsInf(grm_cat, item = 6)
  catR_mei <- MEI(itemBank = it_grm, item = 6, theta = estimateTheta(grm_cat),
                  x = grm_cat@answers[1:5] - 1, method = "BM", model = "GRM",
                  it.given = it_grm[1:5, ])

  expect_equal(round(package_mei, 4), round(catR_mei, 4))
})

test_that("gpcm expectedObsInf calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_mei <- expectedObsInf(gpcm_cat, item = 6)
  catR_mei <- MEI(itemBank = it_gpcm, item = 6, theta = estimateTheta(gpcm_cat),
                  x = gpcm_cat@answers[1:5] - 1, method = "BM", model = "GPCM",
                  it.given = it_gpcm[1:5, ])
  
  expect_equal(round(package_mei, 4), round(catR_mei, 4))
})
