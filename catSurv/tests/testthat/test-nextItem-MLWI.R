library(catR)
context("nextItem-MLWI")
load("cat_objects.Rdata")

test_that("ltm nextItem MLWI calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@selection <- "MLWI"
  ltm_cat@answers[1:30] <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0,
                             1, 0, 1, 1, 0, 0, 1, 1, 1, 0,
                             1, 0, 1, 1, 0, 0, 1, 1, 1, 0)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MLWI"]
  
  catR_next <- nextItem(itemBank = it_ltm, theta = estimateTheta(ltm_cat),
                        x = ltm_cat@answers[1:30], criterion = "MLWI", out = 1:30,
                        method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, catR_est)
})

test_that("grm nextItem MLWI calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@selection <- "MLWI"
  grm_cat@answers[1:18] <- c(4, 5, 2, 4, 4, 1, 2, 2, 1, 3,
                             2, 4, 4, 1, 2, 2, NA, NA)
  ## START HERE:
  ## go into grmCat and gpcmCat and change default
  ## ranges to -5, 5
  ## think about if the same rounding problems will occur
  ## for GPCM, and add error to probability

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MLWI"]

  catR_next <- nextItem(itemBank = it_grm, theta = estimateTheta(grm_cat),
                       x = grm_cat@answers[1:10]-1, criterion = "MLWI",
                       model = "GRM", out = 1:10, method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(grm_cat, estimateTheta(grm_cat), package_item))
  expect_equal(round(package_est, 3), round(catR_est, 3))
})

test_that("gpcm nextItem MLWI calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@selection <- "MLWI"
  gpcm_cat@answers[1:8] <- c(4, 5, 2, 4, 4, 1, 1, 3)
  
  package_next <- selectItem(gpcm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MLWI"]

  catR_next <- nextItem(itemBank = it_gpcm, theta = estimateTheta(gpcm_cat),
                       x = gpcm_cat@answers[1:8]-1, criterion = "MLWI",
                       model = "GPCM", out = 1:8, method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(gpcm_cat, estimateTheta(gpcm_cat), package_item))
  expect_equal(round(package_est, 7), round(catR_est, 7))
})


test_that("nextItem MLWI chooses item (not NA) when no questions asked", {
  ltm_cat@selection <- "MLWI"
  grm_cat@selection <- "MLWI"
  gpcm_cat@selection <- "MLWI"

  expect_true(!is.na(selectItem(ltm_cat)$next_item))
  expect_true(!is.na(selectItem(grm_cat)$next_item))
  expect_true(!is.na(selectItem(gpcm_cat)$next_item))
})

test_that("nextItem MLWI estimates are not NA (when no questions asked)", {
  ltm_cat@selection <- "MLWI"
  grm_cat@selection <- "MLWI"
  gpcm_cat@selection <- "MLWI"

  expect_equal(sum(!is.na(selectItem(ltm_cat)$estimates[,"MLWI"])), 40)
  expect_equal(sum(!is.na(selectItem(grm_cat)$estimates[,"MLWI"])), 18)
  expect_equal(sum(!is.na(selectItem(gpcm_cat)$estimates[,"MLWI"])), 18)
})

test_that("nextItem MLWI is actually the maximum estimate", {
  ltm_cat@selection <- "MLWI"
  grm_cat@selection <- "MLWI"
  gpcm_cat@selection <- "MLWI"
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)
  
  expect_equal(ltm_next$next_item, which(ltm_next$estimates[, "MLWI"] ==
                                        max(ltm_next$estimates[, "MLWI"])))
  expect_equal(grm_next$next_item, which(grm_next$estimates[, "MLWI"] ==
                                        max(grm_next$estimates[, "MLWI"])))
  expect_equal(gpcm_next$next_item, which(gpcm_next$estimates[, "MLWI"] ==
                                        max(gpcm_next$estimates[, "MLWI"])))
})
