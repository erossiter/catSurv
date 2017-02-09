library(catR)
context("nextItem-MFI")
load("cat_objects.Rdata")

test_that("ltm nextItem MFI calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@selection <- "MFI"
  ltm_cat@answers[1:10] <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFI"]
  
  catR_next <- nextItem(itemBank = it_ltm, theta = estimateTheta(ltm_cat),
                        x = ltm_cat@answers[1:10], criterion = "MFI")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(ltm_cat, estimateTheta(ltm_cat), package_item))
  expect_equal(round(package_est, 5), round(catR_est, 5))
})

test_that("grm nextItem MFI calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@selection <- "MFI"
  grm_cat@answers[1:10] <- c(4, 5, 2, 4, 4, 1, 2, 2, 1, 3)

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFI"]

  catR_next <- nextItem(itemBank = it_grm, theta = estimateTheta(grm_cat),
                       x = grm_cat@answers[1:10]-1, criterion = "MFI",
                       model = "GRM", out = 1:10)
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(grm_cat, estimateTheta(grm_cat), package_item))
  expect_equal(round(package_est, 4), round(catR_est, 4))
})

test_that("gpcm nextItem MFI calculates correctly", {
  gpcm_cat@estimation <- "EAP"
  gpcm_cat@selection <- "MFI"
  gpcm_cat@answers[1:8] <- c(4, 5, 2, 4, 4, 1, 1, 3)

  package_next <- selectItem(gpcm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFI"]

  catR_next <- nextItem(itemBank = it_gpcm, theta = estimateTheta(gpcm_cat),
                       x = gpcm_cat@answers[1:8]-1, criterion = "MFI",
                       model = "GPCM", out = 1:8)
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(gpcm_cat, estimateTheta(gpcm_cat), package_item))
  expect_equal(round(package_est, 7), round(catR_est, 7))
})


test_that("nextItem EPV has correct output when no questions asked", {
  ltm_cat@selection <- "MFI"
  grm_cat@selection <- "MFI"
  gpcm_cat@selection <- "MFI"
  ltm_cat@answers <- rep(NA, length(ltm_cat@answers))
  grm_cat@answers <- rep(NA, length(grm_cat@answers))
  gpcm_cat@answers <- rep(NA, length(gpcm_cat@answers))
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)

  expect_is(ltm_next, "list")
  expect_is(grm_next, "list")
  expect_is(gpcm_next, "list")
  expect_true(!is.na(ltm_next$next_item))
  expect_true(!is.na(grm_next$next_item))
  expect_true(!is.na(gpcm_next$next_item))
})

test_that("nextItem MFI is actually the maximum estimate", {
  ltm_cat@selection <- "MFI"
  grm_cat@selection <- "MFI"
  gpcm_cat@selection <- "MFI"
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)
  
  expect_equal(ltm_next$next_item, which(ltm_next$estimates[, "MFI"] ==
                                        max(ltm_next$estimates[, "MFI"])))
  expect_equal(grm_next$next_item, which(grm_next$estimates[, "MFI"] ==
                                        max(grm_next$estimates[, "MFI"])))
  expect_equal(gpcm_next$next_item, which(gpcm_next$estimates[, "MFI"] ==
                                        max(gpcm_next$estimates[, "MFI"])))
})
