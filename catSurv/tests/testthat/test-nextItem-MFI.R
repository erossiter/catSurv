library(catR, quietly = TRUE)
context("nextItem-MFI")
load("cat_objects.Rdata")

test_that("ltm nextItem MFI calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@selection <- "MFI"
  ltm_cat@answers[1:10] <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFI"]
  
  catR_next <- nextItem(itemBank = it_ltm, theta = estimateTheta(ltm_cat),
                        x = ltm_cat@answers[1:10], criterion = "MFI", out = 1:10,
                        method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(ltm_cat, estimateTheta(ltm_cat), package_item))
  expect_equal(round(package_est, 5), round(catR_est, 5))
})

test_that("grm nextItem MFI calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@selection <- "MFI"
  grm_cat@answers[1:10] <- c(4, 5, 2, 4, 4, 1, 2, 2, 1, 3)

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFI"]

  catR_next <- nextItem(itemBank = it_grm, theta = estimateTheta(grm_cat),
                       x = grm_cat@answers[1:10]-1, criterion = "MFI",
                       model = "GRM", out = 1:10, method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(grm_cat, estimateTheta(grm_cat), package_item))
  expect_equal(round(package_est, 3), round(catR_est, 3))
})

test_that("gpcm nextItem MFI calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@selection <- "MFI"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)

  package_next <- selectItem(gpcm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFI"]

  catR_next <- nextItem(itemBank = it_gpcm, theta = estimateTheta(gpcm_cat),
                       x = gpcm_cat@answers[1:5]-1, criterion = "MFI",
                       model = "GPCM", out = 1:5, method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, fisherInf(gpcm_cat, estimateTheta(gpcm_cat), package_item))
  expect_equal(round(package_est, 7), round(catR_est, 7))
})


test_that("nextItem MFI chooses item (not NA) when no questions asked", {
  ltm_cat@selection <- "MFI"
  grm_cat@selection <- "MFI"
  gpcm_cat@selection <- "MFI"

  expect_true(!is.na(selectItem(ltm_cat)$next_item))
  expect_true(!is.na(selectItem(grm_cat)$next_item))
  expect_true(!is.na(selectItem(gpcm_cat)$next_item))
})

test_that("nextItem MFI estimates are not NA (when no questions asked)", {
  ltm_cat@selection <- "MFI"
  grm_cat@selection <- "MFI"
  gpcm_cat@selection <- "MFI"

  expect_equal(sum(!is.na(selectItem(ltm_cat)$estimates[,"MFI"])), 40)
  expect_equal(sum(!is.na(selectItem(grm_cat)$estimates[,"MFI"])), 18)
  expect_equal(sum(!is.na(selectItem(gpcm_cat)$estimates[,"MFI"])), 10)
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

test_that("nextItem MFI correctly skips questions", {
  ltm_cat@selection <- "MFI"
  grm_cat@selection <- "MFI"
  gpcm_cat@selection <- "MFI"
  
  ltm_cat@answers[1:10] <- c(rep(-1, 5), 1, 1, 0, 0, 1)
  grm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  gpcm_cat@answers[1:5] <- c(-1, -1, 5, 4, 3)
  
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)
  
  expect_equal(nrow(ltm_next$estimates) + sum(!is.na(ltm_cat@answers)),
               length(ltm_cat@answers))
  expect_equal(nrow(grm_next$estimates) + sum(!is.na(grm_cat@answers)),
               length(grm_cat@answers))
  expect_equal(nrow(gpcm_next$estimates) + sum(!is.na(gpcm_cat@answers)),
               length(gpcm_cat@answers))
})
