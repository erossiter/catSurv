library(catR, quietly = TRUE)
context("nextItem-EPV")
load("cat_objects.Rdata")

test_that("ltm nextItem EPV calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@selection <- "EPV"
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "EPV"]

  ## EPV next item needs to be "EAP" estimation for Cat objects
  ## because catR package assumes this for theta estimates in EPV function
  catR_next <- nextItem(itemBank = it_ltm, theta = estimateTheta(ltm_cat),
                       x = ltm_cat@answers[1:5], criterion = "MEPV",
                       method = "EAP", out = 1:5)
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, expectedPV(ltm_cat, package_item))
  expect_equal(round(package_est, 5), round(catR_est, 5))
})

test_that("grm nextItem EPV calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@selection <- "EPV"
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "EPV"]

  catR_next <- nextItem(itemBank = it_grm, theta = estimateTheta(grm_cat),
                       x = grm_cat@answers[1:5]-1, criterion = "MEPV",
                       model = "GRM", out = 1:5)
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, expectedPV(grm_cat, package_item))
  expect_equal(round(package_est, 3), round(catR_est, 3))
})

test_that("gpcm nextItem EPV calculates correctly", {
  gpcm_cat@estimation <- "EAP"
  gpcm_cat@selection <- "EPV"
  gpcm_cat@answers[1:8] <- c(4, 5, 2, 4, 4, 1, 1, 3)

  package_next <- selectItem(gpcm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "EPV"]

  catR_next <- nextItem(itemBank = it_gpcm, theta = estimateTheta(gpcm_cat),
                       x = gpcm_cat@answers[1:8]-1, criterion = "MEPV",
                       model = "GPCM", out = 1:8)
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, expectedPV(gpcm_cat, package_item))
  expect_equal(round(package_est, 7), round(catR_est, 7))
})


test_that("nextItem EPV chooses item (not NA) when no questions asked", {
  ltm_cat@selection <- "EPV"
  grm_cat@selection <- "EPV"
  gpcm_cat@selection <- "EPV"

  expect_true(!is.na(selectItem(ltm_cat)$next_item))
  expect_true(!is.na(selectItem(grm_cat)$next_item))
  expect_true(!is.na(selectItem(gpcm_cat)$next_item))
})

test_that("nextItem EPV estimates are not NA (when no questions asked)", {
  ltm_cat@selection <- "EPV"
  grm_cat@selection <- "EPV"
  gpcm_cat@selection <- "EPV"

  expect_equal(sum(!is.na(selectItem(ltm_cat)$estimates[,"EPV"])), 40)
  expect_equal(sum(!is.na(selectItem(grm_cat)$estimates[,"EPV"])), 18)
  expect_equal(sum(!is.na(selectItem(gpcm_cat)$estimates[,"EPV"])), 10)
})

test_that("nextItem EPV is actually the minimum estimate", {
  ltm_cat@selection <- "EPV"
  grm_cat@selection <- "EPV"
  gpcm_cat@selection <- "EPV"
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)
  
  expect_equal(ltm_next$next_item, which(ltm_next$estimates[, "EPV"] ==
                                        min(ltm_next$estimates[, "EPV"])))
  expect_equal(grm_next$next_item, which(grm_next$estimates[, "EPV"] ==
                                        min(grm_next$estimates[, "EPV"])))
  expect_equal(gpcm_next$next_item, which(gpcm_next$estimates[, "EPV"] ==
                                        min(gpcm_next$estimates[, "EPV"])))
})

test_that("nextItem EPV correctly skips questions", {
  ltm_cat@selection <- "EPV"
  grm_cat@selection <- "EPV"
  gpcm_cat@selection <- "EPV"
  
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
