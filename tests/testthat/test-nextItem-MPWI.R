library(catR, quietly = TRUE)
context("nextItem-MPWI")
load("cat_objects.Rdata")

test_that("ltm nextItem MPWI calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@selection <- "MPWI"
  ltm_cat@answers[1:20] <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0,
                             0, 1, 1, 1, 0, 0, 0, 1, 1, 1)

  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MPWI"]

  catR_next <- nextItem(itemBank = it_ltm, theta = estimateTheta(ltm_cat),
                        x = ltm_cat@answers[1:20], criterion = "MPWI", out = 1:20,
                        method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, catR_est)
})

test_that("grm nextItem MPWI calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@selection <- "MPWI"
  grm_cat@answers[1:10] <- c(2, 3, 1, 1, 5, 4, 3, 2, 1, 5)

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MPWI"]

  catR_next <- nextItem(itemBank = it_grm, theta = estimateTheta(grm_cat),
                       x = grm_cat@answers[1:10]-1, criterion = "MPWI",
                       model = "GRM", out = 1:10, method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, catR_est)
})

test_that("gpcm nextItem MPWI calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@selection <- "MPWI"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)

  package_next <- selectItem(gpcm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MPWI"]

  catR_next <- nextItem(itemBank = it_gpcm, theta = estimateTheta(gpcm_cat),
                       x = gpcm_cat@answers[1:5]-1, criterion = "MPWI",
                       model = "GPCM", out = 1:5, method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, catR_est)
})


test_that("nextItem MPWI chooses item (not NA) when no questions asked", {
  ltm_cat@selection <- "MPWI"
  grm_cat@selection <- "MPWI"
  gpcm_cat@selection <- "MPWI"

  expect_true(!is.na(selectItem(ltm_cat)$next_item))
  expect_true(!is.na(selectItem(grm_cat)$next_item))
  expect_true(!is.na(selectItem(gpcm_cat)$next_item))
})

test_that("nextItem MPWI estimates are not NA (when no questions asked)", {
  ltm_cat@selection <- "MPWI"
  grm_cat@selection <- "MPWI"
  gpcm_cat@selection <- "MPWI"

  expect_equal(sum(!is.na(selectItem(ltm_cat)$estimates[,"MPWI"])), 40)
  expect_equal(sum(!is.na(selectItem(grm_cat)$estimates[,"MPWI"])), 18)
  expect_equal(sum(!is.na(selectItem(gpcm_cat)$estimates[,"MPWI"])), 10)
})

test_that("nextItem MPWI is actually the maximum estimate", {
  ltm_cat@selection <- "MPWI"
  grm_cat@selection <- "MPWI"
  gpcm_cat@selection <- "MPWI"
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)

  expect_equal(ltm_next$next_item, which(ltm_next$estimates[, "MPWI"] ==
                                        max(ltm_next$estimates[, "MPWI"])))
  expect_equal(grm_next$next_item, which(grm_next$estimates[, "MPWI"] ==
                                        max(grm_next$estimates[, "MPWI"])))
  expect_equal(gpcm_next$next_item, which(gpcm_next$estimates[, "MPWI"] ==
                                        max(gpcm_next$estimates[, "MPWI"])))
})

test_that("nextItem MPWI correctly skips questions", {
  ltm_cat@selection <- "MPWI"
  grm_cat@selection <- "MPWI"
  gpcm_cat@selection <- "MPWI"
  
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

