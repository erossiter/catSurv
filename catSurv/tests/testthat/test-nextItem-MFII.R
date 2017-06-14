context("nextItem-MFII")
load("cat_objects.Rdata")

test_that("ltm nextItem MFII calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@selection <- "MFII"
  ltm_cat@answers[1:7] <- c(0, 1, 0, 0, 1, 0, 0)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFII"]

  expect_equal(package_item, 27)
  expect_equal(round(package_est, 3), 1.818)
})

test_that("grm nextItem MFII calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@selection <- "MFII"
  grm_cat@answers[1:8] <- c(5, 4, 2, 2, 1, 2, 2, 3)
  
  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MFII"]

  expect_equal(package_item, 10)
  expect_equal(round(package_est, 3), 3.238)
})

test_that("nextItem MFII throws error when no questions asked", {
  ltm_cat@selection <- "MFII"
  grm_cat@selection <- "MFII"
  gpcm_cat@selection <- "MFII"

  expect_error(selectItem(ltm_cat))
  expect_error(selectItem(grm_cat))
  expect_error(selectItem(gpcm_cat))
})

test_that("nextItem MFII is actually the maximum estimate", {
  ltm_cat@selection <- "MFII"
  ltm_cat@answers[1:5] <- c(1, 0, 1, 1, 1)
  grm_cat@selection <- "MFII"
  grm_cat@answers[1:5] <- c(5, 4, 2, 2, 5)
  gpcm_cat@selection <- "MFII"
  gpcm_cat@answers[1:5] <- c(1, 1, 2, 2, 4)
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)

  expect_equal(ltm_next$next_item, ltm_next$estimates[which(ltm_next$estimates[, "MFII"] ==
                                        max(ltm_next$estimates[, "MFII"])), "q_number"])
  expect_equal(grm_next$next_item, grm_next$estimates[which(grm_next$estimates[, "MFII"] ==
                                        max(grm_next$estimates[, "MFII"])), "q_number"])
  expect_equal(gpcm_next$next_item, gpcm_next$estimates[which(gpcm_next$estimates[, "MFII"] ==
                                        max(gpcm_next$estimates[, "MFII"])), "q_number"])
})

test_that("nextItem MFII correctly skips questions", {
  ltm_cat@selection <- "MFII"
  grm_cat@selection <- "MFII"
  gpcm_cat@selection <- "MFII"
  
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
