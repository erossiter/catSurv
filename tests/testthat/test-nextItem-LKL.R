context("nextItem-LKL")
load("cat_objects.Rdata")

test_that("ltm nextItem LKL calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@selection <- "LKL"
  ltm_cat@answers[c(1:7,27,36)] <- c(0, 1, 0, 0, 1, 0, 0, 1, 1)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number 
                                        == package_item, "LKL"]
  expect_equal(package_item, 40)
  expect_equal(round(package_est, 6), 0.000814)

})

test_that("grm nextItem LKL calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@selection <- "LKL"
  grm_cat@answers[c(1:8,14,17)] <- c(3, 4, 2, 2, 1, 2, 2, 3, 4, 4)

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number
                                        == package_item, "LKL"]

  expect_equal(package_item, 10)
  expect_equal(round(package_est, 8), 1.2e-07)
})

test_that("nextItem LKL chooses item (not NA) when no questions asked", {
  ltm_cat@selection <- "LKL"
  grm_cat@selection <- "LKL"
  gpcm_cat@selection <- "LKL"

  expect_true(!is.na(selectItem(ltm_cat)$next_item))
  expect_true(!is.na(selectItem(grm_cat)$next_item))
  expect_true(!is.na(selectItem(gpcm_cat)$next_item))
})

test_that("nextItem LKL estimates are not NA (when no questions asked)", {
  ltm_cat@selection <- "LKL"
  grm_cat@selection <- "LKL"
  gpcm_cat@selection <- "LKL"

  expect_equal(sum(!is.na(selectItem(ltm_cat)$estimates[,"LKL"])), 40)
  expect_equal(sum(!is.na(selectItem(grm_cat)$estimates[,"LKL"])), 18)
  expect_equal(sum(!is.na(selectItem(gpcm_cat)$estimates[,"LKL"])), 10)
})

test_that("nextItem LKL is actually the maximum estimate", {
  ltm_cat@selection <- "LKL"
  grm_cat@selection <- "LKL"
  gpcm_cat@selection <- "LKL"
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)

  expect_equal(ltm_next$next_item, which(ltm_next$estimates[, "LKL"] ==
                                        max(ltm_next$estimates[, "LKL"])))
  expect_equal(grm_next$next_item, which(grm_next$estimates[, "LKL"] ==
                                        max(grm_next$estimates[, "LKL"])))
  expect_equal(gpcm_next$next_item, which(gpcm_next$estimates[, "LKL"] ==
                                        max(gpcm_next$estimates[, "LKL"])))
})

test_that("nextItem LKL correctly skips questions", {
  ltm_cat@selection <- "LKL"
  grm_cat@selection <- "LKL"
  gpcm_cat@selection <- "LKL"
  
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