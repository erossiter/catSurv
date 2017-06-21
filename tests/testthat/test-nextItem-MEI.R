library(catR, quietly = TRUE)
context("nextItem-MEI")
load("cat_objects.Rdata")

test_that("ltm nextItem MEI calculates correctly", {
  ltm_cat@estimation <- "MAP"
  ltm_cat@selection <- "MEI"
  ltm_cat@answers[c(21:40)] <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0,
                             1, 0, 1, 1, 0, 0, 1, 1, 1, 0)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MEI"]
  
  catR_next <- nextItem(itemBank = it_ltm, theta = estimateTheta(ltm_cat),
                        x = ltm_cat@answers[c(21:40)], criterion = "MEI",
                        out = c(21:40), infoType = "observed", method = "BM")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, expectedObsInf(ltm_cat, package_item))
  expect_equal(round(package_est, 5), round(catR_est, 5))
})

test_that("grm nextItem MEI calculates correctly", {
  grm_cat@estimation <- "MAP"
  grm_cat@selection <- "MEI"
  grm_cat@answers[1:3] <- c(4, 5, 2)

  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MEI"]

  catR_next <- nextItem(itemBank = it_grm, theta = estimateTheta(grm_cat),
                       x = grm_cat@answers[1:3]-1, criterion = "MEI", method = "BM",
                       model = "GRM", out = 1:3, infoType = "observed")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, expectedObsInf(grm_cat, package_item))
  expect_equal(round(package_est, 3), round(catR_est, 3))
})

test_that("gpcm nextItem MEI calculates correctly", {
  gpcm_cat@estimation <- "MAP"
  gpcm_cat@selection <- "MEI"
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)

  package_next <- selectItem(gpcm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "MEI"]

  catR_next <- nextItem(itemBank = it_gpcm, theta = estimateTheta(gpcm_cat),
                       x = gpcm_cat@answers[1:5]-1, criterion = "MEI", method = "BM",
                       model = "GPCM", out = 1:5, infoType = "observed")
  catR_item <- catR_next$item
  catR_est <- catR_next$info

  expect_equal(package_item, catR_item)
  expect_equal(package_est, expectedObsInf(gpcm_cat, package_item))
  expect_equal(round(package_est, 5), round(catR_est, 5))
})


test_that("nextItem MEI chooses item (not NA) when no questions asked", {
  ltm_cat@selection <- "MEI"
  grm_cat@selection <- "MEI"
  gpcm_cat@selection <- "MEI"

  expect_true(!is.na(selectItem(ltm_cat)$next_item))
  expect_true(!is.na(selectItem(grm_cat)$next_item))
  expect_true(!is.na(selectItem(gpcm_cat)$next_item))
})

test_that("nextItem MEI estimates are not NA (when no questions asked)", {
  ltm_cat@selection <- "MEI"
  grm_cat@selection <- "MEI"
  gpcm_cat@selection <- "MEI"

  expect_equal(sum(!is.na(selectItem(ltm_cat)$estimates[,"MEI"])), 40)
  expect_equal(sum(!is.na(selectItem(grm_cat)$estimates[,"MEI"])), 18)
  expect_equal(sum(!is.na(selectItem(gpcm_cat)$estimates[,"MEI"])), 10)
})

test_that("nextItem MEI is actually the maximum estimate", {
  ltm_cat@selection <- "MEI"
  grm_cat@selection <- "MEI"
  gpcm_cat@selection <- "MEI"
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)
  
  expect_equal(ltm_next$next_item, which(ltm_next$estimates[, "MEI"] ==
                                        max(ltm_next$estimates[, "MEI"])))
  expect_equal(grm_next$next_item, which(grm_next$estimates[, "MEI"] ==
                                        max(grm_next$estimates[, "MEI"])))
  expect_equal(gpcm_next$next_item, which(gpcm_next$estimates[, "MEI"] ==
                                        max(gpcm_next$estimates[, "MEI"])))
})

test_that("nextItem MEI correctly skips questions", {
  ltm_cat@selection <- "MEI"
  grm_cat@selection <- "MEI"
  gpcm_cat@selection <- "MEI"
  
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
