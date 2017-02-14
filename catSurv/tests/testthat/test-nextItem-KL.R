detach("package:catR", unload = TRUE)
library(catIrt)
context("nextItem-KL")
load("cat_objects.Rdata")

test_that("ltm nextItem KL calculates correctly", {
  ltm_cat@estimation <- "EAP"
  ltm_cat@selection <- "KL"
  ltm_cat@answers[1:7] <- c(0, 1, 0, 0, 1, 0, 0)
  
  package_next <- selectItem(ltm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "KL"]

  delta <- ltm_cat@z * sqrt(fisherTestInfo(ltm_cat))
  catIrt_next <- itChoose(cbind(8:40, it_ltm[8:40,1:3]),
                        mod = "brm",
                        numb = 1,
                        n.select = 1,
                        cat_par = it_ltm[1:7, 1:3],
                        cat_resp = ltm_cat@answers[1:7],
                        cat_theta = estimateTheta(ltm_cat),
                        select = "FI-KL",
                        delta = delta,
                        at = "theta")
  catIrt_item <- as.numeric(catIrt_next$params[1,1])
  catIrt_est <- catIrt_next$info

  expect_equal(package_item, catIrt_item)
  expect_equal(round(package_est, 3), round(catIrt_est, 3))
})

test_that("grm nextItem KL calculates correctly", {
  grm_cat@estimation <- "EAP"
  grm_cat@selection <- "KL"
  grm_cat@answers[1:8] <- c(5, 4, 2, 2, 1, 2, 2, 3)
  
  package_next <- selectItem(grm_cat)
  package_item <- package_next$next_item
  package_est <- package_next$estimates[package_next$estimates$q_number == package_item,
                                        "KL"]

  delta <- grm_cat@z * sqrt(fisherTestInfo(grm_cat))
  catIrt_next <- itChoose(cbind(9:18, it_grm[9:18,]),
                        mod = "grm",
                        numb = 1,
                        n.select = 1,
                        cat_par = it_grm[1:8, ],
                        cat_resp = grm_cat@answers[1:8],
                        cat_theta = estimateTheta(grm_cat),
                        select = "FI-KL",
                        delta = delta,
                        at = "theta")
  catIrt_item <- as.numeric(catIrt_next$params[1,1])
  catIrt_est <- catIrt_next$info

  expect_equal(package_item, catIrt_item)
  expect_equal(round(package_est, 2), round(catIrt_est, 2))
})

test_that("nextItem KL throws error when no questions asked", {
  ltm_cat@selection <- "KL"
  grm_cat@selection <- "KL"
  gpcm_cat@selection <- "KL"

  expect_error(selectItem(ltm_cat))
  expect_error(selectItem(grm_cat))
  expect_error(selectItem(gpcm_cat))
})

test_that("nextItem KL is actually the maximum estimate", {
  ltm_cat@selection <- "KL"
  ltm_cat@answers[1:5] <- c(1, 0, 1, 1, 1)
  grm_cat@selection <- "KL"
  grm_cat@answers[1:5] <- c(5, 4, 2, 2, 5)
  gpcm_cat@selection <- "KL"
  gpcm_cat@answers[1:5] <- c(5, 4, 2, 2, 5)
  ltm_next <- selectItem(ltm_cat)
  grm_next <- selectItem(grm_cat)
  gpcm_next <- selectItem(gpcm_cat)

  expect_equal(ltm_next$next_item, ltm_next$estimates[which(ltm_next$estimates[, "KL"] ==
                                        max(ltm_next$estimates[, "KL"])), "q_number"])
  expect_equal(grm_next$next_item, grm_next$estimates[which(grm_next$estimates[, "KL"] ==
                                        max(grm_next$estimates[, "KL"])), "q_number"])
  expect_equal(gpcm_next$next_item, gpcm_next$estimates[which(gpcm_next$estimates[, "KL"] ==
                                        max(gpcm_next$estimates[, "KL"])), "q_number"])
})


detach("package:catIrt", unload = TRUE)
