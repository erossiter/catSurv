context("ltmCat, tpmCat, grmCat, gpcmCat")
load("cat_objects.Rdata")

test_that("ltmCat populates slots correctly", {
  expect_equal(ltm_cat@model, "ltm")
  expect_equal(sum(ltm_cat@guessing != 0), 0)
  expect_equal(sum(is.na(ltm_cat@difficulty)), 0)
  expect_equal(sum(is.na(ltm_cat@discrimination)), 0)
})

test_that("grmCat populates slots correctly", {
  expect_equal(grm_cat@model, "grm")
  expect_equal(sum(grm_cat@guessing != 0), 0)
  expect_equal(sum(unlist(lapply(grm_cat@difficulty, is.na))), 0)
  expect_equal(sum(is.na(grm_cat@discrimination)), 0)
})

test_that("gpcmCat populates slots correctly", {
  expect_equal(gpcm_cat@model, "gpcm")
  expect_equal(sum(gpcm_cat@guessing != 0), 0)
  expect_equal(sum(unlist(lapply(gpcm_cat@difficulty, is.na))), 0)
  expect_equal(sum(is.na(gpcm_cat@discrimination)), 0)
})
