context("lookAhead")
load("cat_objects.Rdata")

test_that("lookAhead works for ltm Cat", {
  look <- lookAhead(ltm_cat, 1)$estimates

  ltm_cat@answers[1] <- 0
  expect_equal(selectItem(ltm_cat)$next_item, look[1, "next_item"])
  
  ltm_cat@answers[1] <- 1
  expect_equal(selectItem(ltm_cat)$next_item, look[2, "next_item"])
})

test_that("lookAhead works for grm Cat", {
  look <- lookAhead(grm_cat, 1)$estimates

  grm_cat@answers[1] <- 1
  expect_equal(selectItem(grm_cat)$next_item, look[1, "next_item"])
  
  grm_cat@answers[1] <- 2
  expect_equal(selectItem(grm_cat)$next_item, look[2, "next_item"])
  
  grm_cat@answers[1] <- 3
  expect_equal(selectItem(grm_cat)$next_item, look[3, "next_item"])
  
  grm_cat@answers[1] <- 4
  expect_equal(selectItem(grm_cat)$next_item, look[4, "next_item"])

  grm_cat@answers[1] <- 5
  expect_equal(selectItem(grm_cat)$next_item, look[5, "next_item"])
})

test_that("lookAhead works for gpcm Cat", {
  look <- lookAhead(gpcm_cat, 1)$estimates

  gpcm_cat@answers[1] <- 1
  expect_equal(selectItem(gpcm_cat)$next_item, look[1, "next_item"])
  
  gpcm_cat@answers[1] <- 2
  expect_equal(selectItem(gpcm_cat)$next_item, look[2, "next_item"])
  
  gpcm_cat@answers[1] <- 3
  expect_equal(selectItem(gpcm_cat)$next_item, look[3, "next_item"])
  
  gpcm_cat@answers[1] <- 4
  expect_equal(selectItem(gpcm_cat)$next_item, look[4, "next_item"])

  gpcm_cat@answers[1] <- 5
  expect_equal(selectItem(gpcm_cat)$next_item, look[5, "next_item"])
})

test_that("lookAhead throws error when question has been answered", {
  ltm_cat@answers[1] <- 1 
  grm_cat@answers[1] <- 1
  gpcm_cat@answers[1] <- 1
  
  expect_error(lookAhead(ltm_cat, 1))
  expect_error(lookAhead(grm_cat, 1))
  expect_error(lookAhead(gpcm_cat, 1))
})
