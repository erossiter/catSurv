context("lookAhead")
load("cat_objects.Rdata")

test_that("lookAhead works for ltm Cat", {
  look <- lookAhead(ltm_cat, 1)
  
  ltm_cat@answers[1] <- -1
  expect_equal(selectItem(ltm_cat)$next_item, look$next_item[look$response_option == -1])

  ltm_cat@answers[1] <- 0
  expect_equal(selectItem(ltm_cat)$next_item, look$next_item[look$response_option == 0])
  
  ltm_cat@answers[1] <- 1
  expect_equal(selectItem(ltm_cat)$next_item, look$next_item[look$response_option == 1])
})

test_that("lookAhead works for grm Cat", {
  look <- lookAhead(grm_cat, 1)

  grm_cat@answers[1] <- -1
  expect_equal(selectItem(grm_cat)$next_item, look$next_item[look$response_option == -1])
  
  grm_cat@answers[1] <- 1
  expect_equal(selectItem(grm_cat)$next_item, look$next_item[look$response_option == 1])
  
  grm_cat@answers[1] <- 2
  expect_equal(selectItem(grm_cat)$next_item, look$next_item[look$response_option == 2])
  
  grm_cat@answers[1] <- 3
  expect_equal(selectItem(grm_cat)$next_item, look$next_item[look$response_option == 3])
  
  grm_cat@answers[1] <- 4
  expect_equal(selectItem(grm_cat)$next_item, look$next_item[look$response_option == 4])
  
  grm_cat@answers[1] <- 5
  expect_equal(selectItem(grm_cat)$next_item, look$next_item[look$response_option == 5])
})

test_that("lookAhead works for gpcm Cat", {
  look <- lookAhead(gpcm_cat, 1)

  gpcm_cat@answers[1] <- -1
  expect_equal(selectItem(gpcm_cat)$next_item, look$next_item[look$response_option == -1])
  
  gpcm_cat@answers[1] <- 1
  expect_equal(selectItem(gpcm_cat)$next_item, look$next_item[look$response_option == 1])
  
  gpcm_cat@answers[1] <- 2
  expect_equal(selectItem(gpcm_cat)$next_item, look$next_item[look$response_option == 2])
  
  gpcm_cat@answers[1] <- 3
  expect_equal(selectItem(gpcm_cat)$next_item, look$next_item[look$response_option == 3])
  
  gpcm_cat@answers[1] <- 4
  expect_equal(selectItem(gpcm_cat)$next_item, look$next_item[look$response_option == 4])
  
  gpcm_cat@answers[1] <- 5
  expect_equal(selectItem(gpcm_cat)$next_item, look$next_item[look$response_option == 5])
})

test_that("lookAhead returns empty df when question has been answered", {
  ltm_cat@answers[1] <- 1 
  look <- lookAhead(ltm_cat, 1)
  
  expect_equal(as.character(look[1,1]), "NULL")
  expect_equal(dim(look), c(3,2))
})

test_that("lookAhead returns empty df when last question", {
    ltm_cat@answers[1:39] <- sample(c(-1,0,1), 39, replace = TRUE)
    look <- lookAhead(ltm_cat, 40)
    
    expect_equal(as.character(look[1,1]), "NULL")
    expect_equal(dim(look), c(3,2))
})
