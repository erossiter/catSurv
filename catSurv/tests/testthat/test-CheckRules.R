context("checkStopRules")
load("cat_objects.Rdata")

checkStopRules_test <- function(cat){
  stop <- FALSE

  n_answered <- sum(!is.na(cat@answers))
  unanswered <- which(is.na(cat@answers))
  se_est <- estimateSE(cat)
  theta_est <- estimateTheta(cat)
  fish_inf <- sapply(unanswered, function(x) fisherInf(cat, theta_est, x))
  gain <- sapply(unanswered, function(x) abs(se_est - sqrt(expectedPV(cat, x))))

  ## lengthThreshold
  if(!is.na(cat@lengthThreshold)){
    if(n_answered >= cat@lengthThreshold) stop <- TRUE
  }

  ## seThreshold
  if(!is.na(cat@seThreshold)){
    if(se_est < cat@seThreshold) stop <- TRUE
  }

  ## infoThreshold
  if(!is.na(cat@infoThreshold)){
    if(all(fish_inf < cat@infoThreshold)) stop <- TRUE
  }

  ## gainThreshold
  if(!is.na(cat@gainThreshold)){
    if(all(gain < cat@gainThreshold)) stop <- TRUE
  }

  ## lengthOverride
  if(!is.na(cat@lengthOverride)){
    if(n_answered < cat@lengthOverride) stop <- FALSE
  }

  ## gainOverride
  if(!is.na(cat@gainOverride)){
    if(all(gain >= cat@gainOverride)) stop <- FALSE
  }

  return (stop)
}



test_that("lengthThreshold works", {
  ltm_cat@lengthThreshold <- 5
  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))

  ltm_cat@answers[1:5] <- c(0, 1, 1, 0, 1)
  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))
})

test_that("seThreshold works", {
  ltm_cat@seThreshold <- .6
  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))

  ltm_cat@answers[1:10] <- c(0, 1, 1, 0, 1, 1, 1, 1, 0, 0)
  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))
  expect_lt(estimateSE(ltm_cat), ltm_cat@seThreshold)
})

test_that("gainThreshold works", {
  ltm_cat@gainThreshold <- .1
  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))

  ltm_cat@answers[1:10] <- c(0, 1, 1, 0, 1, 1, 1, 1, 0, 0)
  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))
})

test_that("lengthOverride works", {
  ltm_cat@lengthThreshold <- 5
  ltm_cat@lengthOverride <- 10
  ltm_cat@answers[1:7] <- c(0, 1, 1, 0, 1, 1, 0)

  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))
})

test_that("gainOverride works", {
  ltm_cat@answers[1:10] <- c(0, 0, 1, 0, 0, 0, 1, 1, 1, 1)
  ltm_cat@lengthThreshold <- 5 ## can stop if answered 5 questions
  ltm_cat@gainOverride <- .001 ## but cannot stop unless all gains are less than .001

  expect_equal(checkStopRules(ltm_cat), checkStopRules_test(ltm_cat))
})

