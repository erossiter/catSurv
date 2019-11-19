context("Cat-class")
load("cat_objects.Rdata")

test_that("'set' methods work", {
    setEstimation(ltm_cat) <- "WLE"
    expect_equal(ltm_cat@estimation, "WLE")
    
    setDiscrimination(gpcm_cat) <- rep(1, 10)
    expect_equal(gpcm_cat@discrimination, rep(1, 10))
})

test_that("validity tests work", {
    expect_error(storeAnswer(ltm_cat, 1, 4))
    expect_error(storeAnswer(grm_cat, 1, 7))
    expect_error(setEstimation(ltm_cat) <- "eap")
    expect_error(setSelection(gpcm_cat) <- "mfi")
    expect_error(setDiscrimination(gpcm_cat) <- rep(1, 3))
    expect_error(setGuessing(ltm_cat) <- c(.1, .1))
    expect_error(setAnswers(tpm_cat) <- c(1,0,1,0,1,1))
    expect_error(setUpperBound(grm_cat) <- -6)
})
