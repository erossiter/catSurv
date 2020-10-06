context("estimateThetas")
load("cat_objects.Rdata")
data("nfc")
data("npi")
data("polknowMT")
data("polknowTAPS")

test_that("EAP estimation calculates correctly", {
  ltm_cat@estimation <- tpm_cat@estimation <- grm_cat@estimation <- gpcm_cat@estimation <- "EAP"

  indv_ltm <- indv_tpm <- indv_grm <- indv_gpcm <- rep(NA, 10)
  for(i in 1:10){
    ltm_cat@answers <- unlist(npi[i, ])
    tpm_cat@answers <- unlist(polknowMT[i,1:20])
    grm_cat@answers <- unlist(nfc[i, ])
    gpcm_cat@answers <- unlist(polknowTAPS[i, ])

    indv_ltm[i] <- estimateTheta(ltm_cat)
    indv_tpm[i] <- estimateTheta(tpm_cat)
    indv_grm[i] <- estimateTheta(grm_cat)
    indv_gpcm[i] <- estimateTheta(gpcm_cat)
  }

  expect_equal(estimateThetas(ltm_cat, npi[1:10, ]), indv_ltm)
  expect_equal(estimateThetas(tpm_cat, polknowMT[1:10,1:20]), indv_tpm)
  expect_equal(estimateThetas(grm_cat, nfc[1:10, ]), indv_grm)
  expect_equal(estimateThetas(gpcm_cat, polknowTAPS[1:10, ]), indv_gpcm)
})

test_that("MAP estimation calculates correctly", {
  ltm_cat@estimation <- tpm_cat@estimation <- gpcm_cat@estimation <- "MAP"

  indv_ltm <- indv_tpm <- indv_gpcm <- rep(NA, 5)
  for(i in 1:5){
    ltm_cat@answers <- unlist(npi[i, ])
    tpm_cat@answers <- unlist(polknowMT[i,1:20])
    gpcm_cat@answers <- unlist(polknowTAPS[i, ])

    indv_ltm[i] <- estimateTheta(ltm_cat)
    indv_tpm[i] <- estimateTheta(tpm_cat)
    indv_gpcm[i] <- estimateTheta(gpcm_cat)
  }

  expect_equal(estimateThetas(ltm_cat, npi[1:5, ]), indv_ltm)
  expect_equal(estimateThetas(tpm_cat, polknowMT[1:5,1:20]), indv_tpm)
  expect_equal(estimateThetas(gpcm_cat, polknowTAPS[1:5, ]), indv_gpcm)
})

test_that("MLE estimation calculates correctly", {
  ltm_cat@estimation <- tpm_cat@estimation <- grm_cat@estimation <- gpcm_cat@estimation <- "MLE"

  indv_ltm <- indv_tpm <- indv_grm <- indv_gpcm <- rep(NA, 2)
  for(i in c(1,2)){
    ltm_cat@answers <- unlist(npi[i, ])
    tpm_cat@answers <- unlist(polknowMT[i,1:20])
    grm_cat@answers <- unlist(nfc[i, ])
    gpcm_cat@answers <- unlist(polknowTAPS[i, ])

    indv_ltm[i] <- estimateTheta(ltm_cat)
    indv_tpm[i] <- estimateTheta(tpm_cat)
    indv_grm[i] <- estimateTheta(grm_cat)
    indv_gpcm[i] <- estimateTheta(gpcm_cat)
  }

  expect_equal(estimateThetas(ltm_cat, npi[1:2, ]), indv_ltm)
  expect_equal(estimateThetas(tpm_cat, polknowMT[1:2,1:20]), indv_tpm)
  expect_equal(estimateThetas(grm_cat, nfc[1:2, ]), indv_grm)
  expect_equal(estimateThetas(gpcm_cat, polknowTAPS[1:2, ]), indv_gpcm)
})

test_that("WLE estimation calculates correctly", {
  ltm_cat@estimation <- tpm_cat@estimation <- grm_cat@estimation <- gpcm_cat@estimation <- "WLE"

  indv_ltm <- indv_tpm <- indv_grm <- indv_gpcm <- rep(NA, 10)
  for(i in 1:10){
    ltm_cat@answers <- unlist(npi[i, ])
    tpm_cat@answers <- unlist(polknowMT[i,1:20])
    grm_cat@answers <- unlist(nfc[i, ])
    gpcm_cat@answers <- unlist(polknowTAPS[i, ])

    indv_ltm[i] <- estimateTheta(ltm_cat)
    indv_tpm[i] <- estimateTheta(tpm_cat)
    indv_grm[i] <- estimateTheta(grm_cat)
    indv_gpcm[i] <- estimateTheta(gpcm_cat)
  }

  expect_equal(estimateThetas(ltm_cat, npi[1:10, ]), indv_ltm)
  expect_equal(estimateThetas(tpm_cat, polknowMT[1:10,1:20]), indv_tpm)
  expect_equal(estimateThetas(grm_cat, nfc[1:10, ]), indv_grm)
  expect_equal(estimateThetas(gpcm_cat, polknowTAPS[1:10, ]), indv_gpcm)
})
