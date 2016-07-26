library(catSurv)
library(testthat)
library(ltm)
context("makeTree")

test_that("makeTree calculates/formats correctly", {
  
  data("npi")
  data("nfc")
  data("AMTknowledge")
  ltm_data <- npi[1:100, ]
  tpm_data <- AMTknowledge[1:100, ]
  poly_data <- nfc[1:100, ]
  
  ###------------- Only use if introducing extra NAs --------------------
  # bi.options <- c(NA, 0, 1)
  # for(a in 1:nrow(ltm_data)){
  #   ltm_data[a,] <- sample(bi.options,ncol(ltm_data), replace=TRUE)
  # }
  # for(a in 1:nrow(tpm_data)){
  #   tpm_data[a,] <- sample(bi.options,ncol(tpm_data), replace=TRUE)
  # }
  # poly.options <- c(NA, 1:5)
  # for(a in 1:nrow(poly_data)){
  #   poly_data[a,] <- sample(poly.options,ncol(poly_data), replace=TRUE)
  # }
  ###--------------------------------------------------------------------
  
  # binary(ltm)
  binary_cat.ltm <- ltmCat(ltm_data, 100)
  binary_cat.ltm@lengthThreshold <- 5
  
  ltm.list <- makeTree(binary_cat.ltm)
  ltm.flat <- makeTree(binary_cat.ltm, flat = TRUE)
  
  samp.resp <- matrix(NA, ncol=2, nrow = 6)
  for(i in 1:6){
    samp.resp[i,1] <- paste0("Q",selectItem(binary_cat.ltm)$next.item)
    samp.resp[i,2] <- sample(c(0,1), 1, T)
    binary_cat.ltm@answers[selectItem(binary_cat.ltm)$next.item] <- as.numeric(samp.resp[i,2])
  }
  
  list.Q<- ltm.list[[samp.resp[1,2]]][[samp.resp[2,2]]][[samp.resp[3,2]]][[samp.resp[4,2]]][[samp.resp[5,2]]]$Next
  
  expect_equal(list.Q, samp.resp[6,1], 0.001)
  
  flat.Q <- ltm.flat[which(ltm.flat[,samp.resp[1,1]] == as.numeric(samp.resp[1,2]) &
                   ltm.flat[,samp.resp[2,1]] == as.numeric(samp.resp[2,2]) &
                   ltm.flat[,samp.resp[3,1]] == as.numeric(samp.resp[3,2]) &
                   ltm.flat[,samp.resp[4,1]] == as.numeric(samp.resp[4,2]) &
                   ltm.flat[,samp.resp[5,1]] == as.numeric(samp.resp[5,2])), "NextItem"]
  
  expect_equal(flat.Q, samp.resp[6,1], 0.001)
  
  
  
  # binary(tpm)
  binary_cat.tpm <- tpmCat(tpm_data, 100)
  binary_cat.tpm@lengthThreshold <- 5

  tpm.list <- makeTree(binary_cat.tpm)
  tpm.flat <- makeTree(binary_cat.tpm, flat = TRUE)
  
  samp.resp <- matrix(NA, ncol=2, nrow = 6)
  for(i in 1:6){
    samp.resp[i,1] <- paste0("Q",selectItem(binary_cat.tpm)$next.item)
    samp.resp[i,2] <- sample(c(0,1), 1, T)
    binary_cat.tpm@answers[selectItem(binary_cat.tpm)$next.item] <- as.numeric(samp.resp[i,2])
  }
  
  list.Q<- tpm.list[[samp.resp[1,2]]][[samp.resp[2,2]]][[samp.resp[3,2]]][[samp.resp[4,2]]][[samp.resp[5,2]]]$Next
  
  expect_equal(list.Q, samp.resp[6,1], 0.001)
  
  flat.Q <- tpm.flat[which(tpm.flat[,samp.resp[1,1]] == as.numeric(samp.resp[1,2]) &
                             tpm.flat[,samp.resp[2,1]] == as.numeric(samp.resp[2,2]) &
                             tpm.flat[,samp.resp[3,1]] == as.numeric(samp.resp[3,2]) &
                             tpm.flat[,samp.resp[4,1]] == as.numeric(samp.resp[4,2]) &
                             tpm.flat[,samp.resp[5,1]] == as.numeric(samp.resp[5,2])), "NextItem"]
  
  expect_equal(flat.Q, samp.resp[6,1], 0.001)

  # categorical(grm)
  poly_cat <- grmCat(poly_data, 100)
  poly_cat@lengthThreshold <- 5
  poly_cat@answers <- rep(NA, length(poly_cat@answers))

  poly.list <- makeTree(poly_cat)
  poly.flat <- makeTree(poly_cat, flat = TRUE)
  
  samp.resp <- matrix(NA, ncol=2, nrow = 6)
  for(i in 1:6){
    samp.resp[i,1] <- names(poly_cat@discrimination)[selectItem(poly_cat)$next.item]
    samp.resp[i,2] <- sample(c(1:5), 1, T)
    poly_cat@answers[selectItem(poly_cat)$next.item] <- as.numeric(samp.resp[i,2])
  }
  
  
  list.Q<- poly.list[[samp.resp[1,2]]][[samp.resp[2,2]]][[samp.resp[3,2]]][[samp.resp[4,2]]][[samp.resp[5,2]]]$Next
  
  expect_equal(list.Q, samp.resp[6,1], 0.001)
  
  flat.Q <- poly.flat[which(poly.flat[,samp.resp[1,1]] == as.numeric(samp.resp[1,2]) &
                              poly.flat[,samp.resp[2,1]] == as.numeric(samp.resp[2,2]) &
                              poly.flat[,samp.resp[3,1]] == as.numeric(samp.resp[3,2]) &
                              poly.flat[,samp.resp[4,1]] == as.numeric(samp.resp[4,2]) &
                              poly.flat[,samp.resp[5,1]] == as.numeric(samp.resp[5,2])), "NextItem"]
  
  expect_equal(flat.Q, samp.resp[6,1], 0.001)
  
})